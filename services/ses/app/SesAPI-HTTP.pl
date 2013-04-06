#!/usr/bin/perl

use strict;
use threads;
use HTTP::Daemon;
use Ses::Config;
use Ses::UserAPI;
use Ses::Db;

################ Configuration ####################

sub DEBUG { 1 }

my %HANDLERS = (
    '/identity/list'  => \&API_Identity_List,
    '/identity/add'   => \&API_Identity_Add,
    # ...
);

############ End of Configuration #################

my $d = HTTP::Daemon->new(LocalAddr => CFG_HTTP_LISTEN_ADDR, LocalPort => CFG_HTTP_LISTEN_PORT, Listen => 20) 
    or die "SES API Web Server failed to start\n";

printf "SES API Web Server started at %s:%d\n", $d->sockhost(), $d->sockport();

while (my $c = $d->accept) {
    threads->create(\&process_one_req, $c)->detach();
}

sub process_one_req {
    my $c = shift;
    my $r = $c->get_request;
    my $handler = $HANDLERS{$r->url->path};

    printf "HTTP Request: %s %s\n", $r->method, $r->url->path if DEBUG;

    if (ref($handler) eq "CODE") {
        Call_API($handler, $r, $c);
    } else {
        print "  -> 404 Not Found\n";
        $c->send_basic_header(404);
        $c->send_header("Connection", "close");
        $c->send_header("Content-type", "text/plain");
        $c->send_crlf;
        print $c "404 Not Found" if DEBUG;
    }

    $c->close;
    undef($c);
}

############################################################################3

sub Call_API {
    my ($handler, $r, $c) = @_;

    $c->send_basic_header(200);
    $c->send_header("Connection", "close");
    $c->send_header("Content-type", "application/json");
    $c->send_crlf;

    my ($uid, $err) = Auth($r);
    if (!defined $uid) {
        print "  -> UserAPI Auth error: $err\n" if DEBUG;
        print $c result_err(1, "UserAPI Auth error: $err");
        return;
    }

    my $db = new Ses::Db;
    my $user = $db->findUser(uid => $uid);
    $db->addUser($uid) unless defined $user->{id};
    $user = $db->findUser(uid => $uid);
    if (!defined $user->{id}) {
        $db->close();
        printf "  -> DB: Cannot find new user after creation (uid='%s')\n", $uid if DEBUG;
        print $c result_err(255, "Cannot find new user after creation");
        return;
    }
    printf "  -> UserAPI Auth OK (user.id='%d', user.uid='%s')\n", $user->{id}, $user->{uid} if DEBUG;
    print  "  -> Calling API function: $handler ...\n" if DEBUG;
    $handler->($c, $r, $db, $user);
    $db->close();
    print  "  -> API function finished\n" if DEBUG;
}

############################################################################3

sub result_ok {
    JSON::to_json({ status => "OK", result => shift });
}

sub result_err {
    my ($code,$str) = @_;
    JSON::to_json({ status => "FAIL", error => { code=>$code, str=>$str } });
}

###################################################################################

sub Auth {
    my $r = shift;
    $r->header('Cookie') =~ /session=(\S+)/;
    my $session = $1 or return (undef, "No session cookie");
    return CallUserAPI($session);
};

############################################################################3

sub API_Identity_List {
    my ($c,$r,$db,$user) = @_;
    print "  ** API_Identity_List\n" if DEBUG;
    print $c result_ok [$db->getAllIdentities($user)];
}

sub API_Identity_Add {
    my ($c,$r,$db,$user) = @_;
    print "  ** API_Identity_Add\n" if DEBUG;
    my $req;
    eval {
        $req = JSON::from_json($r->content);
    };
    if (!defined $req) {
        print $c result_err(254, "Bad JSON");
        return;
    }
    printf "     email: '%s'\n", $req->{email} if DEBUG;
    if (!defined($req->{email})) {
        print $c result_err(2, "Parameter 'email' not specified");
        return;
    }
    if ($req->{email} !~ /^[a-z0-9A-Z_\.-]+\@[a-z0-9A-Z_\.-]+\.[a-z]+$/) {
        print $c result_err(3, "Invalid 'email' value");
        return;
    }
    if ($db->addIdentity($user, $req->{email})) {
        print $c result_ok;
    }
    else {
        print $c result_err(4, "Failed");
    }
}

