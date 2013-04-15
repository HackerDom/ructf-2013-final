#!/usr/bin/perl

use strict;
use threads;
use Digest::SHA qw(sha1_hex);
use HTTP::Daemon;
use Ses::Message;
use Ses::UserAPI;
use Ses::Config;
use Ses::Utils;
use Ses::Db;
use Ses::I18N;

################ Configuration ####################

sub DEBUG { 1 }

my %HANDLERS = (
    '/identity/list'    => \&API_Identity_List,
    '/identity/add'     => \&API_Identity_Add,
    '/identity/del'     => \&API_Identity_Del,
    '/credentials/list' => \&API_Credentials_List,
    '/credentials/add'  => \&API_Credentials_Add,
    '/credentials/del'  => \&API_Credentials_Del,
    '/mail/send'        => \&API_Mail_Send,
    '/stats'            => \&API_Stats,
    '/error'            => \&API_Error,
);

############ End of Configuration #################

my $d = HTTP::Daemon->new(
        LocalPort => CFG_HTTP_PORT,
        Listen => 20
    ) or die "SES API Web Server failed to start: $!\n";

printf "SES API Server started at %s:%d\n", $d->sockhost(), $d->sockport();

$SIG{'INT'} = sub {
    print "<Ctrl+C> pressed\n";
    close $d;
    exit 0;
};

while (my $c = $d->accept) {
    threads->create(\&process_one_req, $c)->detach();
}

exit 0;

#############################################################################

sub process_one_req {
    my $c = shift;
    my $r = $c->get_request;
    printf "HTTP Request: %s %s\n", $r->method, $r->url->path if DEBUG;

    my $handler = $HANDLERS{$r->url->path};
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
    undef $c;
}

#############################################################################

sub Call_API {
    my ($handler, $r, $c) = @_;

    $c->send_basic_header(200);
    $c->send_header("Connection", "close");
    $c->send_header("Content-type", "application/json");
    $c->send_crlf;

    my ($uid, $lang, $err) = Auth($r);
    if (!defined $uid) {
        print "  -> UserAPI Auth error: $err\n" if DEBUG;
        print $c result_err(1);
        return;
    }
    my $db = new Ses::Db;
    my $user = $db->findUser(uid => $uid);
    $db->addUser($uid,$lang) unless defined $user->{id};
    $user = $db->findUser(uid => $uid);
    if (!defined $user->{id}) {
        $db->close();
        printf "  -> DB: Cannot find new user after creation (uid='%s')\n", $uid if DEBUG;
        print $c result_err(2);
        return;
    }
    printf "  -> UserAPI Auth OK (user.id='%d', user.uid='%s')\n", $user->{id}, $user->{uid} if DEBUG;
    print  "  -> Calling API function: $handler ...\n" if DEBUG;
    $handler->($c, $r, $db, $user);
    $db->close();
    print  "  -> API function finished\n" if DEBUG;
}

#############################################################################

sub result_ok {
    JSON::to_json({ status => "OK", result => shift });
}

sub result_err {
    my $code = shift;
    JSON::to_json({ status => "FAIL", error => $code });
}

#############################################################################

sub Auth {
    my $r = shift;
    $r->header('Cookie') =~ /session=(\S+)/;
    my $session = $1 or return (undef, "No session cookie");
    my $api = new Ses::UserAPI("http://127.0.0.1");
    return $api->user($session);
};

#############################################################################

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
        print $c result_err(3);
        return;
    }
    printf "     email: '%s'\n", $req->{email} if DEBUG;
    if (!defined($req->{email})) {
        print $c result_err(4);
        return;
    }
    if ($req->{email} !~ /^[a-z0-9A-Z_\.-]+\@[a-z0-9A-Z_\.-]+\.[a-z]+$/) {
        print $c result_err(5);
        return;
    }
    if ($db->addIdentity($user, $req->{email})) {
        print $c result_ok;
    }
    else {
        print $c result_err(6);
    }
}

sub API_Identity_Del {
    my ($c,$r,$db,$user) = @_;
    print "  ** API_Identity_Del\n" if DEBUG;
    my $req;
    eval {
        $req = JSON::from_json($r->content);
    };
    if (!defined $req) {
        print $c result_err(3);
        return;
    }
    printf "     id: '%s'\n", $req->{id} if DEBUG;
    if (!defined($req->{id})) {
        print $c result_err(7);
        return;
    }
    if ($req->{id} !~ /^[0-9]+$/) {
        print $c result_err(8);
        return;
    }
    if ($db->delIdentity($user, $req->{id})) {
        print $c result_ok;
    }
    else {
        print $c result_err(6);
    }
}

sub API_Credentials_List {
    my ($c,$r,$db,$user) = @_;
    print "  ** API_Credentials_List\n" if DEBUG;
    print $c result_ok [$db->getAllCredentials($user)];
}

sub API_Credentials_Add {
    my ($c,$r,$db,$user) = @_;
    print "  ** API_Credentials_Add\n" if DEBUG;

    my $login = randomStr(10);
    my $pass  = randomStr(20);
    my $hash  = sha1_hex($pass);

    print "HASH: $hash ",length($hash)," bytes\n";

    if ($db->addCredentials($user, $login, $hash)) {
        print $c result_ok { login => $login, pass => $pass };
    }
    else {
        print $c result_err(6);
    }
}

sub API_Credentials_Del {
    my ($c,$r,$db,$user) = @_;
    print "  ** API_Credentials_Del\n" if DEBUG;
    my $req;
    eval {
        $req = JSON::from_json($r->content);
    };
    if (!defined $req) {
        print $c result_err(3);
        return;
    }
    printf "     id: '%s'\n", $req->{id} if DEBUG;
    if (!defined($req->{id})) {
        print $c result_err(7);
        return;
    }
    if ($req->{id} !~ /^[0-9]+$/) {
        print $c result_err(8);
        return;
    }
    if ($db->delCredentials($user, $req->{id})) {
        print $c result_ok;
    }
    else {
        print $c result_err(6);
    }
}

sub API_Mail_Send {
    my ($c,$r,$db,$user) = @_;
    print "  ** API_Mail_Send\n" if DEBUG;

    my $req;
    eval {
        $req = JSON::from_json($r->content);
    };
    if (!defined $req) {
        print $c result_err(3);
        return;
    }
    defined($req->{from})    or do { print $c result_err(9); return };
    defined($req->{to})      or do { print $c result_err(9); return };
    defined($req->{message}) or do { print $c result_err(9); return };
    defined($req->{subject}) or do { print $c result_err(9); return };

    if ($req->{from} !~ /^[a-z0-9A-Z_\.-]+\@[a-z0-9A-Z_\.-]+\.[a-z]+$/) {
        print $c result_err(10);
        return;
    }
    if ($req->{to} !~ /^[a-z0-9A-Z_\.-]+\@[a-z0-9A-Z_\.-]+\.[a-z]+$/) {
        print $c result_err(10);
        return;
    }
    if (!$db->findIdentity($user,$req->{from})) {
        print $c result_err(11);
        return;
    }
    my $msg = new Ses::Message;
    $msg->{from} = $req->{from};
    $msg->{to} = $req->{to};
    $msg->writeMessage($req->{message},$req->{subject});

    $db->addCounters($user,1,$msg->{size});

    print $c result_ok { id => $msg->{id} };
}

sub API_Stats {
    my ($c,$r,$db,$user) = @_;
    print "  ** API_Stats\n" if DEBUG;

    print $c result_ok { mails => $user->{mails}, bytes => $user->{bytes} };
}

sub API_Error {
    my ($c,$r,$db,$user) = @_;
    print "  ** API_Error\n" if DEBUG;
    my $req;
    eval {
        $req = JSON::from_json($r->content);
    };
    if (!defined $req) {
        print $c result_err(3);
        return;
    }
    my $h = Ses::I18N->get_handle($user->{language});
    print $c result_ok { text => $h->maketext($req->{id}) };
}

