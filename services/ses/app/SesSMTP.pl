#!/usr/bin/perl

use strict;
use threads;
use Digest::SHA qw(sha1_hex);
use IO::Socket::INET;
use MIME::Base64;
use Ses::Message;
use Ses::Config;
use Ses::Db;

################ Configuration ####################

$|=1;
sub DEBUG { 1 }

############ End of Configuration #################

my $s = IO::Socket::INET->new(
        Proto     => 'tcp',
        LocalPort => CFG_SMTP_PORT,
        Listen    => 20,
        Reuse     => 1
    ) or die "SES SMTP Server failed to start: $!\n";

printf "SES SMTP Server started at %s:%d\n", $s->sockhost(), $s->sockport();

$SIG{'INT'} = sub {
    print "<Ctrl+C> pressed\n";
    close $s;
    exit 0;
};

while (my $c = $s->accept) {
    printf "Client connected: %s:%s\n", $c->sockhost(), $c->sockport();
    threads->create(\&process_client, $c)->detach();
}

print "This should never happen\n";
exit 0;

#############################################################################

sub exit_client {
    my $c = shift;
    print $c "221 Bye\r\n" if defined $c;
    $c->close;
    threads->exit();
}

sub process_client {
    my $c = shift;
    print $c "220 SES SMTP Service ready\r\n";
    my ($s,$from,$to);
    while (1) {
        $s = <$c>;
        print $s;
        $s =~ /^QUIT/ and exit_client $c ;
        $s =~ /^HELO \S+/ and last;
        $s =~ /^EHLO \S+/ and last;
        print $c "500 Syntax error, command unrecognised\r\n";
    }
    print $c "250-ses\r\n";
    print $c "250-PIPELINING\r\n";
    print $c "250-8BITMIME\r\n";
    print $c "250 AUTH LOGIN\r\n";
    while (1) {
        $s = <$c>;
        print $s;
        $s =~ /^QUIT/ and exit_client $c ;
        $s =~ /^AUTH LOGIN/ and last;
        print $c "500 Expected: AUTH LOGIN\r\n";
    }

    print $c "334 VXNlcm5hbWU6\r\n";
    chomp(my $login = <$c>);
    $login = decode_base64($login);

    printf $c "334 UGFzc3dvcmQ6\r\n";
    chomp(my $pass = <$c>);
    $pass = decode_base64($pass);

    my $db = new Ses::Db;
    if (!$db->authenticate($login, sha1_hex($pass))) {
        print $c "535 Username and Password not accepted\r\n";
        exit_client $c;
    }
    print $c "235 2.0.0 OK\r\n";

    while (1) {
        $s = <$c>;
        print $s;
        $s =~ /^QUIT/ and exit_client $c ;
        $s =~ /^MAIL FROM:(\S+)/ and do { $from=$1; last };
        print $c "500 Expected: MAIL FROM\r\n";
    }
    print "from = $from\n";
    print $c "250 2.1.0 OK\r\n";

    while (1) {
        $s = <$c>;
        print $s;
        $s =~ /^QUIT/ and exit_client $c ;
        $s =~ /^RCPT TO:(\S+)/ and do { $to=$1; last };
        print $c "500 Expected: RCPT TO\r\n";
    }
    print "from = $to\n";
    print $c "250 2.1.5 OK\r\n";

    while (1) {
        $s = <$c>;
        print $s;
        $s =~ /^QUIT/ and exit_client $c ;
        $s =~ /^DATA/ and last;
        print $c "500 Expected: DATA\r\n";
    }
    print $c "354 End data with <CR><LF>.<CR><LF>\r\n";

    my @data = ();
    while (1) {
        $s = <$c>;
        $s =~ s/[\r\n]+$//;
        push @data, $s;
        last if $s eq '.';
    }
    my $msg = new Ses::Message;
    $msg->{from} = $from;
    $msg->{to} = $to;
    $msg->writeRaw(join $/,@data);
    printf $c "250 2.0.0 Ok: queued as %s\r\n", $msg->{id};
    printf "Ok: queued as %s\r\n", $msg->{id};
    exit_client $c;
}

#############################################################################


