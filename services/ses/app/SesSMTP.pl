#!/usr/bin/perl

use strict;
use threads;
use Digest::SHA qw(sha1_hex);
use IO::Socket::INET;
use MIME::Base64;
use Ses::Config;
use Ses::Db;

################ Configuration ####################

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

exit 0;

#############################################################################

sub exit_client {
    my $c = shift;
    print $c "221 Bye\r\n";
    $c->close;
    undef $c;
    print "exit_client\n";
    threads->exit();
}

sub process_client {
    my $c = shift;
    print $c "220 SES SMTP Service ready\r\n";
    my $s;
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

    print $c "334 VXNlcm5hbWU6\r\n";    # Username:
    chomp(my $login = <$c>);
    $login = decode_base64($login);

    print $c "334 UGFzc3dvcmQ6\r\n";    # Password:
    chomp(my $pass = <$c>);
    $pass = decode_base64($pass);
    print $c "235 2.0.0 OK\r\n";

    print "DEBUG: login=$login, pass=$pass\n";
    my $db = new Ses::Db;
    if (!$db->authenticate($login, sha1_hex($pass))) {
        print $c "535 Username and Password not accepted\r\n";
        sleep 2;
        exit_client $c;
    }

    while (1) {
        $s = <$c>;
        print $s;
        $s =~ /^QUIT/ and exit_client $c ;
        $s =~ /^MAIL FROM:(\S+)/ and last;
        print $c "500 Expected: MAIL FROM\r\n";
    }
    my $from = $1;
    print $c "250 2.1.0 OK\r\n";

    while (1) {
        $s = <$c>;
        print $s;
        $s =~ /^QUIT/ and exit_client $c ;
        $s =~ /^RCPT TO:(\S+)/ and last;
        print $c "500 Expected: RCPT TO\r\n";
    }
    my $to = $1;
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
        print "$s\n";
        push @data, $s;
        last if $s eq '.';
    }
    print $c "250 2.0.0 Ok: queued as XXXYYYZZZ\r\n";
    exit_client $c;
}

