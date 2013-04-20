#!/usr/bin/perl

use strict;
use threads;
use Digest::SHA qw(sha1_hex);
use IO::Socket::INET;
use MIME::Base64;
use Net::DNS;
use Net::SMTP;
use Net::Domain qw(hostfqdn);
use Ses::Message;
use Ses::Config;
use Ses::Db;

################ Configuration ####################

$|=1;
sub DEBUG { 1 }
sub QUEUE_SLEEP { 60 }

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

$SIG{'CHLD'} = 'IGNORE';

threads->create(\&queue_processor)->detach();

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
        last if $s eq '.';
        push @data, $s;
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

sub queue_processor {
    while (1) {
        print "queue_processor is alive\n";
        my $dir = CFG_QUEUE_DIR;
        for my $fname (<$dir/*>) {
            $fname =~ m|/([^/.]+)\.msg|;
            eval {
                queue_deliver($1);
            };
        }
        sleep QUEUE_SLEEP;
    }
}

sub queue_deliver {
    my $id = shift;
    my $Msg = new Ses::Message($id);
    my @data = $Msg->read();
    chomp for @data;
    my ($from,$to) = @data[0,1];
    $from =~ s/^MAIL FROM://;
    $from =~ s/ //g;
    $from =~ s/[<>]//g;
    $to =~ s/^RCPT TO://;
    $to =~ s/ //g;
    $to =~ s/[<>]//g;
    print " queue_deliver: $id: $from -> $to\n";

    my $sendmail = `which sendmail` or do {
        warn "Error: sendmail command not found, skip delivery\n";
        return;
    };
    $sendmail =~ s/[\r\n]//g;

    $from =~ s/[^a-z0-9_@=.-]//g;
    $to   =~ s/[^a-z0-9_@=.-]//g;

    open MAIL, "| $sendmail -f $from $to" or do {
        warn "Error: sendmail command failed to execute, skip delivery\n";
        return;
    };
    for my $s (@data) {
        print MAIL "$s\n";
    }
    close MAIL;
    my $ret = $? >> 8;
    if ($ret!=0) {
        warn "Error: sendmail failed with code $ret\n";
        return;
    }
    print "Sent OK, remove from queue\n";
    $Msg->remove();
}

