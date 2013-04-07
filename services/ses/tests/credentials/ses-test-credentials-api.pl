#!/usr/bin/perl

use strict;
use JSON;
$|=1;

#########################     Config    #########################

my $CALLAPI = '../../tools/call-ses-api.pl';
my $HOST    = '127.0.0.1';
my $PORT    = 8888;
my $SESSION = 'qwer';
my $RUNS    = 10;
my $OUTPUT  = 0;    # Print commands' stdout

######################### End of config #########################

sub callSesApi {
    my ($action,$args) = @_;
    print "$action ... ";
    my @S = `$CALLAPI http://$HOST:$PORT/$action $SESSION $args`;
    print $/,@S,$/ if $OUTPUT;
    my $exit = $?>>8;
    if ($exit) {
        print "exit code: $exit\n";
        return undef;
    }
    my $Json = join '', grep { /^[^#]/ } @S;
    my $Reply = from_json($Json);
    if ($Reply->{status} ne "OK") {
        printf "%s (%s,%s)\n", $Reply->{status}, $Reply->{error}->{code}, $Reply->{error}->{str};
        return undef;
    }
    else {
        print "OK\n";
        return $Reply;
    }
}

my %ARGS = map { $_, 1 } @ARGV;
my $tStart;
my @A;

print "Usage: ses-test-credentials-api.pl [add] [list] [del]\n" if @ARGV==0;

## Step 1. Add some emails.

if ($ARGS{add}) {
    my $ok = 0;
    $tStart = time();
    for (1..$RUNS) {
        printf "%3d / %d ", $_, $RUNS;
        callSesApi("credentials/add") and $ok++;
    }
    printf "# Done in %d sec (OK=$ok)\n", time()-$tStart;
}

## Step 2. List all emails

if ($ARGS{list}) {
    $tStart = time();
    print "       ";
    my $R = callSesApi("credentials/list");
    @A = @{$R->{result}};
    printf "# Got %d mails\n", 0+@A;
    printf "# Done in %d sec\n", time()-$tStart;
}

## Step 3. Delete all emails

if ($ARGS{del}) {
    my $ok = 0;
    $tStart = time();
    my $i = 1;
    for (@A) {
        printf "%3d / %d ", $i++, 0+@A;
        callSesApi("credentials/del", "id:$_->{id}") and $ok++;
    }
    printf "# Done in %d sec (OK=$ok)\n", time()-$tStart;
}

