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

sub genmail {
    my @C = ('a'..'z','0'..'9');
    my $mail = join '', map { $C[int rand @C] } 1..12;
    substr($mail,8,0) = '@';
    return "$mail.com";
}

sub callSesApi {
    my ($action,$args) = @_;
    print "$action ... ";
    my @S = `$CALLAPI http://$HOST:$PORT $action $SESSION $args`;
    print $/,@S,$/ if $OUTPUT;
    my $exit = $?>>8;
    if ($exit) {
        print "exit code: $exit\n";
        return undef;
    }
    my $Json = join '', grep { /^[^#]/ } @S;
    my $Reply = from_json($Json);
    if ($Reply->{status} ne "OK") {
        printf "%s (%d)\n", $Reply->{status}, $Reply->{error};
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

print "Usage: ses-test-identity-api.pl [add] [list] [del]\n" if @ARGV==0;

## Step 1. Add some emails.

if ($ARGS{add}) {
    my $ok = 0;
    $tStart = time();
    for (1..$RUNS) {
        my $email = genmail();
        printf "%3d/%d ", $_, $RUNS;
        callSesApi("identity/add", "email:$email") and $ok++;
    }
    printf "# Done in %d sec (OK=$ok)\n", time()-$tStart;
}

## Step 2. List all emails

if ($ARGS{list}) {
    $tStart = time();
    print "       ";
    my $R = callSesApi("identity/list");
    @A = @{$R->{result}} if defined $R;
    printf "# Got %d mails\n", 0+@A;
    printf "# Done in %d sec\n", time()-$tStart;
}

## Step 3. Delete all emails

if ($ARGS{del}) {
    my $ok = 0;
    $tStart = time();
    my $i = 1;
    for (@A) {
        printf "%3d/%d ", $i++, 0+@A;
        callSesApi("identity/del", "id:$_->{id}") and $ok++;
    }
    printf "# Done in %d sec (OK=$ok)\n", time()-$tStart;
}

