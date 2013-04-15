#!/usr/bin/perl

use strict;
use Ses::UserAPI;
use JSON;

sub usage {
    die "Usage: call-user-api.pl <endpoint> [user <session>] [register <login> <pass> <first> <last> <lang>] [login <login> <pass>]\n";
}

my %ACTIONS = (
    user     => \&user,
    register => \&register,
    login    => \&login,
);

my $endpoint = shift or usage;
my $a = shift        or usage;
exists $ACTIONS{$a}  or usage;

my $api = new Ses::UserAPI($endpoint);

$ACTIONS{$a}->(@ARGV);
exit 0;

###########################################################

sub result {
    my ($res,$err,$cookie) = @_;
    if (defined $res) {
        print "UserAPI call: SUCCESS\n";
        print JSON::to_json($res, { pretty => 1 });
        print "Set-Cookie: $cookie\n";
    }
    else {
        print "UserAPI call: FAILURE: $err\n";
    }
}

sub user {
	result $api->user(@_);
}

sub register {
    my ($uid,$err) = $api->register(@_);
    print defined $uid ? "OK: uid='$uid'\n" : "Error: $err\n";
}

sub login {
    my ($cookie,$err) = $api->login(@_);
    print defined $cookie ? "OK: cookie='$cookie'\n" : "Error: $err\n";
}

