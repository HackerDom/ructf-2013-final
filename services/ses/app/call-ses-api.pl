#!/usr/bin/perl

use strict;
use UserAPI;

my $session = shift or die "Usage: call-user-api.pl session\n";
my ($uid, $err) = CallUserAPI($session);

if (defined $uid) {
    printf "UserAPI call OK. Got uid: %s\n", $uid;
}
else {
    printf "UserAPI call Failed. %s\n", $err;
}

