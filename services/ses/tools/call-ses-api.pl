#!/usr/bin/perl
use strict;
use warnings;
use JSON;
use Ses::SesAPI;

sub DEBUG{1};

my $endpoint = shift or print_usage();
my $method   = shift or print_usage();
my $session  = shift or print_usage();
my $params = get_params(@ARGV);
my $json = JSON::to_json($params);

if (DEBUG) {
    print "#------------------> REQUEST ----------------------------\n";
    print "# Endpoint: $endpoint\n";
    print "# Method:   $method\n";
    print "# Session:  $session\n";
    print "# JSON:     $json\n";
    print "#------------------< REPLY ------------------------------\n";
}

my $api = new Ses::SesAPI($endpoint,"session=$session");
my ($reply,$status) = $api->sendRequest($method,$params);

printf "# Status: %s\n", $status;
print  defined $reply ? JSON::to_json($reply) : '{}';
print $/;

#exit defined $reply ? 0 : 1;

#####################################################################################################################

sub print_usage {
    print <<'END';

  Usage: call-ses-api.pl <Endpoint> <Method> <Session> [param1:value1] [param2:value2] ...

END
    exit 0;
}

sub get_params {
	my %params;
	for my $kv (@_) {
		my ($key,$val) = split ':', $kv;
		$params{$key}=$val;
	}
	return \%params;
}

