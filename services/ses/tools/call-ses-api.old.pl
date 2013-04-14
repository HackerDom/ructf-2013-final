#!/usr/bin/perl
use strict;
use warnings;
use JSON;
use LWP::UserAgent;

sub DEBUG{1};

my $url     = shift or print_usage();
my $session = shift or print_usage();

my $params = get_params(@ARGV);
my $json = to_json( $params, {pretty=>0} );

my $cookie = "session=$session";

if (DEBUG) {
    print "#------------------> REQUEST ----------------------------\n";
    print "# URL:    $url\n";
    print "# Cookie: $cookie\n";
    print "# JSON:   $json\n";
    print "#------------------< REPLY ------------------------------\n";
}

my $ua       = LWP::UserAgent->new();
$ua->default_header('Cookie' => $cookie);
my $response = $ua->post( $url, Content => $json );
my $content  = $response->decoded_content();
#$content = encode(CODEPAGE, decode 'utf-8', $content);

print "# Status: ",$response->status_line,$/ if DEBUG;
if ($response->is_success) {
    print to_json( from_json($content), {pretty=>1} ).$/;
    exit 0;
}
else {
    exit 1;
}

#####################################################################################################################

sub print_usage {
	print <<'END';
	
  Usage: call-ses-api.pl <URL> <Session> [param1:value1] [param2:value2] ...

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
