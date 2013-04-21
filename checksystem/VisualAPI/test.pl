#!/usr/bin/perl
use strict;
use JSON;
use DBI;

my ($DBNAME,$DBHOST,$DBPORT,$DBUSER,$DBPASS) = qw(ructfetest 127.0.0.1 5432 testuser test1234);

my %H = (
    points_history => \&points_history,
);

my $act = shift or die "Give action as first argument\n";
exists $H{$act} ? printf "%s\n", $H{$act}->() : die "Error: unknown action\n";

my $connstr = "DBI:Pg:dbname=$DBNAME;host=$DBHOST;port=$DBPORT";
my $dbh = DBI->connect($connstr, $DBUSER, $DBPASS, {'RaiseError' => 1});


$dbh->disconnect();

exit 0;

########################################################################################################

sub points_history {
    my $time = shift;
    my @t;
    push @t, { team => "Test", points => [ 1, 1, 4, 5, 5, 10, 11, 13 ] };
    push @t, { team => "Foo",  points => [ 2, 3, 9, 9, 9, 99, 99, 993 ] };
    JSON::to_json( \@t );
}

