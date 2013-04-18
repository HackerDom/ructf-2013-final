#!/usr/bin/perl
use strict;

@ARGV==2 or die "Usage: named-conf-gen.pl <teams count> <dir>\n";
my ($TEAMS,$DIR) = @ARGV;

print " // RuCTF 2013 Bind config\n\n";

for (1..$TEAMS) {
    my $fname = "team$_.ructf.db";
    print_conf($_, $fname);
    write_zone($_,"$DIR/$fname");
}

exit 0;

sub print_conf {
    my ($n,$fname) = @_;
    print <<"END";
zone "team$_.ructf" {
        type master;
        file "$fname";
        notify no;
};

END
}

sub write_zone {
    my ($n,$fname) = @_;
    open F, ">$fname" or die "Error: cannot create file: $fname";
    my $zone = "team$n.ructf";
    my $time = curtime();
    print F <<"END";
;
; Zone file for $zone
;
\$TTL 3D
@       IN      SOA     ns.$zone. root.$zone. (
                        $time      ; serial, todays date + todays serial #
                        8H              ; refresh, seconds
                        2H              ; retry, seconds
                        4W              ; expire, seconds
                        1D )            ; minimum, seconds

                NS      ns              ; Inet Address of name server
                MX      10 mail         ; Primary Mail Exchanger

ns              A       172.16.16.100
\@               A       172.16.16.10$_
*               A       172.16.16.10$_

END
    close F;
}

sub curtime {
    my ($sec,$min,$hour,$mday,$mon,$year) = localtime;
    sprintf "%04d%02d%02d%02d%02d%02d", 1900+$year, $mon+1, $mday, $hour, $min, $sec;
}

