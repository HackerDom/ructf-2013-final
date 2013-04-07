package Ses::Utils;

use strict;
use Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

$VERSION     = 1.00;
@ISA         = qw(Exporter);
@EXPORT      = qw(randomStr);
@EXPORT_OK   = ();
%EXPORT_TAGS = ();

my @CHARSET_DEFAULT = ('a'..'z','0'..'9');

sub randomStr {
    my $len = shift;
    my @C = (@_>0 ? @_ : @CHARSET_DEFAULT);
    return join '', map { $C[int rand @C] } 1..$len;
}

