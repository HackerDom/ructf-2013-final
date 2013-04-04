package SesConfig;

use strict;
use Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

$VERSION     = 1.00;
@ISA         = qw(Exporter);
@EXPORT      = qw(CFG_USERAPI_ENDPOINT CFG_API_TIMEOUT);
@EXPORT_OK   = ();
%EXPORT_TAGS = ();

sub CFG_USERAPI_ENDPOINT { 'http://127.0.0.1/cgi-bin/user' }
sub CFG_API_TIMEOUT      { 2 }

1;

