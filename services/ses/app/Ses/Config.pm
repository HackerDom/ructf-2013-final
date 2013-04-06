package Ses::Config;

use strict;
use Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

$VERSION     = 1.00;
@ISA         = qw(Exporter);
@EXPORT      = qw(CFG_USERAPI_ENDPOINT CFG_API_TIMEOUT CFG_DB_DIR CFG_HTTP_LISTEN_ADDR CFG_HTTP_LISTEN_PORT);
@EXPORT_OK   = ();
%EXPORT_TAGS = ();

sub CFG_USERAPI_ENDPOINT { 'http://127.0.0.1/cgi-bin/user' }
sub CFG_API_TIMEOUT      { 2 }
sub CFG_DB_DIR           { '/home/dima/git/ructf2013-final/services/ses/app/db' }
sub CFG_HTTP_LISTEN_ADDR { '0.0.0.0' }
sub CFG_HTTP_LISTEN_PORT { 8888 }

1;

