package Ses::Config;

use strict;
use Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

$VERSION     = 1.00;
@ISA         = qw(Exporter);
@EXPORT      = qw(
                    CFG_USERAPI_ENDPOINT
                    CFG_API_TIMEOUT
                    CFG_DB_DIR
                    CFG_QUEUE_DIR
                    CFG_HTTP_PORT
                    CFG_SMTP_PORT
                );
@EXPORT_OK   = ();
%EXPORT_TAGS = ();

sub CFG_USERAPI_ENDPOINT { 'http://127.0.0.1/cgi-bin/user' }
sub CFG_API_TIMEOUT      { 2 }
sub CFG_DB_DIR           { '/home/ses/db' }
sub CFG_QUEUE_DIR        { '/home/ses/queue' }
sub CFG_HTTP_PORT        { 8888 }
sub CFG_SMTP_PORT        { 2525 }

1;

