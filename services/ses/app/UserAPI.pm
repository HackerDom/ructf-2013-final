package UserAPI;

use strict;
require LWP::UserAgent;
use SesConfig;
use JSON -no_export;
use Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

$VERSION     = 1.00;
@ISA         = qw(Exporter);
@EXPORT      = qw(CallUserAPI);
@EXPORT_OK   = ();
%EXPORT_TAGS = ();

# Проверить сессию у User API

sub CallUserAPI {

    my $session = shift;

    my $req = HTTP::Request->new('POST', CFG_USERAPI_ENDPOINT);
    $req->header('Content-Type' => 'application/json');
    $req->content(JSON::to_json({ session => $session }));

    my $ua = LWP::UserAgent->new;
    $ua->timeout(CFG_API_TIMEOUT);
    my $r = $ua->request($req);

    return undef, $r->status_line unless $r->is_success;

    my $json = JSON::from_json($r->decoded_content);
    return undef, sprintf "%s { code: %d, str: '%s' }",
        $json->{status}, $json->{error}->{code}, $json->{error}->{str} 
        unless $json->{status} eq 'OK';

    return $json->{uid};
}

1;

