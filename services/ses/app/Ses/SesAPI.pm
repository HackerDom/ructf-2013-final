package Ses::SesAPI;

use strict;
require LWP::UserAgent;
use Ses::Config;
use JSON -no_export;

sub new {
    my ($class, $endpoint, $cookie) = @_;

    my $ua = LWP::UserAgent->new;
    $ua->timeout(CFG_API_TIMEOUT);
    $ua->default_header('Cookie' => $cookie);

    my $self = { endpoint => $endpoint, ua => $ua };
    bless $self, $class;
    return $self;
}

sub sendRequest {
    my ($self, $method, $args) = @_;
    my $url = sprintf "%s/%s", $self->{endpoint}, $method;

    my $r       = $self->{ua}->post( $url, Content => JSON::to_json($args) );

#   my $content = $r->decoded_content();
#   print "DEBUG: Ses::SesAPI.sendRequest: content = $content\n";
#   my $result;
#   $result = JSON::from_json($content) if $r->is_success;
#   print "DEBUG: Ses::SesAPI.sendRequest: result->{status} = ".$result->{status}."\n";

    return $r->is_success, $r->decoded_content, $r->status_line;
}

