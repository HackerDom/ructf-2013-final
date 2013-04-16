package Ses::UserAPI;

use strict;
require LWP::UserAgent;
use Ses::Config;
use JSON -no_export;

#########################################################################################

sub new {
    my ($class, $host) = @_;

    my $ua = LWP::UserAgent->new;
    $ua->timeout(CFG_API_TIMEOUT);

    my $self = { ua => $ua, host => $host };
    bless $self, $class;
    return $self;
}

sub sendRequest {
    my ($self, $url, $args) = @_;

    my $req = HTTP::Request->new('POST', 'http://'.$self->{host}."/$url");
    $req->header('Content-Type'     => 'application/json');
    $req->header('X-Requested-With' => 'XMLHttpRequest');
    $req->header('Host'             => $self->{host});
    $req->content(JSON::to_json($args));
    warn JSON::to_json($args);

    my $r = $self->{ua}->request($req);
    warn $r->decoded_content;
    return undef, $r->status_line unless $r->is_success;

    my $json = JSON::from_json($r->decoded_content);
    return undef, $r->decoded_content unless $json->{status} eq 'OK';

    my $cookie = $r->header("Set-Cookie");
    $cookie=~/^([^;]+)/;
    return $json, undef, $1;
}

###############################################################################################

sub user {
    my ($self,$session) = @_;
    my ($r,$err) = $self->sendRequest("user", { session => $session } );
    return defined $r ? ($r->{uid},$r->{language}) : (undef,undef,$err);
}

sub register {
    my ($self,$login,$pass,$first,$last,$lang) = @_;
    my ($r,$err) = $self->sendRequest("register", {
        login      => $login,
        password   => $pass,
        first_name => $first,
        last_name  => $last,
        language   => $lang
    });
    return defined $r ? $r->{uid} : (undef,$err);
}

sub login {
    my ($self,$login,$pass) = @_;
    my ($r,$err,$cookie) = $self->sendRequest("login", { login => $login, password => $pass } );
    return defined $r ? $cookie : (undef,$err);
}

1;

