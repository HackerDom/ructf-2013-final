#!/usr/bin/perl -wl

use Mojolicious::Lite;
use Mojo::Util qw/b64_encode b64_decode/;

my %data = ();
open my $f, 'data.txt' or die "[open:r]: data.txt: $!";
while (<$f>) {
    my ($key, $data) = split ' ';
    $data{b64_decode($key)} = b64_decode $data;
}
close $f;
open $f, '>>data.txt' or die "[open:a]: data.txt: $!";
my $tmp = select $f;
$| = 1;
select $tmp;

post '/add' => sub {
    my $self = shift;
    my $data = $self->req->body;
    if (256*1024 < length $data) {
        $self->res->code(400);
        return $self->render_data('Too long! Must be <= 256k.');
    }
    my $key = $self->req->param('key');
    unless (defined $key) {
        $self->res->code(400);
        return $self->render_data(q{Param 'key' not found!});
    }
    $data{$key} = $data;
    print $f b64_encode($key, ''), ' ', b64_encode($data, '');
    return $self->render_data('Ok!');
};

get '/get' => sub {
    my $self = shift;
    my $key = $self->req->param('key');
    unless (defined $key) {
        $self->res->code(400);
        return $self->render_data(q{Param 'key' not found!});
    }
    unless (defined $data{$key}) {
        $self->res->code(404);
        return $self->render_data('Key not found!');
    }
    return $self->render_data($data{$key});
};

app->start;
