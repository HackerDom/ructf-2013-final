#!/usr/bin/perl -lw

use strict;
use Mojo::UserAgent;
use Mojo::URL;
use Mojo::Asset::Memory;
use Mojo::Asset::File;
use Mojo::Log;
use Mojo::Util 'md5_sum';
use Data::Dumper;

my ($SERVICE_OK, $FLAG_GET_ERROR, $SERVICE_CORRUPT, $SERVICE_FAIL, $INTERNAL_ERROR) = (101, 102, 103, 104, 110);
my %MODES = (check => \&check, get => \&get, put => \&put);

my ($mode, $host) = (shift // '', shift // '');
unless ($mode ~~ %MODES and $host =~ /team(\d+)\.ructf$/) {
    warn "Invalid input data. Corrupt mode or ip address.";
    exit $INTERNAL_ERROR;
}

my $users_url = Mojo::URL->new("http://$host/");
my $ips_url = Mojo::URL->new("http://ips.$host/");
my $ua = Mojo::UserAgent->new(max_redirects => 5);
my $log = Mojo::Log->new;
exit $MODES{$mode}->(@ARGV);

sub register {
    my ($login, $password) = @_;
    $users_url->path('/register');
    my $tx = $ua->post($users_url, {'X-Requested-With' => 'XMLHttpRequest'} => json =>
        {
            login      => $login,
            password   => $password,
            first_name => $login,
            last_name  => '',
            language   => 'ru'
        });
    my $res = $tx->success;
    unless ($res) {
        print 'Error on register';
        return $SERVICE_FAIL;
    }
    my $json = $res->json;
    unless ($json->{status} eq 'OK') {
        print 'Error2 on register';
        return $SERVICE_FAIL;
    }
    return $SERVICE_OK;
}

sub login {
    my ($login, $password) = @_;
    $users_url->path('/login');
    my $tx = $ua->post($users_url, {'X-Requested-With' => 'XMLHttpRequest'} => json =>
        {
            login    => $login,
            password => $password
        });
    my $res = $tx->success;
    unless ($res) {
        print 'Error on login';
        exit $SERVICE_FAIL;
    }
    my $json = $res->json;
    unless ($json->{status} eq 'OK') {
        print 'Error2 on login';
        exit $SERVICE_FAIL;
    }
    return 1;
}

sub check {
    my $login = md5_sum rand.rand;
    my $password = md5_sum rand.rand;
    $log->info("Create and login for $login and $password");
    my $x = register($login, $password);
    exit  $x unless $x == $SERVICE_OK;
    login($login, $password);
    my $src_port = 60001 + int rand 5535;
    my $dst_port = int rand 65535;
    my $host = '10.23.' . (1+int(rand(15))) . '.2';
    $log->info("Create proxy rules for $src_port and $dst_port and $host");
    # ADD
    $ips_url->path('/api_add');
    my $tx = $ua->post($ips_url => json =>
        {
            src_port => int($src_port),
            dst_host => $host,
            dst_port => int($dst_port),
            rules => 'test'
        });
    my $res = $tx->success;
    unless ($res) {
        print 'Error on add rule';
        exit $SERVICE_FAIL;
    }
    my $json = $res->json;
    unless ($json->{status} eq 'OK') {
        $log->error(Dumper $json);
        print 'Error2 on add rule';
        exit $SERVICE_FAIL;
    }
    # LIST
    $ips_url->path('/api_list');
    $tx = $ua->get($ips_url);
    $res = $tx->success;
    unless ($res) {
        print 'Error on list rule';
        exit $SERVICE_FAIL;
    }
    $json = $res->json;
    unless ($json->{status} eq 'OK') {
        print 'Error2 on list rule';
        exit $SERVICE_FAIL;
    }
    my $rule = $json->{rules}->[0];
    unless ($rule) {
        print 'Error3 on list rule';
        exit $SERVICE_FAIL;
    }
    # DELETE
    $ips_url->path('/api_del');
    $tx = $ua->post($ips_url => json =>
        {
            src_port => $src_port,
            dst_port => $dst_port,
            dst_host => $host
        });
    $res = $tx->success;
    unless ($res) {
        print 'Error on delete rule';
        exit $SERVICE_FAIL;
    }
    $json = $res->json;
    unless ($json->{status} eq 'OK') {
        print 'Error2 on delete rule';
        exit $SERVICE_FAIL;
    }
    return $SERVICE_OK;
}

sub get {
    my ($id, $flag) = @_;
    my ($login, $password) = ('user', '[eqgbplfbl;buehlf');
    $log->info("Create and login for $login and $password");
    register($login, $password);
    login($login, $password);
    $ips_url->path('/get');
    $ips_url->port(60000);
    my $tx = $ua->get($ips_url->query(key => $id));
    my $res = $tx->success;
    unless ($res) {
        print 'Error on get flag';
        exit $SERVICE_FAIL;
    }
    unless ($res->body eq $flag) {
        print 'Error2 on set flag';
        exit $SERVICE_CORRUPT;
    }
    exit $SERVICE_OK;
}

sub put {
    my ($id, $flag) = @_;
    my ($login, $password) = ('user', '[eqgbplfbl;buehlf');
    $log->info("Create and login for $login and $password");
    register($login, $password);
    login($login, $password);
    # ADD
    $ips_url->path('/api_add');
    my $tx = $ua->post($ips_url => json =>
        {
            src_port => 60000,
            dst_host => '10.23.0.5',
            dst_port => 3000,
            rules => 'never surrender'
        });
    $ips_url->path('/add');
    $ips_url->port(60000);
    $tx = $ua->post($ips_url->query(key => $id) => $flag);
    my $res = $tx->success;
    unless ($res) {
        print 'Error on set flag';
        exit $SERVICE_FAIL;
    }
    unless ($res->body eq 'Ok!') {
        print 'Error2 on set flag';
        exit $SERVICE_CORRUPT;
    }
    exit $SERVICE_OK;
}
