use Mojo::Base -strict;

use Test::More;
use Test::Mojo;

do 'script/indexes';

my $t = Test::Mojo->new('Users');
$t->get_ok('/')->status_is(200);
$t->post_ok('/register' => {'X-Requested-With' => 'XMLHttpRequest'} => json => {})->status_is(200)
  ->json_is('/status' => 'FAIL')->json_is('/error/code' => 0)
  ->json_is('/error/str' => 'invalid input');

$t->post_ok(
  '/register' => {'X-Requested-With' => 'XMLHttpRequest'} => json => {
    login      => 'avkhozov',
    password   => 'my_Secret',
    first_name => 'Andrey',
    last_name  => 'Khozov',
    language   => 'ru'
  })->status_is(200)->json_is('/status' => 'OK')->json_has('/uid');

$t->post_ok(
  '/register' => {'X-Requested-With' => 'XMLHttpRequest'} => json => {
    login      => 'avkhozov',
    password   => 'my_Secret',
    first_name => 'Andrey',
    last_name  => 'Khozov',
    language   => 'ru'
  }
  )->status_is(200)->json_is('/status' => 'FAIL')->json_is('/error/code' => 1)
  ->json_is('/error/str' => 'login already used');

$t->post_ok('/login' => {'X-Requested-With' => 'XMLHttpRequest'} => json =>
    {login => 'avkhozov', password => '123',})->status_is(200)->json_is('/status' => 'FAIL')
  ->json_is('/error/code' => 3)->json_is('/error/str' => 'invalid login or password');

$t->post_ok('/login' => {'X-Requested-With' => 'XMLHttpRequest'} => json =>
    {login => 'andgein', password => '123',})->status_is(200)->json_is('/status' => 'FAIL')
  ->json_is('/error/code' => 3)->json_is('/error/str' => 'invalid login or password');

$t->post_ok('/login' => {'X-Requested-With' => 'XMLHttpRequest'} => json =>
    {login => 'avkhozov', password => 'my_Secret',})->status_is(200)->json_is('/status' => 'OK');
my ($cookie) = $t->ua->cookie_jar->all;
is $cookie->name, 'session', 'cookie has session';
like $cookie->value, qr/^[0-9a-f]{24}![0-9a-f]{40}$/, 'session looks good';

done_testing();
