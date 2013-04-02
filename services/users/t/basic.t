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

done_testing();
