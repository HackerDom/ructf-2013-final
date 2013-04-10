package Users;
use Mojo::Base 'Mojolicious';
use Mojo::Util 'md5_sum';
use MongoDB;

sub startup {
  my $self = shift;

  $self->plugin('Config', {file => 'user.config'});
  $self->secret(md5_sum rand . $$);

  # Routers
  my $r = $self->routes;
  $r->get('/')->to('main#index')->name('index');
  $r->post('/register')->to('main#register')->name('register');
  $r->post('/login')->to('main#login')->name('login');
  $r->post('/user')->to('main#user')->name('user');

  $r->get('/login')->to('main#sign_in')->name('sign_in');
  $r->get('/register')->to('main#sign_up')->name('sign_up');
  $r->get('/logout')->to('main#logout')->name('logout');

  $self->helper(
    db => sub {
      state $db = MongoDB::MongoClient->new;
    });
}

1;
