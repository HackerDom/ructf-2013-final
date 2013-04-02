package Users;
use Mojo::Base 'Mojolicious';
use Mango;

sub startup {
  my $self = shift;

  # Routers
  my $r = $self->routes;
  $r->get('/')->to('main#index')->name('index');
  $r->post('/register')->to('main#register')->name('register');

  $self->helper(
    mango => sub {
      state $mango = Mango->new;
    });
}

1;
