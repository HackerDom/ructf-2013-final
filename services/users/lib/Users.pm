package Users;
use Mojo::Base 'Mojolicious';

sub startup {
  my $self = shift;

  # Routers
  my $r = $self->routes;
  $r->get('/')->to('main#index');
}

1;
