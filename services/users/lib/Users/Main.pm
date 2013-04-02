package Users::Main;
use Mojo::Base 'Mojolicious::Controller';

sub welcome {
  my $self = shift;
  $self->render();
}

1;
