package Users::Main;
use Mojo::Base 'Mojolicious::Controller';
use Mojo::Util 'sha1_sum';
use Mango::BSON ':bson';

sub index {
  my $self = shift;
  $self->render();
}

sub register {
  my $self = shift;
  $self->render_later;
  my $db = $self->mango->db('users');
  my $user;

  if ($self->req->is_xhr) {
    my $json = $self->req->json;
    return $self->render_json($self->_error(0, 'invalid input')) unless $json;
    my $pointer = Mojo::JSON::Pointer->new;
    for my $field (qw/login password first_name last_name language/) {
      return $self->render_json($self->_error(0, 'invalid input'))
        unless $pointer->contains($json, "/$field");
      $user->{$field} = $pointer->get($json, "/$field");
    }
  }
  $user->{salt} = $self->_salt;
  $user->{hash} = sha1_sum delete $user->{password}, $user->{salt};
  $db->collection('user')->insert(
    $user => sub {
      my ($collection, $err, $uid) = @_;
      if ($err) {
        given ($err) {
          when (/E11000/) {
            return $self->render_json($self->_error(1, 'login already used'));
          }
          default {
            $self->app->log->error("Error while insert user: $err");
            return $self->render_json($self->_error(-1, 'internal error'));
          }
        }
      }
      return $self->render_json({status => 'OK', uid => $uid});
    });
}

sub _salt {
  return sha1_sum rand . $$;
}

sub _error {
  my ($self, $code, $str) = @_;
  return {status => 'FAIL', error => {code => $code, str => $str}};
}

1;
