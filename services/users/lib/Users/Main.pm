package Users::Main;
use Mojo::Base 'Mojolicious::Controller';
use Mojo::Util qw/sha1_sum hmac_sha1_sum secure_compare/;
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
  $user->{hash} = sha1_sum delete($user->{password}) . $user->{salt};
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

sub login {
  my $self = shift;
  $self->render_later;
  my $db = $self->mango->db('users');
  my ($login, $password);

  if ($self->req->is_xhr) {
    my $json = $self->req->json;
    return $self->render_json($self->_error(0, 'invalid input')) unless $json;
    my $pointer = Mojo::JSON::Pointer->new;
    for my $field (qw/login password/) {
      return $self->render_json($self->_error(0, 'invalid input'))
        unless $pointer->contains($json, "/$field");
    }
    ($login, $password) = ($json->{login}, $json->{password});
  }
  $db->collection('user')->find_one(
    {login => $login} => sub {
      my ($collection, $err, $user) = @_;
      if ($err) {
        $self->app->log->error("Error while find_one user: $err");
        return $self->render_json($self->_error(-1, 'internal error'));
      }
      return $self->render_json($self->_error(3, 'invalid login or password')) unless $user;

      if ($user->{hash} eq sha1_sum $password . $user->{salt}) {
        my $token = "$user->{_id}";
        my $data = $token . '!' . hmac_sha1_sum $token, $self->app->secret;
        $self->cookie(session => $data);
        return $self->render_json({status => 'OK'});
      } else {
        return $self->render_json($self->_error(3, 'invalid login or password'));
      }
    });
}

sub user {
  my $self = shift;
  $self->render_later;
  my $db = $self->mango->db('users');
  my $session;

  if ($self->req->is_xhr) {
    my $json = $self->req->json;
    return $self->render_json($self->_error(0, 'invalid input')) unless $json;
    my $pointer = Mojo::JSON::Pointer->new;
    for my $field (qw/session/) {
      return $self->render_json($self->_error(0, 'invalid input'))
        unless $pointer->contains($json, "/$field");
    }
    $session = $json->{session};
  }
  my ($uid, $sign) = split '!', $session;
  if ($sign && secure_compare($sign, hmac_sha1_sum $uid, $self->app->secret)) {
    $db->collection('user')->find_one(
      {_id => bson_oid $uid},
      sub {
        my ($collection, $err, $user) = @_;
        if ($err) {
          $self->app->log->error("Error while find_one user: $err");
          return $self->render_json($self->_error(-1, 'internal error'));
        }
        my $response = {status => 'OK'};
        @{$response}{'uid', 'first_name', 'last_name', 'language'} =
          @{$user}{'_id',   'first_name', 'last_name', 'language'};
        return $self->render_json($response);
      });
  } else {
    return $self->render_json($self->_error(5, 'invalid sign, possible hack attempt'));
  }
}

sub _salt {
  return sha1_sum rand . $$;
}

sub _error {
  my ($self, $code, $str) = @_;
  return {status => 'FAIL', error => {code => $code, str => $str}};
}

1;
