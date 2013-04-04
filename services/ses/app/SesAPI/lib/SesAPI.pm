package SesAPI;

use strict;
use JSON -no_export;
use Dancer ':syntax';
use UserAPI;

our $VERSION = '0.1';

sub result_ok {
    JSON::to_json({ status => "OK" });
}

sub result_err {
    my ($code,$str) = @_;
    JSON::to_json({ status => "FAIL", error => { code=>$code, str=>$str } });
}

###################################################################################

get '/' => sub {
    my $session = cookies->{session};
    defined $session or return result_err(1, "No session cookie");

    my ($uid,$err) = CallUserAPI($session->value);
    defined $uid or return result_err(2, "$err");

    return result_ok;
};

get '/identity/verify/email' => sub {
    return "TBD";
};

get '/identity/verify/domain' => sub {
    return "TBD";
};

get '/identity/delete' => sub {
    return "TBD";
};

get '/identity/list' => sub {
    return "TBD";
};

post '/send' => sub {
    return "TBD";
};

get '/statistics' => sub {
    return "TBD";
};

get '/smtpcredentials/add' => sub {
    return "TBD";
};

get '/smtpcredentials/list' => sub {
    return "TBD";
};

get '/smtpcredentials/delete' => sub {
    return "TBD";
};

true;

