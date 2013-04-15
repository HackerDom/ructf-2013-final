package Ses::I18N::en;
use base 'Ses::I18N';

%Lexicon = (
    '_AUTO' => 1,
    1 => 'UserAPI auth error',
    2 => 'Cannot find user in DB after creation',
    3 => 'Bad JSON',
    4 => 'Parameter "email" not specified',
    5 => 'Invalid "email" value',
    6 => 'Failed',
    7 => 'Parameter "id" not specified',
    8 => 'Invalid "id" value',
    9 => 'One of parameters not specified: "from", "to", "message", "subject"',
    10 => 'Invalid value in one of: "from", "to", "message", "subject"',
    11 => 'Forbidden. You cannot send emails from this identity/email',
);

1;

