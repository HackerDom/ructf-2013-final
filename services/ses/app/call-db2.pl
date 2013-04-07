#!/usr/bin/perl
use strict;
use Ses::Db;

my $db = new Ses::Db;
$db->addUser("uiduiduiuid23");

my $user;

#$user = $db->findUser(uid => "uiduiduiuid");
$user = $db->findUser(id => 3);
printf  "  Found user: id=%s, uid=%s, mails=%s\n", $user->{id}, $user->{uid}, $user->{mails};

#$user->{mails} ++;
#$db->updateUser($user);

#$db->addIdentity($user, sprintf('dimmoborgir_%d@gmail.com', localtime()));
my @ids = $db->getAllIdentities($user);
for (@ids) {
        printf "  id = %3d: %s %s\n", $_->{id}, $_->{email}, $_->{verified};
#        $db->createVerification($_);
}

