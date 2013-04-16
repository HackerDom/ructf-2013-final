package Ses::Db;

use strict;
use DBI;
use Ses::Config;

sub DEBUG      {1}
sub SECRET_LEN {20};
my @CHARSET = ('a'..'z','0'..'9');

###################################### SQL ###############################################

my $CREATE_users=<<"SQL";
    CREATE TABLE users (
        id              INTEGER PRIMARY KEY AUTOINCREMENT,
        uid             VARCHAR(20) NOT NULL,
        language        VARCHAR(8)  NOT NULL,
        production      BOOLEAN     NOT NULL  DEFAULT 0,
        mails           BIGINT      NOT NULL  DEFAULT 0,
        bytes           BIGINT      NOT NULL  DEFAULT 0,
        CONSTRAINT uid_unique UNIQUE(uid)
    );
SQL

my $CREATE_credentials=<<"SQL";
    CREATE TABLE credentials (
        id              INTEGER PRIMARY KEY AUTOINCREMENT,
        user            INTEGER NOT NULL,
        login           VARCHAR(32) NOT NULL,
        hash            VARCHAR(64) NOT NULL,
        CONSTRAINT fk_user FOREIGN KEY(user) REFERENCES users(id) ON DELETE CASCADE
    );
SQL

my $CREATE_identities=<<"SQL";
    CREATE TABLE identities (
        id              INTEGER PRIMARY KEY AUTOINCREMENT,
        user            INTEGER NOT NULL,
        email           VARCHAR(64) NOT NULL,
        verified        BOOLEAN     NOT NULL  DEFAULT 0,
        CONSTRAINT fk_user           FOREIGN KEY(user) REFERENCES users(id) ON DELETE CASCADE,
        CONSTRAINT user_email_unique UNIQUE(user,email)
    );
SQL

my $CREATE_verification=<<"SQL";
    CREATE TABLE verification (
        id              INTEGER PRIMARY KEY AUTOINCREMENT,
        secret          VARCHAR(20) NOT NULL,
        identity        INTEGER     NOT NULL,
        CONSTRAINT fk_identity FOREIGN KEY(identity) REFERENCES identities(id) ON DELETE CASCADE
    );
SQL

#################################### Methods #############################################

sub new {
    my $class = shift;
    my $dbname = sprintf "%s/ses.sqlite", CFG_DB_DIR;
    my $create = !-f $dbname;
    my $self = {};
    $self->{db} = DBI->connect("dbi:SQLite:dbname=$dbname","","") or die "Fatal error: DBI->connect failed";
    bless $self, $class;
    $self->createTables() if $create;
    return $self;
}

sub createTables {
    my $self = shift;
    print "Creating tables ... \n";
    $self->{db}->do($CREATE_users);
    $self->{db}->do($CREATE_credentials);
    $self->{db}->do($CREATE_identities);
    $self->{db}->do($CREATE_verification);
    print "Creating tables finished.\n";
}

sub addUser {
    my ($self,$uid,$lang) = @_;
    my $rows = $self->{db}->do("INSERT INTO users(uid,language) VALUES(?,?)",undef,$uid,$lang);
    printf "INSERT INTO users (uid='$uid', lang='$lang'): %s\n", $rows>0 ? "Success" : "Failure" if DEBUG;
}

sub findUser {
    my ($self,$key,$val) = @_;
    $key=~s/[^a-z]//g;
    my $st = $self->{db}->prepare("SELECT * FROM users WHERE $key=?");
    $st->execute($val) or die $self->{db}->errstr;
    return $st->fetchrow_hashref();
}

sub updateUser {
    my ($self,$user) = @_;
    my $rows = $self->{db}->do("UPDATE users SET uid=?, production=?, mails=?, bytes=? WHERE id=?",undef,
        $user->{uid}, $user->{production}, $user->{mails}, $user->{bytes}, $user->{id}
    );
    printf "UPDATE users (id='%s'): %s\n", $user->{id}, $rows>0 ? "Success" : "Failure" if DEBUG;
}

sub addCounters {
    my ($self,$user,$mails,$bytes) = @_;
    my $rows = $self->{db}->do("UPDATE users SET mails=mails+?, bytes=bytes+? WHERE id=?", undef, $mails, $bytes, $user->{id});
    printf "UPDATE users (id='%s'): %s\n", $user->{id}, $rows>0 ? "Success" : "Failure" if DEBUG;
}

sub addIdentity {
    my ($self,$user,$email) = @_;
    my $rows = $self->{db}->do("INSERT INTO identities(user,email) VALUES(?,?)",undef,$user->{id},$email);
    printf "INSERT INTO identities(user='%s', email='%s'): %s\n", $user->{id}, $email, $rows>0 ? "Success" : "Failure" if DEBUG;
    return $rows>0;
}

sub delIdentity {
    my ($self,$user,$id) = @_;
    my $rows = $self->{db}->do("DELETE FROM identities WHERE id=? AND user=?",undef,$id,$user->{id});
    printf "DELETE FROM identities(id='%s', user='%s'): %s\n", $id, $user->{id}, $rows>0 ? "Success" : "Failure" if DEBUG;
    return $rows>0;
}

sub findIdentity {
    my ($self,$user,$email) = @_;
    my $rows = $self->{db}->selectrow_arrayref("SELECT COUNT(*) FROM identities WHERE user=? AND email=?",
            undef,$user->{id},$email)->[0];
    printf "SELECT COUNT(*) FROM identities: $rows\n" if DEBUG;
    return $rows;
}

sub getAllCredentials {
    my ($self,$user) = @_;
    my @result;
    my $st = $self->{db}->prepare("SELECT id,login FROM credentials WHERE user=?");
    $st->execute($user->{id}) or die $self->{db}->errstr;
    while (my $row = $st->fetchrow_hashref()) {
        push @result, $row;
    }
    return @result;
}

sub addCredentials {
    my ($self,$user,$login,$hash) = @_;
    my $rows = $self->{db}->do("INSERT INTO credentials(user,login,hash) VALUES(?,?,?)",undef,$user->{id},$login,$hash);
    printf "INSERT INTO credentials(user='%s', login='%s', hash='%s'): %s\n",
            $user->{id}, $login, $hash, $rows>0 ? "Success" : "Failure" if DEBUG;
    return $rows>0;
}

sub delCredentials {
    my ($self,$user,$id) = @_;
    my $rows = $self->{db}->do("DELETE FROM credentials WHERE id=? AND user=?",undef,$id,$user->{id});
    printf "DELETE FROM credentials(id='%s', user='%s'): %s\n", $id, $user->{id}, $rows>0 ? "Success" : "Failure" if DEBUG;
    return $rows>0;
}

sub authenticate {
    my ($self,$login,$hash) = @_;
    my $rows = $self->{db}->selectrow_arrayref("SELECT COUNT(*) FROM credentials WHERE login=? AND hash=?",undef,$login,$hash)->[0];
    printf "SELECT COUNT(*) FROM credentials: $rows\n" if DEBUG;
    return $rows;
}

sub getAllIdentities {
    my ($self,$user) = @_;
    my @result;
    my $st = $self->{db}->prepare("SELECT * FROM identities WHERE user=?");
    $st->execute($user->{id}) or die $self->{db}->errstr;
    while (my $row = $st->fetchrow_hashref()) {
        push @result, $row;
    }
    return @result;
}

sub createVerification {
    my ($self,$identity) = @_;
    my $secret = generateSecret();
    my $rows = $self->{db}->do("INSERT INTO verification(secret,identity) VALUES(?,?)",undef,$secret,$identity->{id});
    printf "INSERT INTO verification(secret='%s', identity='%s'): ", $secret, $identity->{id} if DEBUG;
    if ($rows > 0) {
        print "Success\n" if DEBUG;
        return $secret;
    }
    else {
        print "Failure\n" if DEBUG;
        return undef;
    }
}

sub verifyIdentity {
    my ($self,$secret) = @_;
    my ($id) = $self->{db}->selectrow_array("SELECT identity FROM verification WHERE secret=?",undef,$secret);
    if (!defined $id) {
        print "verifyIdentity(secret='$secret'): Failure (no such secret)\n" if DEBUG;
        return undef;
    }
    my $rows = $self->{db}->do("UPDATE identities SET verified=1 WHERE id=?",undef,$id);
    printf "UPDATE identities (id='%s'): %s\n", $id, $rows>0 ? "Success" : "Failure" if DEBUG;
    my $rows = $self->{db}->do("DELETE FROM verification WHERE identity=?",undef,$id);
    printf "DELETE FROM verification: %d rows\n", $rows if DEBUG;
}

sub close {
    my $self = shift;
    undef $self->{db};
}

#################################### Helpers #############################################

sub generateSecret {
    join '', map { $CHARSET[int rand @CHARSET] } 1..SECRET_LEN;
}

1;

