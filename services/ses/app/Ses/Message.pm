package Ses::Message;

use strict;
use DBI;
use Date::Format;
use Ses::Config;
use Ses::Utils;

sub ID_LEN     {10}
sub LOCK_TRIES {4}
sub LOCK_SLEEP {1}

#################################### Methods #############################################

sub new {
    my ($class,$id) = @_;
    my $self = {};
    $self->{isNew} = length($id)==0;
    $id = randomStr(ID_LEN) if $self->{isNew};
    $self->{id} = $id;
    $self->{fnameMsg} = sprintf "%s/%s.msg", CFG_QUEUE_DIR, $id;
    $self->{dirLock}  = sprintf "%s/%s.lck", CFG_QUEUE_DIR, $id;
    bless $self, $class;
    return $self;
}

sub read {
    my $self = shift;
    $self->lock();
    open F, $self->{fnameMsg} or die "Error: Message.read() failed for '%s': $!\n", $self->{fnameMsg};
    sysread F, my $data, -s F;
    close F;
    $self->unlock();
    return $data;
}

sub writeRaw {
    my ($self,$data) = @_;
    $self->lock();
    printf "Write to: %s ... (data: %d bytes)\n", $self->{fnameMsg}, length($data);
    open F, '>', $self->{fnameMsg} or die "Error: Message.write() failed for '%s': $!\n", $self->{fnameMsg};
    printf F "MAIL FROM: %s\n", $self->{from};
    printf F "RCPT TO: %s\n",   $self->{to};
    print F $data;
    close F;
    $self->unlock();
}

sub writeMessage {
    my ($self,$msg,$subject) = @_;
    $self->lock();
    printf "Write to: %s ... \n", $self->{fnameMsg};
    open F, '>', $self->{fnameMsg} or die "Error: Message.write() failed for '%s': $!\n", $self->{fnameMsg};
    printf F "MAIL FROM: <%s>\n", $self->{from};
    printf F "RCPT TO: <%s>\n",   $self->{to};
    printf F "Message-ID: <%s\@ses>\n", $self->{id};
    printf F "Date: %s\n",     time2str("%a, %d %h %Y %H:%M:%S %z",time);
    printf F "From: %s\n",     $self->{from};
    printf F "To: %s\n",       $self->{to};
    printf F "Subject: %s\n",  $subject;
    print  F "\n$msg";
    close F;
    $self->unlock();

}

sub lock {
    my $self = shift;
    for my $try (1..LOCK_TRIES) {
        mkdir $self->{dirLock} or do { sleep LOCK_SLEEP; next };
        return 1;
    }
    warn sprintf "Error: Message.lock() failed for '%s'\n", $self->{fnameMsg};
    return undef;
}

sub unlock {
    my $self = shift;
    rmdir $self->{dirLock} and return 1;
    warn sprintf "Error: Message.unlock() failed for '%s'\n", $self->{fnameMsg};
    return undef;
}

1;

