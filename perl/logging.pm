
use Data::Dumper;

local $_;

=head2 Logger

A package intended to be used as a class representing a logger instance. The Logger is responsible for
receiving debug messages and conditionally printing them, based on the debug level chosen by the user.

=cut

package Logger {

    sub new {
        my $class = shift;
        return bless({-level => 0}, $class);
    }

    sub set_debug_level {
        my $self = shift;
        my $level = shift;
        $self->{-level} = $level;
    }

    sub echo {
        my $self = shift;
        my $level = shift;
        my $str = shift;
        print STDERR "$str\n" if $level <= $self->{-level};
    }

}

my $logger = Logger->new();

=head2 get_logger()

Returns the default Logger instance, which is the only Logger instance that is necessary for most purposes.

=cut

sub get_logger {
    return $logger;
}

1;
