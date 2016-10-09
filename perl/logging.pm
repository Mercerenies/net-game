
use Data::Dumper;

local $_;

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

sub get_logger {
    return $logger;
}

1;
