
local $_;

use Data::Dumper;
use feature 'unicode_strings';
use 5.016;
use List::Util qw(sum);
use POSIX;

=head2 median(...)

Computes the median of a list of numbers. Returns C<undef> if the list is empty.

=cut

sub median {
    return undef unless @_;
    # Copied from http://www.perlmonks.org/?node_id=474564
    sum( ( sort { $a <=> $b } @_ )[ int( $#_/2 ), ceil( $#_/2 ) ] )/2;
}

=head2 evaluate_number($number)

Given a numerical-like string, evaluate the string to produce a number. If the string is already
a valid integer or floating point number (currently, scientific notation is not supported by this
function, due to incompatibilities with the suffix form), it is returned as-is. If the string is
a valid number followed by an order of magnitude suffix (million, billion, etc.), the suffix is
applied and the new value is returned. Commas are allowed as separators in the argument; they will
be removed in the returned number. Any suffix text that is not a known magnitude suffix is ignored.

=cut

sub evaluate_number {
    my $number = $_[0];

    state $suffixes = {
        million  => 1e6,
        billion  => 1e9,
        trillion => 1e12
    };

    my $result = $number;

    $result =~ s/,//g; # Remove comma delimiters
    $result =~ s/[^\d.]*$//; # Remove any non-numerical suffix

    # Apply magnitude suffixes
    for my $key (keys %$suffixes) {
        $result *= $suffixes->{$key} while $number =~ /$key/g;
    }

    return $result;

}

1;
