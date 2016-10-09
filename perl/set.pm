
=begin

A very simple wrapper to make structures that behave like finite unordered sets.

=cut

sub set_containing {
    my %hash;
    @hash{@_} = (1) x @_; # The value `1' is arbitrary; it just needs to be a defined, truthy value.
    return %hash;
}

sub set_contains {
    my $hash = shift;
    my $value = shift;
    return defined $hash->{$value};
}

1;
