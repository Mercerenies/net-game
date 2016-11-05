
=head2 Filters

This package is dedicated to several simple regex filters, to be applied to names and texts using apply_filter
and associated functions.

=cut

package Filters {

    sub trailing_comma_phrase {
        for (@_) { s/,.*$//; }
    }

    sub slash_phrase {
        for (@_) { s|/[^ /]/||g; }
    }

    sub paren_expr {
        my $matches;
        for (@_) {
            do { $matches = s/\([^()]*\)//g; } while $matches > 0;
        }
    }

    sub consecutive_spaces {
        for (@_) {
            s/ {2,}/ /g;
            s/^ +//;
            s/ +$//;
            s/ +,/,/g;
        }
    }

    sub appositive_phrase {
        for (@_) {
            s/,[A-Za-z0-9:\-' _"]*,//g;
        }
    }

    sub quoted_phrase {
        for (@_) {
            s/"[A-Za-z0-9:\-' _,.]*"//g;
        }
    }

    sub people_titles {
        for (@_) {
            s/(Jr\.?|Sr\.|CC|[CKDGOM]BE)//g
        }
    }

}

=head2 apply_filter($func, $elems...)

Apply the filter given by $func to each of the arguments supplied, producing an array twice as big as the list
of arguments passed in, containing (in an unspecified order) all of the original elements plus the filtered
elements.

=cut

sub apply_filter {
    my $func = shift;
    my @arr;
    for (@_) {
        my $arg = $_;
        $func->($arg);
        push @arr, ($_, $arg);
    }
    return @arr;
}

=head2 apply_filters($funcs, $elems...)

Apply the collection of filters given in the first argument to the elements, producing a list of all possible
combinations of filters applied to elements.

=cut

sub apply_filters {
    my $funcs = shift;
    my @arr = @_;
    for my $func (@$funcs) {
        @arr = apply_filter($func, @arr);
    }
    return @arr;
}

1;
