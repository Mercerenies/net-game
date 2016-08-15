
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
        }
    }

    sub appositive_phrase {
        for (@_) {
            s/,[A-Za-z0-9:\-' _]*,//g;
        }
    }

}

1;
