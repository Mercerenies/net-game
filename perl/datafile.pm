
local $_;

=head2 load_two_column_file($fh, $fname)

Loads the file using a two-column format. Each line should be two columns of text, separated by at least
two consecutive spaces. The text entries should consist of word characters, nonconsecutive spaces, or
hyphens.

=cut

sub load_two_column_file {
    local $_;
    my $fh = $_[0];
    my $fname = $_[1];
    my %result;
    while (<$fh>) {
        chomp;
        /^((?:[\w\-]+ )+) +([\w ]+)$/ or die("Illegal line in $fname at line $.");
        my $key = $1;
        chop $key;
        $result{$key} = $2;
    }
    return %result;
}

1;
