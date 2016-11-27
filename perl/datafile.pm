
use Data::Dumper;

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

=head2 load_keyword_search_file($fh, $fname)

Loads the file using a keyword attribute format. Each line of the file should be a keyword, followed by
the numerical stats that it influences and by how much it influences them. For example,

 keyword-here: attribute1 +2, attribute2 -2

In this exampe, every instance of the keyword "keyword-here" would add two points to attribute1 and subtract
two points from attribute2.

=cut

sub load_keyword_search_file {
    local $_;
    my $fh = $_[0];
    my $fname = $_[1];
    my %result;
    while (<$fh>) {
        chomp;
        /^([\w\- ]+)+: (.*)$/ or die("Illegal line in $fname at line $.");
        my $key = $1;
        my @rest = split(/,/, $2);
        my %stats;
        foreach my $token (@rest) {
            $token =~ /\b(\w+) *([-+]\d+)/ or die("Illegal line in $fname at line $.");
            $stats{$1} = 0+ $2;
        }
        $result{$key} = \%stats;
    }
    return %result;
}

package OData {

    sub new {
        my $class = shift;
        return bless({-occu => undef,
                      -mwords => undef,
                      -fwords => undef,
                      -placenames => undef,
                      -weapons => undef,
                      -animals => undef,
                      -foodprefixes => undef,
                      -foodnegatives => undef,
                      -foodblacklist => undef,
                      -foodsuffixes => undef,
                      -foodtrees => undef,
                      -foodnutrition => undef,
                      -foodpoison => undef,
                      -foodsections => undef,
                      -monsters => undef}, $class);
    }

    sub occupations {
        my $self = shift;
        return %{$self->{-occu}};
    }

    sub male_words {
        my $self = shift;
        return @{$self->{-mwords}};
    }

    sub female_words {
        my $self = shift;
        return @{$self->{-fwords}};
    }

    sub placenames {
        my $self = shift;
        return %{$self->{-placenames}};
    }

    sub weapons {
        my $self = shift;
        return %{$self->{-weapons}};
    }

    sub animals {
        my $self = shift;
        return %{$self->{-animals}};
    }

    sub food_prefixes {
        my $self = shift;
        return @{$self->{-foodprefixes}};
    }

    sub food_suffixes {
        my $self = shift;
        return @{$self->{-foodsuffixes}};
    }

    sub food_blacklist {
        my $self = shift;
        return @{$self->{-foodblacklist}};
    }

    sub food_negatives {
        my $self = shift;
        return @{$self->{-foodnegatives}};
    }

    sub food_sections {
        my $self = shift;
        return @{$self->{-foodsections}};
    }

    sub food_nutrition {
        my $self = shift;
        return @{$self->{-foodnutrition}};
    }

    sub food_poison {
        my $self = shift;
        return @{$self->{-foodpoison}};
    }

    sub food_trees {
        my $self = shift;
        return %{$self->{-foodtrees}};
    }

    sub monster_types {
        my $self = shift;
        return %{$self->{-monsters}};
    }

    sub monster_affinities {
        my $self = shift;
        return %{$self->{-monstertypes}};
    }

}

sub data_compile {
    my %data = %{$_[0]};
    my $result = OData->new();
    $result->{-occu} = $data{'occu'};
    $result->{-mwords} = $data{'mwords'};
    $result->{-fwords} = $data{'fwords'};
    $result->{-placenames} = $data{'placenames'};
    $result->{-weapons} = $data{'weapons'};
    $result->{-animals} = $data{'animals'};
    $result->{-foodprefixes} = $data{'foodprefixes'};
    $result->{-foodnegatives} = $data{'foodnegatives'};
    $result->{-foodblacklist} = $data{'foodblacklist'};
    $result->{-foodsuffixes} = $data{'foodsuffixes'};
    $result->{-foodtrees} = $data{'foodtrees'};
    $result->{-foodnutrition} = $data{'foodnutrition'};
    $result->{-foodpoison} = $data{'foodpoison'};
    $result->{-foodsections} = $data{'foodsections'};
    $result->{-monsters} = $data{'monsters'};
    $result->{-monstertypes} = $data{'monstertypes'};
    return $result;
}

1;
