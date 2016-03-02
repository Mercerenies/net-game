
use Data::Dumper;

my $LINKVERB = "(?:is|was|are|were)";
my $ARTICLE = "(?:an? |the )";

local $_;

# crop($x);
sub crop {
    $_[0] =~ s/b'(.*)'/$1/;
    $_[0] =~ s/b"(.*)"/$1/;
}

# find_occu($summary, %occu);
sub find_occu {
    local $_;
    my $summary = $_[0];
    my %occu = %{$_[1]};
    my @res = ();
    foreach my $o (keys %occu) {
        my @arr = ($o, $occu{$o});
        push @res, \@arr if ($summary =~ /\b$o\b/i);
    }
    return @res;
}

# compute_gender($summary, @mwords, @fwords);
sub compute_gender {
    local $_;
    my $summary = $_[0];
    my @mwords = @{$_[1]};
    my @fwords = @{$_[2]};
    my $male = 0;
    my $female = 0;
    my $ptn;
    foreach $ptn (@mwords) {
        $male++ if ($summary =~ /\b$ptn\b/i);
    }
    foreach $ptn (@fwords) {
        $female++ if ($summary =~ /\b$ptn\b/i);
    }
    return undef if ($male == 0 and $female == 0);
    return ($male > $female) ? "male" : "female";
}

# find_place_information($title, $summary, %placenames)
sub find_place_information {
    my $title = $_[0];
    my $summary = $_[1];
    my %placenames = %{$_[2]};
    my $shortsumm = $summary;
    my $matches = 1;
    $matches = $shortsumm =~ s/\([^()]*\)//g while $matches > 0;
    $shortsumm =~ s/,[A-Za-z0-9:\-' _]*,//g;
    $shortsumm =~ s|/[^ /]+/||g;
    $shortsumm =~ s/ {2,}/ /g;
    my $ptn;
    my @titles = ("$title", "$title", "$title", "$title");
    $titles[1] =~ s/,.*$//;
    $matches = 1;
    $matches = $titles[2] =~ s/ ?\([^()]*\) ?//g while $matches > 0;
    $titles[3] =~ s/(Greater|Lesser) +//;
    foreach $ptn (keys %placenames) {
        foreach my $titlevar (@titles) {
            if ($shortsumm =~
                  /$titlevar (?:(or|in) (?:[\w-]+ ){1,3})?$LINKVERB (?:[\w-]+ )?$ARTICLE?(?:[^ ]+ ){0,9}\b$ptn\b/i) {
                return $ptn;
            }
        }
    }
    return undef;
}

# find_weapon_information($title, $summary, %weapons)
sub find_weapon_information {
    my $title = $_[0];
    my $summary = $_[1];
    my %weapons = %{$_[2]};
    my $shortsumm = $summary;
    my $matches = 1;
    $matches = $shortsumm =~ s/\([^()]*\)//g while $matches > 0;
    $shortsumm =~ s/,[A-Za-z0-9:\-' _]*,//g;
    $shortsumm =~ s|/[^ /]+/||g;
    $shortsumm =~ s/"//g;
    $shortsumm =~ s/ {2,}/ /g;
    my $ptn;
    $title =~ s/-/ /;
    $shortsumm =~ s/-/ /;
    my @titles = ("$title", "$title", "$title", "$title");
    $titles[1] =~ s/,.*$//;
    $matches = 1;
    $matches = $titles[2] =~ s/ ?\([^()]*\) ?//g while $matches > 0;
    #$titles[3] = $1 if ($titles[3] =~ /(\w+)$/);
    foreach $ptn (keys %weapons) {
        foreach my $titlevar (@titles) {
            if ($shortsumm =~
                  /$titlevar (?:or (?:[\w-]+ ){1,3})?$LINKVERB (?:[\w-]+ )?$ARTICLE?(?:[^ ]+ ){0,9}\b$ptn\b/i) {
                return $ptn;
            }
        }
    }
    return undef;
}

1;
