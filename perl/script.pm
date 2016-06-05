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
    $shortsumm =~ s|/[^ /]+/||g;
    my $longsumm = $shortsumm;
    $shortsumm =~ s/,[A-Za-z0-9:\-' _]*,//g;
    $shortsumm =~ s/ {2,}/ /g;
    $longsumm =~ s/ {2,}/ /g;
    my $ptn;
    my @titles = ("$title", "$title", "$title", "$title");
    $titles[1] =~ s/,.*$//;
    $matches = 1;
    $matches = $titles[2] =~ s/ ?\([^()]*\) ?//g while $matches > 0;
    $titles[3] =~ s/(Greater|Lesser) +//;
    foreach $ptn (keys %placenames) {
        foreach my $titlevar (@titles) {
            my $expr = qr/$titlevar (?:(or|in) (?:[\w-]+ ){1,3})?$LINKVERB (?:[\w-]+ )?$ARTICLE?(?:[^ ]+ ){0,9}\b$ptn\b/i;
            if ($shortsumm =~ $expr || $summary =~ $expr || $longsumm =~ $expr) {
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
    $shortsumm =~ s|/[^ /]+/||g;
    $shortsumm =~ s/"//g;
    my $longsumm = $shortsumm;
    $shortsumm =~ s/,[A-Za-z0-9:\-' _]*,//g;
    $shortsumm =~ s/ {2,}/ /g;
    $longsumm =~ s/ {2,}/ /g;
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
            my $expr = qr/$titlevar (?:or (?:[\w-]+ ){1,3})?$LINKVERB (?:[\w-]+ )?$ARTICLE?(?:[^ ]+ ){0,9}\b$ptn\b/i;
            if ($shortsumm =~ $expr || $summary =~ $expr || $longsumm =~ $expr) {
                return $ptn;
            }
        }
    }
    return undef;
}

# deduce_animal_stats($title, $summary, $data)
sub deduce_animal_stats {
    my $title = $_[0];
    my $summary = $_[1];
    my $data = $_[2];
    my %stats;
    foreach my $keyword (keys %{$data->{'animals'}}) {
        my $constant;
        if ($title =~ /\b$keyword\b/i) {
            $constant = 4;
        } else {
            $constant = @{[ $summary =~ /\b$keyword\b/gi ]};
        }
        $stats{'matches'} += $constant;
#        print STDERR "$title has $keyword match $constant times\n" if $constant > 0;
        foreach my $stat (keys %{$data->{'animals'}->{$keyword}}) {
            my $coef = $data->{'animals'}->{$keyword}->{$stat};
            $stats{$stat} += $coef * $constant;
        }
   }
    return \%stats;
}

# shortest_food_synonym($title, $summary, $data)
sub shortest_food_synonym {
    my $title = $_[0];
    my $summary = $_[1];
    my $data = $_[2];
    my $shortsumm = $summary;
    my $matches = 1;
    $matches = $title =~ s/\([^()]*\)//g while $matches > 0;
    $shortsumm =~ s|/[^ /]+/||g;
    $shortsumm =~ s/"//g;
    my $longsumm = $shortsumm;
    #$shortsumm =~ s/,[A-Za-z0-9:\-' _]*,//g;
    $matches = 1;
    $matches = $shortsumm =~ s/\([^()]*\)//g while $matches > 0;
    $shortsumm =~ s/ {2,}/ /g;
    $longsumm =~ s/ {2,}/ /g;
    $title =~ s/-/ /;
    $shortsumm =~ s/-/ /;
    my @candidates;
    push @candidates, $title;
    foreach my $prefix_loop (@{$data->{'foodprefixes'}}) {
        my $prefix = $prefix_loop;
        $prefix =~ s/\$title/$title/g;
        foreach my $suffix_loop (@{$data->{'foodsuffixes'}}) {
            my $suffix = $suffix_loop;
            $suffix =~ s/\$title/$title/g;
            if (($shortsumm =~ /\b$prefix (?:the )?([\w ]+)$suffix\W/) or
                ($longsumm =~ /\b$prefix (?:the )?([\w ]+)$suffix\W/)) {
                push @candidates, $1;
            }
        }
    }
    my $shortest = $title;
  EXCLUDE: foreach my $candidate (@candidates) {
      foreach my $word (@{$data->{'foodblacklist'}}) {
          next EXCLUDE if $candidate =~ /\b$word\b/i;
      }
      foreach my $word (@{$data->{'foodnegatives'}}) {
          next EXCLUDE if $candidate =~ /^\s*$word\s*$/i;
      }
      $shortest = $candidate if length $candidate < length $shortest;
  }
    $shortest =~ s/^\s+|\s+$//g;
    return $shortest;
}

# get_plant_type($title, $summary, $data)
sub get_plant_type {
    my $title = $_[0];
    my $summary = $_[1];
    my $data = $_[2];
    my $max = undef;
    my $max_num = 0; # Set a threshold so that if nothing matches, we don't falsely identify as something
    foreach my $curr (keys %{$data->{'foodtrees'}}) {
        my $constant = 0;
        foreach my $key (@{$data->{'foodtrees'}->{$curr}}) {
            $constant += 8 if ($title =~ /\b$key\b/i);
            $constant += @{[ $summary =~ /\b$key\b/gi ]};
        }
        if ($constant > $max_num) {
            $max = $curr;
            $max_num = $constant;
        }
    }
    return $max;
}

1;
