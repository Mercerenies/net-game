
use Data::Dumper;

my $LINKVERB = "(?:is|was|are|were)";
my $ARTICLE = "(?:an? |the )";

# ///// Refactor this file into something much less messy... Good luck :)

local $_;

# find_occu($summary, %occu);
sub find_occu {
    local $_;
    my $summary = $_[0];
    my %occu = %{$_[1]};
    my @res = ();
    foreach my $o (keys %occu) {
        my @arr = ($occu{$o}, $o);
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
            my $expr = qr/\Q$titlevar\E (?:(or|in|of) (?:[\w-]+ ){1,3})?$LINKVERB (?:[\w-]+ )?$ARTICLE?(?:[^ ]+ ){0,9}\b$ptn\b/i;
            if ($shortsumm =~ $expr || $summary =~ $expr || $longsumm =~ $expr) {
                return [$placenames{$ptn}, $ptn];
            }
        }
    }
    foreach $ptn (keys %placenames) {
        foreach my $titlevar (@titles) {
            if ($titlevar =~ /\b$ptn\b/i && not $titlevar =~ /^$ptn$/i) {
                return [$placenames{$ptn}, $ptn];
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
            my $expr = qr/\Q$titlevar\E (?:or (?:[\w-]+ ){1,3})?$LINKVERB (?:[\w-]+ )?$ARTICLE?(?:[^ ]+ ){0,9}\b$ptn\b/i;
            if ($shortsumm =~ $expr || $summary =~ $expr || $longsumm =~ $expr) {
                return [$weapons{$ptn}, $ptn];
            }
        }
    }
    foreach $ptn (keys %weapons) {
        foreach my $titlevar (@titles) {
            if ($titlevar =~ /\b$ptn\b/i && not $titlevar =~ /^$ptn$/i) {
                return [$weapons{$ptn}, $ptn];
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
        #print STDERR "$title has $keyword match $constant times\n" if $constant > 0;
        foreach my $stat (keys %{$data->{'animals'}->{$keyword}}) {
            my $coef = $data->{'animals'}->{$keyword}->{$stat};
            $stats{$stat} += $coef * $constant;
        }
   }
    return \%stats;
}

# normalize_animal_stats(%stats)
sub normalize_animal_stats {
    my %stats = %{$_[0]};
    # $threat, $size, $pack, $speed are on a scale of 1 to 5
    # $air, $sea are booleans
    my($threat, $size, $pack, $speed, $air, $sea);
    $threat = 5;
    $threat -=!! ($stats{'threat'} <=  9);
    $threat -=!! ($stats{'threat'} <=  4);
    $threat -=!! ($stats{'threat'} <=  2);
    $threat -=!! ($stats{'threat'} <= -1);
    $size = 5;
    $size -=!! ($stats{'size'} <= 10);
    $size -=!! ($stats{'size'} <=  8);
    $size -=!! ($stats{'size'} <=  4);
    $size -=!! ($stats{'size'} <= -4);
    $pack = 5;
    $pack -=!! ($stats{'pack'} <= 3);
    $pack -=!! ($stats{'pack'} <= 2);
    $pack -=!! ($stats{'pack'} <= 1);
    $pack -=!! ($stats{'pack'} <= 0);
    $speed = 5;
    $speed -=!! ($stats{'speed'} <= 10);
    $speed -=!! ($stats{'speed'} <=  6);
    $speed -=!! ($stats{'speed'} <=  0);
    $speed -=!! ($stats{'speed'} <= -2);
    $sea = 0+!! ($stats{'sea'} > 1);
    $air = 0+!! ($stats{'air'} > 1);
    return (
        threat => $threat,
        size => $size,
        pack => $pack,
        speed => $speed,
        sea => \$sea,
        air => \$air
        );
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
            if (($shortsumm =~ /\b$prefix (?:the )?([\w ]+)\b$suffix\W/) or
                ($longsumm =~ /\b$prefix (?:the )?([\w ]+)\b$suffix\W/)) {
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

# get_plant_type($title, $summary, $trees)
sub get_plant_type {
    my $title = $_[0];
    my $summary = $_[1];
    my %trees = %{$_[2]};
    my $max = undef;
    my $max_num = 0; # Set a threshold so that if nothing matches, we don't falsely identify as something
    foreach my $curr (keys %trees) {
        my $constant = 0;
        foreach my $key (@{$trees{$curr}}) {
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

# get_nutrition_information($title, %page, $data)
sub get_nutrition_information {
    my $title = $_[0];
    my %page = %{$_[1]};
    my $data = $_[2];
    my %result = ( 'nutrition' => 0, 'poison' => 0 );
    my %sections = %{flatten_sections(\%page)};
    foreach my $section (keys %sections) {
        my $okay = 0;
        foreach my $keyword (@{$data->{'foodsections'}}) {
            $keyword =~ s/\$title/$title/g;
            if ($section =~ /\b$keyword\b/i) {
                $okay = 1;
                last;
            }
        }
        next unless $okay;
        my $summary = $sections{$section};
        foreach my $curr (@{$data->{'foodnutrition'}}) {
            my $constant = 0;
            $constant += @{[ $summary =~ /\b$curr\b/gi ]};
            $result{'nutrition'} += $constant;
        }
        foreach my $curr (@{$data->{'foodpoison'}}) {
            my $constant = 0;
            $constant += @{[ $summary =~ /\b$curr\b/gi ]};
            $result{'poison'} += $constant;
        }
    }
    return \%result;
}

1;
