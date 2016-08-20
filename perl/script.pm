
use perl::filters;
use perl::sentence;
use perl::navigation;

use Data::Dumper;

my $LINKVERB = "(?:is|was|are|were)";
my $ARTICLE = "(?:an? |the )";

# ///// Refactor this file into something much less messy... Good luck :)

local $_;

# find_occu($title, $summary, %occu);
sub find_occu {
    local $_;
    my $title = $_[0];
    my $summary = $_[1];
    my @titles = apply_filters(
        [
         \&Filters::paren_expr,
         \&Filters::trailing_comma_phrase
        ],
        $title
        );
    my @summaries = apply_filters(
        [
         \&Filters::paren_expr,
         \&Filters::slash_phrase,
         \&Filters::appositive_phrase,
         \&Filters::quoted_phrase,
         sub { for (@_) { s/"//g; } },
         sub { for (@_) { s/,//g; } }
        ],
        $summary
        );
    Filters::consecutive_spaces(@titles);
    Filters::consecutive_spaces(@summaries);
    my %occu = %{$_[2]};
    my @res = ();
  KEYWORD: foreach my $o (keys %occu) {
      foreach my $titlevar (@titles) {
          my $expr = simple_linked_sentence($titlevar, $o, {MiddleNameRule => 1});
          my @arr = ($occu{$o}, $o);
          foreach my $summaryvar (@summaries) {
              if ($summaryvar =~ $expr) {
                  push @res, \@arr;
                  next KEYWORD;
              }
          }
      }
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
    # TODO Currently this counts the number of distinct pronouns that match; should we change it to count
    #      all matches? The gender identification works pretty well right now.
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
    my @titles = apply_filters(
        [
         \&Filters::trailing_comma_phrase,
         \&Filters::paren_expr,
         sub { for (@_) { s/(Greater|Lesser) +//; } }
        ],
        $title
        );
    my @summaries = apply_filters(
        [
         \&Filters::paren_expr,
         \&Filters::slash_phrase,
         \&Filters::appositive_phrase
        ],
        $summary
        );
    Filters::consecutive_spaces(@titles);
    Filters::consecutive_spaces(@summaries);
    my @res = ();
    my $ptn;
  PATTERN: foreach $ptn (keys %placenames) {
      foreach my $titlevar (@titles) {
          my $expr = simple_linked_sentence($titlevar, $ptn, {MoreRenameClauses => 1});
          foreach my $summaryvar (@summaries) {
              if ($summaryvar =~ $expr) {
                  push @res, [$placenames{$ptn}, $ptn];
                  next PATTERN;
              }
          }
      }
  }
  PATTERN: foreach $ptn (keys %placenames) {
      foreach my $titlevar (@titles) {
          if ($titlevar =~ /\b$ptn\b/i && not $titlevar =~ /^$ptn$/i) {
              push @res, [$placenames{$ptn}, $ptn];
              next PATTERN;
          }
      }
  }
    return \@res;
}

# find_weapon_information($title, $summary, %weapons)
sub find_weapon_information {
    my $title = $_[0];
    my $summary = $_[1];
    my %weapons = %{$_[2]};
    my $shortsumm = $summary;
    $title =~ s/-/ /;
    $summary =~ s/-/ /;
    my @titles = apply_filters(
        [
         \&Filters::trailing_comma_phrase,
         \&Filters::paren_expr,
        ],
        $title
        );
    my @summaries = apply_filters(
        [
         \&Filters::paren_expr,
         \&Filters::slash_phrase,
         sub { for (@_) { s/"//g; } },
         \&Filters::appositive_phrase
        ],
        $summary
        );
    Filters::consecutive_spaces(@titles);
    Filters::consecutive_spaces(@summaries);
    my @res;
    my $ptn;
  PATTERN: foreach $ptn (keys %weapons) {
      foreach my $titlevar (@titles) {
          my $expr = simple_linked_sentence($titlevar, $ptn, {MoreRenameClauses => 0});
          for my $summaryvar (@summaries) {
              if ($summaryvar =~ $expr) {
                  push @res, [$weapons{$ptn}, $ptn];
                  next PATTERN;
              }
          }
      }
  }
  PATTERN: foreach $ptn (keys %weapons) {
      foreach my $titlevar (@titles) {
          if ($titlevar =~ /\b$ptn\b/i && not $titlevar =~ /^$ptn$/i) {
              push @res, [$weapons{$ptn}, $ptn];
              next PATTERN;
          }
      }
  }
    return \@res;
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
    $title =~ s/-/ /;
    $summary =~ s/-/ /;
    Filters::paren_expr($title); # TODO Make the synonym checker use @titles like the other systems
    my @summaries = apply_filters(
        [
         \&Filters::paren_expr,
         \&Filters::slash_phrase,
         sub { for (@_) { s/"//g; } },
         \&Filters::paren_expr,
         \&Filters::appositive_phrase
        ],
        $summary
        );
    Filters::consecutive_spaces($title);
    Filters::consecutive_spaces(@summaries);
    my @candidates;
    push @candidates, $title;
    foreach my $prefix_loop (@{$data->{'foodprefixes'}}) {
        my $prefix = $prefix_loop;
        $prefix =~ s/\$title/$title/g;
        foreach my $suffix_loop (@{$data->{'foodsuffixes'}}) {
            my $suffix = $suffix_loop;
            $suffix =~ s/\$title/$title/g;
            for my $summaryvar (@summaries) {
                if ($summaryvar =~ /\b$prefix (?:the )?([\w ]+)\b$suffix\W/) {
                    push @candidates, $1;
                }
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
