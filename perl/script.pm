
use perl::filters;
use perl::sentence;
use perl::navigation;
use perl::numbers;

use feature 'unicode_strings';
use Data::Dumper;

my $LINKVERB = "(?:is|was|are|were)";
my $ARTICLE = "(?:an? |the )";

local $_;

=head2 find_occu($title, $summary, $xdata)

Given the name of a person and the page summary for that person, attempts to determine the person's occupation.
The return value is a list of the possible occupations, with each occupation formatted as a pair of elements
of the form C<["keyword", "friendly_name"]>. For example, the following is one possible result of
C<find_occu>.

 [["musician", "singer"], ["actor", "actress"], ["musician", "songwriter"]]

=cut

sub find_occu {
    local $_;
    my $title = $_[0];
    my $summary = $_[1];
    my $xdata = $_[2];
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
         \&Filters::people_titles,
         sub { for (@_) { s/"//g; } },
         sub { for (@_) { s/,//g; } }
        ],
        $summary
        );
    Filters::consecutive_spaces(@titles);
    Filters::consecutive_spaces(@summaries);
    my %occu = $xdata->occupations();
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

=head2 compute_gender($summary, $xdata)

Attempts to deduce the gender of the person whose summary text is given, returning the string "male" or
"female". The value undef is returned if there is not enough information to draw a conclusion.

=cut

sub compute_gender {
    local $_;
    my $summary = $_[0];
    my $xdata = $_[1];
    my @mwords = $xdata->male_words();
    my @fwords = $xdata->female_words();
    my $male = 0;
    my $female = 0;
    my $ptn;
    # TODO Currently this counts the number of distinct pronouns that match; should we change it to
    #      count all matches? The gender identification works pretty well right now.
    foreach $ptn (@mwords) {
        $male++ if ($summary =~ /\b$ptn\b/i);
    }
    foreach $ptn (@fwords) {
        $female++ if ($summary =~ /\b$ptn\b/i);
    }
    return undef if ($male == 0 and $female == 0);
    return ($male > $female) ? "male" : "female";
}

=head2 find_place_information($title, $summary, $xdata)

Given the name and summary text of a location, determines the nature of the location, as an expression
of the form C<["keyword", "friendly_name"]>. If no location nature can be determined, an array ref to
an empty array C<[]> is returned. If natures could be determined, an array of possible natures is
returned.

=cut

sub find_place_information {
    my $title = $_[0];
    my $summary = $_[1];
    my $xdata = $_[2];
    my %placenames = $xdata->placenames();
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

=head2 determine_population($xml, $xdata)

Given a page about a location, attempts to determine the approximate population of the area. Note
that population makes little sense for "structure" locations, like towers and gardens, and tends to
make more sense for cities and countries. A numerical quantity is returned, or C<undef> if no
information could be garnered from the page.

=cut

sub determine_population {
    my $xml = $_[0];
    my $xdata = $_[1];
    my $title = page_title($xml);
    my $text = full_page_text($xml);
    # TODO We would like to move the keywords here into a ./data/ file and load them through $xdata
    my @pop;
    my $linking = qr/of|was estimated(?: \w+)?|to be/;
    my $adj = qr/(?: over| under)?/;
    my $ptn = qr/([\d,]+(?:\.\d*)?(?: (?:m|b|tr)illion)?)(?:(?<=illion)|[^\d,.%])/;
    for my $noun (qr/population/, qr/\d{4} census/) {
        my $expr = simple_linked_sentence($noun, $ptn, {
            SkimWordCount => 1,
            TitleRegexp => 1,
            MoreRenameClauses => 1,
            AdditionalLinkingVerbs => qr/$linking$adj/i
        });
        while ($text =~ /$expr/g) {
            get_logger()->echo(2, "Population match for $title at $1");
            push @pop, evaluate_number($1);
        }
    }
    return median @pop;
}


=head2 find_weapon_information($title, $summary, $xdata)

Determines the nature of the weapon whose title and summary are supplied, returning a list of
expressions of the form C<["keyword", "friendly_name"]>. If no such information can be drawn
from the summary, the array ref C<[]> is returned.

=cut

sub find_weapon_information {
    my $title = $_[0];
    my $summary = $_[1];
    my $xdata = $_[2];
    my %weapons = $xdata->weapons();
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

=head2 deduce_animal_stats($xml, $xdata)

Counts up the number of appearances of miscellaneous keywords used to determine the nature and behavior
of animals from the summary and title of an animal page. A hashref containing the counted stats is
returned.

=cut

sub deduce_animal_stats {
    my $xml = $_[0];
    my $xdata = $_[1];
    my $title = page_title($xml);
    my $summary = page_summary($xml);
    my %animals = $xdata->animals();
    my %stats;
    foreach my $keyword (keys %animals) {
        my $constant;
        if ($title =~ /\b$keyword\b/i) {
            $constant = 4;
        } else {
            $constant = @{[ $summary =~ /\b$keyword\b/gi ]};
        }
        $stats{'matches'} += $constant;
        get_logger()->echo(2, "Animal $title has $keyword match $constant times") if $constant > 0;
        foreach my $stat (keys %{$animals{$keyword}}) {
            my $coef = $animals{$keyword}->{$stat};
            $stats{$stat} += $coef * $constant;
        }
    }
    return \%stats;
}

=head2 normalize_animal_stats(%stats)

Given the output from C<deduce_animal_stats>, normalizes the stats to be on a scale from 1 to 5
(integer values), with the "boolean-ish" quantities such as air-based and sea-based determinations
normalized to true or false (here, 1 or 0). The hashref that is passed in is not modified; a new
hash is returned.

=cut

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

=head2 shortest_food_synonym($title, $summary, $xdata)

Given the title and summary of a food page, attempts to determine a more "user-friendly" synonym, or
nickname, for the food. The nickname returned from this subroutine is the shortest nickname that is
found, or the original title if it is shorter than any nickname.

=cut

sub shortest_food_synonym {
    my $title = $_[0];
    my $summary = $_[1];
    my $xdata = $_[2];
    $title =~ s/-/ /;
    $summary =~ s/-/ /;
    my @titles = apply_filters(
        [
         \&Filters::paren_expr,
         \&Filters::quoted_phrase
        ],
        $title
        );
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
    foreach my $titlevar (@titles) {
        foreach my $prefix_loop ($xdata->food_prefixes()) {
            my $prefix = $prefix_loop;
            $prefix =~ s/\$title/$titlevar/g;
            foreach my $suffix_loop ($xdata->food_suffixes()) {
                my $suffix = $suffix_loop;
                $suffix =~ s/\$title/$titlevar/g;
                for my $summaryvar (@summaries) {
                    if ($summaryvar =~ /\b$prefix (?:the )?([\w ]+)\b$suffix\W/) {
                        push @candidates, $1;
                    }
                }
            }
        }
    }
    my $shortest = $title;
  EXCLUDE: foreach my $candidate (@candidates) {
      foreach my $word ($xdata->food_blacklist()) {
          next EXCLUDE if $candidate =~ /\b$word\b/i;
      }
      foreach my $word ($xdata->food_negatives()) {
          next EXCLUDE if $candidate =~ /^\s*$word\s*$/i;
      }
      $shortest = $candidate if length $candidate < length $shortest;
  }
    $shortest =~ s/^\s+|\s+$//g;
    return $shortest;
}

=head2 get_plant_type($title, $summary, $xdata)

Tries to determine the sort of plant (such as tree, grass, or flower) that the food grows on
naturally. If such a plant can be determined from the page, a string containing the type of plant
is returned. Otherwise, the special value undef is returned.

=cut

sub get_plant_type {
    my $title = $_[0];
    my $summary = $_[1];
    my $xdata = $_[2];
    my %trees = $xdata->food_trees();
    my $max = undef;
    # Set a threshold so that if nothing matches, we don't falsely identify as something
    my $max_num = 0;
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

=head2 get_nutrition_information($title, %page, $xdata)

Given the information about a food, determines nutrition information about the food. A hashref
containing the data acquired is returned. Values that could not be determined will default to
an appropriate value (usually 0).

=cut

sub get_nutrition_information {
    my $title = $_[0];
    my %page = %{$_[1]};
    my $xdata = $_[2];
    my %result = ( 'nutrition' => 0, 'poison' => 0 );
    my %sections = %{flatten_sections(\%page)};
    foreach my $section (keys %sections) {
        my $okay = 0;
        foreach my $keyword ($xdata->food_sections()) {
            $keyword =~ s/\$title/$title/g;
            if ($section =~ /\b$keyword\b/i) {
                $okay = 1;
                last;
            }
        }
        next unless $okay;
        my $summary = $sections{$section};
        foreach my $curr ($xdata->food_nutrition()) {
            my $constant = 0;
            $constant += @{[ $summary =~ /\b$curr\b/gi ]};
            $result{'nutrition'} += $constant;
        }
        foreach my $curr ($xdata->food_poison()) {
            my $constant = 0;
            $constant += @{[ $summary =~ /\b$curr\b/gi ]};
            $result{'poison'} += $constant;
        }
    }
    return \%result;
}


=head2 find_monster_type($title, $summary, $xdata)

Given the name and summary text of a monster, determines the type of creature the monster
constitutes, returning an expression of the form C<["keyword", "friendly_name"]>. If no type can
be determined, an array ref to an empty array C<[]> is returned. Otherwise, an array of possible
monster types is returned.

=cut

sub find_monster_type {
    my $title = $_[0];
    my $summary = $_[1];
    my $xdata = $_[2];
    my %monsters = $xdata->monster_types();
    my @titles = apply_filters(
        [
         \&Filters::trailing_comma_phrase,
         \&Filters::paren_expr,
         sub { for (@_) { s/$/s/; } }
        ],
        $title
        );
    # This phrase seems to be ubiquitous on pages, so go ahead and snip it off
    $summary =~ s/^\n*In \w+ mythology, ?//i;
    my @summaries = apply_filters(
        [
         \&Filters::paren_expr,
         \&Filters::slash_phrase,
         \&Filters::appositive_phrase,
         sub { for (@_) { s/"//g; } },
         sub { for (@_) { s/,//g; } }
        ],
        $summary
        );
    Filters::consecutive_spaces(@titles);
    Filters::consecutive_spaces(@summaries);
    my @res = ();
    my $ptn;
  PATTERN: foreach $ptn (keys %monsters) {
      foreach my $titlevar (@titles) {
          my $expr = simple_linked_sentence($titlevar, $ptn, {MoreRenameClauses => 1});
          foreach my $summaryvar (@summaries) {
              if ($summaryvar =~ $expr) {
                  push @res, [$monsters{$ptn}, $ptn];
                  next PATTERN;
              }
          }
      }
  }
  PATTERN: foreach $ptn (keys %monsters) {
      foreach my $titlevar (@titles) {
          if ($titlevar =~ /\b$ptn\b/i && not $titlevar =~ /^$ptn$/i) {
              push @res, [$monsters{$ptn}, $ptn];
              next PATTERN;
          }
      }
  }
    return \@res;
}


=head2 deduce_monster_affinity($xml, $xdata)

Counts up the number of appearances of miscellaneous keywords used to determine the nature and
affinity of monsters from the summary and title of the given page. A hashref containing the resulting
stats is returned.

=cut

sub deduce_monster_affinity {
    my $xml = $_[0];
    my $xdata = $_[1];
    my $title = page_title($xml);
    my %affinities = $xdata->monster_affinities();
    my %stats;
    foreach my $keyword (keys %affinities) {
        my $constant;
        if ($title =~ /\b$keyword\b/i) {
            $constant = 4;
        } else {
            $constant = 0; # TODO Do this same select_sections thing with deduce_animal_stats
            my %sections = select_sections($xml, qr/$title|Overview|Mythology/i);
            for (values %sections) {
                $constant += @{[ /\b$keyword\b/gi ]};
            }
        }
        $stats{'matches'} += $constant;
        get_logger()->echo(2, "Monster $title has $keyword match $constant times") if $constant > 0;
        foreach my $stat (keys %{$affinities{$keyword}}) {
            my $coef = $affinities{$keyword}->{$stat};
            $stats{$stat} += $coef * $constant;
        }
    }
    return \%stats;
}

=head2 interpret_monster_affinity(%stats)

Given the return value from C<deduce_monster_affinity>, determines the chaos (chaotic / neutral /
civilized) and affinity (light / neutral / dark) of the monster, returning a hashref of the results.

=cut

sub interpret_monster_affinity {
    my %stats = %{$_[0]};
    my %result = (chaos => 'neutral',
                  affinity => 'neutral');
    my $sensitivity = 2; # TODO Make this configurable
    $result{'affinity'} = 'dark'      if $stats{'light'} <= - $sensitivity;
    $result{'affinity'} = 'light'     if $stats{'light'} >=   $sensitivity;
    $result{'chaos'   } = 'chaotic'   if $stats{'civil'} <= - $sensitivity;
    $result{'chaos'   } = 'civilized' if $stats{'civil'} >=   $sensitivity;
    return \%result;
}

1;
