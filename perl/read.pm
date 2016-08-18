
use Data::Dumper;
use perl::navigation;

=head2 unparen($value)

Remove any trailing parenthesized expression from the string.

=cut

sub unparen {
    my $value = $_[0];
    $value =~ s/ *\(.*\) *$//;
    return $value;
}

# Celebs / People
sub read_person {
    my $xml = $_[0];
    my $data = $_[1];
    my $name = page_title($xml);
    my $summary = page_summary($xml);
    my @occs = find_occu($name, $summary, $data->{'occu'});
    my $gender = compute_gender($summary, $data->{'mwords'}, $data->{'fwords'});
    my %curr = (
        nature => 'Person',
        name => unparen($name),
        gender => $gender,
        occupations => \@occs
        );
    return \%curr;
}

# Places
sub read_place {
    my $xml = $_[0];
    my $data = $_[1];
    my $name = page_title($xml);
    my $summary = page_summary($xml);
    my $info = find_place_information($name, $summary, $data->{'placenames'});
    my %curr = (
        nature => 'Place',
        name => unparen($name),
        info => $info
        );
    return \%curr;
}

# Weapons
sub read_weapon {
    my $xml = $_[0];
    my $data = $_[1];
    my $name = page_title($xml);
    my $summary = page_summary($xml);
    my $info = find_weapon_information($name, $summary, $data->{'weapons'});
    # TODO Factor out these depluralizations for modularity's sake
    $name =~ s/swords/sword/g;
    $name =~ s/blades/blade/g;
    my %curr = (
        nature => 'Weapon',
        name => unparen($name),
        info => $info
        );
    return \%curr;
}

# Monsters
sub read_monster {
    # TODO Monster Reading
}

# Animals
sub read_animal {
    local $_;
    my $xml = $_[0];
    my $data = $_[1];
    my $name = page_title($xml);
    my $summary = page_summary($xml);
    my %stats = %{deduce_animal_stats($name, $summary, $data)};
    my %norm = normalize_animal_stats(\%stats);
#    print STDERR $name;
#    print STDERR Dumper \%stats;
    my %curr = (
        nature => 'Animal',
        name => unparen($name),
        %norm,
        matches => $stats{'matches'}
        );
    return \%curr;
}

# Foods
sub read_food {
    local $_;
    my $xml = $_[0];
    my $data = $_[1];
    my $name = page_title($xml);
    my $title = unparen($name);
    my $summary = page_summary($xml);
    my $nickname = shortest_food_synonym($title, $summary, $data);
    my $plant = get_plant_type($name, $summary, $data->{'foodtrees'});
    my %nutrition = %{get_nutrition_information($name, $xml, $data)};
    my %curr = (
        nature => 'Food',
        name => $title,
        nickname => $nickname,
        plant => $plant,
        nutrition => $nutrition{'nutrition'},
        poison => $nutrition{'poison'}
        );
    return \%curr;
}

1;
