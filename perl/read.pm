
use Data::Dumper;
use perl::navigation;
use perl::logging;

=head2 unparen($value)

Removes any trailing parenthesized expression from the string.

=cut

sub unparen {
    my $value = $_[0];
    $value =~ s/ *\(.*\) *$//;
    return $value;
}

=head2 read_person($xml, $data)

Given the XML data for a person page, parses the page for the person's basic information and returns a hashref
containing the appropriate information, substituting undef for any values that cannot be determined.

=cut

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

=head2 read_place($xml, $data)

Given the XML data for a place page, computes the name and basic information about the location, returning
a hashref containing the results, with undef filled in for any un-identifiable fields.

=cut

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

=head2 read_weapon($xml, $data)

Given the XML data for a weapon page, parses the page and computes the basic information about the weapon
itself, returning a hashref. Any un-identifiable fields are filled in with undef.

=cut

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

=head2 read_animal($xml, $data)

Reads the XML animal page data supplied and returns a hashref containing computed values for the animal's nature
and attitude, filling in un-identifiable values with undef, or 0 in cases where the value would be computed,
not parsed.

=cut

sub read_animal {
    local $_;
    my $xml = $_[0];
    my $data = $_[1];
    my $name = page_title($xml);
    my $summary = page_summary($xml);
    my %stats = %{deduce_animal_stats($name, $summary, $data)};
    my %norm = normalize_animal_stats(\%stats);
    #get_logger()->echo(2, "Animal $name has the following stats:");
    #get_logger()->echo_obj(2, \%stats); # TODO echo_obj was broken; write a working version
    my %curr = (
        nature => 'Animal',
        name => unparen($name),
        %norm,
        matches => $stats{'matches'}
        );
    return \%curr;
}

=head2 read_food($xml, $data)

Parses the XML food page and determines basic information about the food's name and nutritional value, returning
a hashref containing the results. The special value undef is used in place of any fields that cannot be
determined from the data.

=cut

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
