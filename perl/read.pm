
use Data::Dumper;

sub unparen {
    $_[0] =~ s/ *\(.*\) *$//;
    return $_[0];
}

# Celebs / People
sub read_person {
    my $xml = $_[0];
    my $data = $_[1];
    my $name = $xml->{'name'};
    my $summary = $xml->{'content'};
    crop $name;
    crop $summary;
    my @occs = find_occu($summary, $data->{'occu'});
    $name =~ s-"-\"-;
    my $gender = compute_gender($summary, $data->{'mwords'}, $data->{'fwords'});
    my %curr = (
        nature => 'Person',
        name => unparen($name),
        gender => $gender,
        occupations => []
        );
    foreach (@occs) {
        my @arr = @{$_};
        my @new = ($arr[1], $arr[0]);
        push @{$curr{'occupations'}}, \@new;
    }
    return \%curr;
}

# Places
sub read_place {
    my $xml = $_[0];
    my $data = $_[1];
    my $name = $xml->{'name'};
    my $summary = $xml->{'content'};
    crop $name;
    crop $summary;
    my $info = find_place_information($name, $summary, $data->{'placenames'});
    my %curr = (
        nature => 'Place',
        name => unparen($name),
        info => ($info ? [$data->{'placenames'}->{$info}, $info] : undef)
        );
    return \%curr;
}

# Weapons
sub read_weapon {
    my $xml = $_[0];
    my $data = $_[1];
    my $name = $xml->{'name'};
    my $summary = $xml->{'content'};
    crop $name;
    crop $summary;
    my $info = find_weapon_information($name, $summary, $data->{'weapons'});
    $name =~ s/swords/sword/g;
    $name =~ s/blades/blade/g;
    my %curr = (
        nature => 'Weapon',
        name => unparen($name),
        info => ($info ? [$data->{'weapons'}->{$info}, $info] : undef)
        );
    return \%curr;
}

# Monsters
sub read_monster {
    # TODO This
}

# Animals
sub read_animal {
    # TODO This
}

1;
