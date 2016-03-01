
use Data::Dumper;

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
    $name =~ s/\\x..//g;
    my $gender = compute_gender($summary, $data->{'mwords'}, $data->{'fwords'});
    my %curr = (
        nature => 'Person',
        name => $name,
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
    $name =~ s/\\x..//g;
    $summary =~ s/\\x..//g;
    my $info = find_place_information($name, $summary, $data->{'placenames'});
    my %curr = (
        nature => 'Place',
        name => $name,
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
    $name =~ s/\\x..//g;
    $summary =~ s/\\x..//g;
    my $info = find_weapon_information($name, $summary, $data->{'weapons'});
    my %curr = (
        nature => 'Weapon',
        name => $name,
        info => ($info ? [$data->{'weapons'}->{$info}, $info] : undef)
        );
    return \%curr;
}

1;
