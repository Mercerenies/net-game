
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
    local $_;
    my $xml = $_[0];
    my $data = $_[1];
    my $name = $xml->{'name'};
    my $summary = $xml->{'content'};
    my %stats = %{deduce_animal_stats($name, $summary, $data)};
    # $threat, $size, $pack are on a scale of 1 to 5
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
    # TODO Speed
    $speed = 5;
    $speed -=!! ($stats{'speed'} <= 10);
    $speed -=!! ($stats{'speed'} <=  6);
    $speed -=!! ($stats{'speed'} <=  0);
    $speed -=!! ($stats{'speed'} <= -2);
    $sea = 0+!! ($stats{'sea'} > 1);
    $air = 0+!! ($stats{'air'} > 1);
#    print STDERR $name;
#    print STDERR Dumper \%stats;
    my %curr = (
        name => unparen($name),
        threat => $threat,
        size => $size,
        pack => $pack,
        speed => $speed,
        sea => \$sea,
        air => \$air,
        matches => $stats{'matches'}
        );
    return \%curr;
}

1;
