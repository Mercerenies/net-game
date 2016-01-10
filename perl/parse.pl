#!/usr/bin/perl

use perl::script;

use strict;
use 5.010;
use warnings;

use Data::Dumper;

local $_;

my %data = do './perl/load.pl';

my ($name, $summary, @occs, $gender, $info, @args);

# Celebs / People
my $second = 0;
PEOPLE: while (<>) {
    chomp;
    if (/^BREAK$/) {
        last "PEOPLE" if $second;
        $second = 1;
        next "PEOPLE";
    }
    @args = split(/'  /);
    $name = $args[0] . "'";
    $summary = $args[1];
    crop $name;
    crop $summary;
    @occs = find_occu($summary, \%{$data{'occu'}});
    $name =~ s-"-\"-;
    $name =~ s/\\x..//g;
    $gender = compute_gender($summary, \@{$data{'mwords'}}, \@{$data{'fwords'}});
    $gender = (defined $gender ? ":gender $gender " : "");
    print qq[(person "$name" $gender:occupations (];
    foreach (@occs) {
        my @arr = @{$_};
        $arr[0] =~ s-"-\"-;
        print qq[($arr[1] . "$arr[0]")];
        print ' ' unless \$_ == \$occs[-1];
    }
    print qq[))\n];
}

# Places
while (<>) {
    chomp;
    last if (/^BREAK$/);
    @args = split(/'  /);
    $name = $args[0] . "'";
    $summary = $args[1];
    crop $name;
    crop $summary;
    $name =~ s/\\x..//g;
    $summary =~ s/\\x..//g;
    $info = find_place_information($name, $summary, \%{$data{'placenames'}});
    $info = $info ? qq[ :info ($data{'placenames'}->{$info} . "$info")] : '';
    print qq[(place "$name"$info)\n];
}

# Weapons
while (<>) {
    chomp;
    last if (/^BREAK$/);
    @args = split(/'  /);
    $name = $args[0] . "'";
    $summary = $args[1];
    crop $name;
    crop $summary;
    $name =~ s/\\x..//g;
    $summary =~ s/\\x..//g;
    $info = find_weapon_information($name, $summary, \%{$data{'weapons'}});
    $info = $info ? qq[ :type ($data{'weapons'}->{$info} . "$info")] : '';
    print qq[(weapon "$name"$info)\n];
}
