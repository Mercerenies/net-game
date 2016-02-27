#!/usr/bin/perl

use perl::script;

use strict;
use 5.010;
use warnings;

use Data::Dumper;
use JSON::PP;

local $_;

my %data = do './perl/load.pl';

my ($name, $summary, @occs, $gender, $info, @args);

my @result = ();

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
    push @result, \%curr;
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
    my %curr = (
        nature => 'Place',
        name => $name,
        info => ($info ? [$data{'placenames'}->{$info}, $info] : undef)
        );
    push @result, \%curr;
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
    my %curr = (
        nature => 'Weapon',
        name => $name,
        info => ($info ? [$data{'weapons'}->{$info}, $info] : undef)
        );
    push @result, \%curr;
}

print encode_json \@result;
