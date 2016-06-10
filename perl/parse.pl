#!/usr/bin/perl

use perl::script;
use perl::read;
use perl::navigation;

use strict;
use 5.010;
use warnings;

use Data::Dumper;
use JSON::PP;
use XML::Simple qw(:strict);

local $_;

my %data = do './perl/load.pl';
die("$@") if $@;

my %table = (
    'places'   => \&read_place  ,
    'people'   => \&read_person ,
    'celebs'   => \&read_person ,
    'weapons'  => \&read_weapon ,
    'monsters' => \&read_monster,
    'animals'  => \&read_animal ,
    'foods'    => \&read_food
    );

my @result = ();
my $xml = XMLin(\*STDIN, ForceArray => 1, KeyAttr => {});
my @pages = @{$xml->{'pages'}};

for my $pageset (@pages) {
    my $call = $table{ $pageset->{'type'} };
    next unless defined $call;
    for my $page (@{$pageset->{'page'}}) {
        push @result, $call->($page, \%data);
    }
}

print encode_json \@result;
