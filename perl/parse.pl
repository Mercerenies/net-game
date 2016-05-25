#!/usr/bin/perl

use perl::script;
use perl::read;

use strict;
use 5.010;
use warnings;

use Data::Dumper;
use JSON::PP;
use XML::Simple qw(:strict);

local $_;

my %data = do './perl/load.pl';

my %table = (
    'places'   => \&read_place  ,
    'people'   => \&read_person ,
    'celebs'   => \&read_person ,
    'weapons'  => \&read_weapon ,
    'monsters' => \&read_monster,
    'animals'  => \&read_animal
    );

my @result = ();
my $xml = XMLin(\*STDIN, ForceArray => [], KeyAttr => {});
my @pages = @{$xml->{'pages'}};

for my $pageset (@pages) {
    my $call = $table{ $pageset->{'type'} };
    next unless defined $call;
    my $set = $pageset->{'page'};
    for my $page (ref($set) eq 'ARRAY' ? @$set : ($set//())) {
        push @result, $call->($page, \%data);
    }
}

print encode_json \@result;
