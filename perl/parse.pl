#!/usr/bin/perl

use perl::logging;
use perl::script;
use perl::read;
use perl::navigation;

use 5.016;
use strict;
use warnings;
use feature 'unicode_strings';

use Data::Dumper;
use JSON::PP;
use XML::Simple qw(:strict);

local $_;

my %data = do './perl/load.pl';
die("$@") if $@;
#print STDERR Dumper \%data;

my $debug_level = $ARGV[0];
get_logger()->set_debug_level($debug_level);

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

my $xdata = data_compile(\%data);

for my $pageset (@pages) {
    my $call = $table{ $pageset->{'type'} };
    next unless defined $call;
    for my $page (@{$pageset->{'page'}}) {
        get_logger()->echo(1, "Parsing ${\page_title($page)}");
        push @result, $call->($page, $xdata);
    }
}

print encode_json \@result;
