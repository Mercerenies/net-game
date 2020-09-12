
local $_;

use 5.016;
use strict;
use warnings;
use feature 'unicode_strings';

use lib 'perl';

use datafile;
use set;

my(%occu, @mwords, @fwords, %placenames, %weapons, %animals, @foodprefixes, @foodblacklist, @foodsuffixes,
   %foodtrees, @foodnegatives, @foodnutrition, @foodpoison, @foodsections, %monsters, %monstertypes);
my $fh;

open $fh, '<', './data/occupations.txt' or die("$!");
%occu = load_two_column_file($fh, 'occupations.txt');
close $fh;

open $fh, '<', './data/gender.txt' or die("$!");
my %genders = load_two_column_file($fh, 'gender.txt');
foreach my $key (keys %genders) {
    my $gen = $genders{$key};
    push @mwords, $key if ($gen =~ /^male$/);
    push @fwords, $key if ($gen =~ /^female$/);
}
close $fh;

open $fh, '<', './data/placenames.txt' or die("$!");
%placenames = load_two_column_file($fh, 'placenames.txt');
close $fh;

open $fh, '<', './data/weapons.txt' or die("$!");
%weapons = load_two_column_file($fh, 'weapons.txt');
close $fh;

open $fh, '<', './data/animals.txt' or die("$!");
%animals = load_keyword_search_file($fh, 'animals.txt');
close $fh;

open $fh, '<', './data/foodnames.txt' or die("$!");
my $foodmode = 'prefix';
my %foodmodes = set_containing('prefix', 'suffix', 'blacklist', 'negative',
                               'plant', 'nutrition', 'poison', 'sections');
while (<$fh>) {
    chomp;
    if (s/^://) {
        $foodmode = $_;
        die("Illegal line in foodnames.txt at line $.") unless defined $foodmodes{$foodmode};
    } elsif ($foodmode eq 'blacklist') {
        push @foodblacklist, $_;
    } elsif ($foodmode eq 'suffix') {
        push @foodsuffixes, $_;
    } elsif ($foodmode eq 'prefix') {
        push @foodprefixes, $_;
    } elsif ($foodmode eq 'negative') {
        push @foodnegatives, $_;
    } elsif ($foodmode eq 'plant') {
        /^(\w+)/ or die("Illegal line in foodnames.txt at line $.");
        $foodtrees{$1} = [split / /];
    } elsif ($foodmode eq 'nutrition') {
        push @foodnutrition, split / /;
    } elsif ($foodmode eq 'poison') {
        push @foodpoison, split / /;
    } elsif ($foodmode eq 'sections') {
        push @foodsections, $_;
    } else {
        die("Illegal line in foodnames.txt at line $.");
    }
}
close $fh;

open $fh, '<', './data/monsters.txt' or die("$!");
%monsters = load_two_column_file($fh, 'monsters.txt');
close $fh;

open $fh, '<', './data/affinity.txt' or die("$!");
%monstertypes = load_keyword_search_file($fh, 'affinity.txt');
close $fh;

(
 'occu' => \%occu,
 'mwords' => \@mwords,
 'fwords' => \@fwords,
 'placenames' => \%placenames,
 'weapons' => \%weapons,
 'animals' => \%animals,
 'foodprefixes' => \@foodprefixes,
 'foodnegatives' => \@foodnegatives,
 'foodblacklist' => \@foodblacklist,
 'foodsuffixes' => \@foodsuffixes,
 'foodtrees' => \%foodtrees,
 'foodnutrition' => \@foodnutrition,
 'foodpoison' => \@foodpoison,
 'foodsections' => \@foodsections,
 'monsters' => \%monsters,
 'monstertypes' => \%monstertypes
);
