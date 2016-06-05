local $_;

my(%occu, @mwords, @fwords, %placenames, %weapons, %animals, @foodprefixes, @foodblacklist, @foodsuffixes,
   %foodtrees, @foodnegatives);
my $fh;

open $fh, '<', './data/occupations.txt' or die("$!");
while (<$fh>) {
    chomp;
    /^((?:\w+ )+) +([\w ]+)$/ or die("Illegal line in occupations.txt at line $.");
    my $key = $1;
    chop $key;
    $occu{$key} = $2;
}
close $fh;

open $fh, '<', './data/gender.txt' or die("$!");
while (<$fh>) {
    chomp;
    /^((?:\w+ )+) +([\w ]+)$/ or die("Illegal line in gender.txt at line $.");
    my $key = $1;
    my $gen = $2;
    push @mwords, $key if ($gen =~ /^male$/);
    push @fwords, $key if ($gen =~ /^female$/);
}
chop @mwords;
chop @fwords;
close $fh;

open $fh, '<', './data/placenames.txt' or die("$!");
while (<$fh>) {
    chomp;
    /^((?:[\w\-]+ )+) +([\w ]+)$/ or die("Illegal line in placenames.txt at line $.");
    my $key = $1;
    chop $key;
    $placenames{$key} = $2;
}
close $fh;

open $fh, '<', './data/weapons.txt' or die("$!");
while (<$fh>) {
    chomp;
    /^((?:[\w\-]+ )+) +([\w ]+)$/ or die("Illegal line in weapons.txt at line $.");
    my $key = $1;
    chop $key;
    $weapons{$key} = $2;
}
close $fh;

open $fh, '<', './data/animals.txt' or die("$!");
while (<$fh>) {
    chomp;
    /^([\w\- ]+)+: (.*)$/ or die("Illegal line in animals.txt at line $.");
    my $key = $1;
    my @rest = split(/,/, $2);
    my %stats;
    foreach my $token (@rest) {
        $token =~ /\b(\w+) *([-+]\d+)/ or die("Illegal line in animals.txt at line $.");
        $stats{$1} = 0+ $2;
    }
    $animals{$key} = \%stats;
}

open $fh, '<', './data/foodnames.txt' or die("$!");
while (<$fh>) {
    chomp;
    if (s/^\^//) {
        push @foodblacklist, $_;
    } elsif (s/^\>//) {
        push @foodsuffixes, $_;
    } elsif (s/^\<//) {
        push @foodprefixes, $_;
    } elsif (s/^\!//) {
        push @foodnegatives, $_;
    } elsif (s/^\*//) {
        /^(\w+)/ or die("Illegal line in foodnames.txt at line $.");
        $foodtrees{$1} = [split / /];
    } else {
        die("Illegal line in foodnames.txt at line $.");
    }
}
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
 'foodtrees' => \%foodtrees
);
