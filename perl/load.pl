local $_;

my(%occu, @mwords, @fwords, %placenames, %weapons);
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
    /^((?:\w+ )+) +([\w ]+)$/ or die("Illegal line in placenames.txt at line $.");
    my $key = $1;
    chop $key;
    $placenames{$key} = $2;
}
close $fh;

open $fh, '<', './data/weapons.txt' or die("$!");
while (<$fh>) {
    chomp;
    /^((?:\w+ )+) +([\w ]+)$/ or die("Illegal line in weapons.txt at line $.");
    my $key = $1;
    chop $key;
    $weapons{$key} = $2;
}
close $fh;

(
    'occu' => \%occu,
    'mwords' => \@mwords,
    'fwords' => \@fwords,
    'placenames' => \%placenames,
    'weapons' => \%weapons
);
