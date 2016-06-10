
use Data::Dumper;

local $_;

# _flatten_sections(%page, $prefix)
sub _flatten_sections {
    my %page = %{$_[0]};
    my $prefix = $_[1];
    my $new_prefix = $page{'name'};
    $new_prefix = $prefix . "\0" . $new_prefix unless $prefix eq '';
    my %sections;
    $sections{$new_prefix} = $page{'content'};
    for my $section (@{$page{'section'}}) {
        my %curr = %{_flatten_sections($section, $new_prefix)};
        @sections{keys %curr} = values %curr;
    }
    return \%sections;
}

# flatten_sections(%page)
sub flatten_sections {
    return _flatten_sections($_[0], '');
}

# nonhierarchical(%sections) # Expects the output of flatten_sections()
sub nonhierarchical {
    local $1;
    my %sections = %{$_[0]};
    my %new_sections;
    @new_sections{map { /\x00([^\x00]+)$/ ? $1 : $_ } (keys %sections)} = values %sections;
    return \%new_sections;
}

1;
