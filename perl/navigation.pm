
use Data::Dumper;

local $_;

# _flatten_sections(%page, $prefix)
sub _flatten_sections {
    my %page = %{$_[0]->{'text'}->[0]};
    my $prefix = $_[1];
    my $new_prefix = $_[0]->{'name'};
    $new_prefix = $prefix . "\0" . $new_prefix unless $prefix eq '';
    my %sections;
    $sections{$new_prefix} = $page{'content'};
    for my $section (@{$page{'section'}}) {
        my %curr = %{_flatten_sections($section, $new_prefix)};
        @sections{keys %curr} = values %curr;
    }
    return \%sections;
}

=head2 flatten_sections(%page)

Given a Wikipedia page with section headers, flatten the hierarchy into a single
hash consisting of all sections on the page, listed hierarchically. For example,
given the following section hierarchy:

=over

=item * History

=item * Uses

=over

=item * Recent Uses

=item * Historical Uses

=back

=item * Pop Culture

=back

The following result will be produced

=over

=item * History

=item * Uses

=item * Uses\0Recent Uses

=item * Uses\0Historical Uses

=item * Pop Culture

=back

=cut

sub flatten_sections {
    return _flatten_sections($_[0], '');
}

=head2 nonhierarchical(%sections)

Given the output of flatten_sections(), remove the hierarchical information and keep only the
deepest-level section name of each entry.

=cut

sub nonhierarchical {
    local $1;
    my %sections = %{$_[0]};
    my %new_sections;
    @new_sections{map { /\x00([^\x00]+)$/ ? $1 : $_ } (keys %sections)} = values %sections;
    return \%new_sections;
}

=head2 nonhierarchical_sections(%page)

Flattens the sections and then removes the hierarchical nature from them.

=cut

sub nonhierarchical_sections {
    return nonhierarchical flatten_sections shift;
}

=head2 page_title($xml)

Return the page's title.

=cut

sub page_title {
    return $_[0]->{'name'};
}

=head2 page_summary($xml)

Return the page's summary text.

=cut

sub page_summary {
    return $_[0]->{'text'}->[0]->{'content'};
}

=head2 page_links($xml)

Compute and return an array of the links on the page, in an unspecified order.

=cut

sub page_links {
    return $_[0]->{'links'}->[0]->{'link'};
}

=head2 full_page_text($xml)

Returns the full text of the page, leaving headers in as plaintext (without the Wikipedia-style header markdown).
Note that, as the page sections themselves are hash elements, the order of sections is undefined, but all sections
will be present.

=cut

sub full_page_text { # TODO Get the sections in the right order, if possible
    my %sects = %{nonhierarchical_sections shift};
    my $text = '';
    for my $key (keys %sects) {
        $text .= "$key\n$sects{$key}\n";
    }
    return $text;
}

1;
