
use Data::Dumper;

use 5.016;
use strict;
use warnings;
use feature 'unicode_strings';

local $_;

# _flatten_sections($name, %page, $prefix)
sub _flatten_sections {
    my $name = $_[0];
    my %page = %{$_[1]};
    my $prefix = $_[2];
    my $new_prefix = $page{'name'} // $name;
    $new_prefix = $prefix . "\0" . $new_prefix unless $prefix eq '';
    my %sections;
    $sections{$new_prefix} = $page{'content'};
    for my $section (@{$page{'section'}}) {
        my %curr = %{_flatten_sections($name, $section, $new_prefix)};
        @sections{keys %curr} = values %curr;
    }
    return \%sections;
}

=head2 flatten_sections(%page)

Given a Wikipedia page with section headers, flattens the hierarchy into a single
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
    return _flatten_sections($_[0]->{'name'}, $_[0]->{'text'}->[0], '');
}

=head2 nonhierarchical(%sections)

Given the output of flatten_sections(), removes the hierarchical information to keep only the
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

Returns the page's title.

=cut

sub page_title {
    return $_[0]->{'name'};
}

=head2 page_summary($xml)

Returns the page's summary text.

=cut

sub page_summary {
    return $_[0]->{'text'}->[0]->{'content'};
}

=head2 page_links($xml)

Computes and returns an array of the links on the page, in an unspecified order.

=cut

sub page_links {
    return $_[0]->{'links'}->[0]->{'link'};
}

=head2 full_page_text($xml)

Returns the full text of the page, leaving headers in as plaintext (without the Wikipedia-style header markdown).
Note that, as the page sections themselves are hash elements, the order of sections is undefined, but all sections
will be present.

=cut

sub full_page_text {
    my %sects = %{nonhierarchical_sections shift};
    my $text = '';
    for my $key (keys %sects) {
        $text .= "$key\n$sects{$key}\n";
    }
    return $text;
}

=head2 select_sections($xml, $regex)

Returns a hash consisting of all of the sections (with corresponding text) whose headers match the given
regular expression.

=cut

sub select_sections {
    my $xml = shift;
    my $regex = shift;
    my %sections = %{nonhierarchical_sections $xml};
    my %result;
    for my $key (keys %sections) {
        $result{$key} = $sections{$key} if $key =~ $regex;
    }
    return %result;
}

1;
