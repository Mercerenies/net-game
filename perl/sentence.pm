
local $_;

use Data::Dumper;
use feature 'unicode_strings';

my $LINKVERB = "(?:is|was|are|were)";
my $ARTICLE = "(?:an?|the)"; # Unused currently; we'll see if we can do something about that at some point
my $RENAME = "(?:or|in|of)";

=head2 simple_linked_sentence($titlevar, $ptn, %options)

Compiles a regular expression to detect sentences roughly of the form "SOMETHING is SOMETHING_ELSE".
The $titlevar argument is taken literally, while $ptn will be interpreted as a regex string. This
distinction is to allow $ptn to be loaded from trusted data files, while $titlevar is downloaded raw
as a page title from outside sources.

The %options argument can contain the following elements.

=over

=item * MoreRenameClauses (default: false)

If this value is truthy, the extended rename clauses will be used, allowing any word in $RENAME to
introduce a rename clause. By default, only the word "or" introduces a rename clause.

=item * StrictSkimWords (default: false)

If this value is truthy, a "word" which can be skimmed over is considered more strictly, in that it can
only consist of regex word characters and hyphens. By default, any sequence of non-space characters is
considered a word for the purposes of skimming.

=item * SkimWordCount (default: 9)

The maximum number of "skim words" allowed between the title and the pattern. More skim words results
in more false positives but also more correct results. Fewer skim words eliminates results but also
eliminates false positives.

=item * MiddleNameRule (default: false)

If this value is truthy, it will allow a single "word" (using a variant of strict words, even if
StrictSkimWords is not specified) in between the first and last name of the title. Specifically, it
will allow a single word immediately before the final word of the title. This rule is ignored if the
title consists of less than two words.

=back

=item * TitleRegexp (default: false)

If this value is truthy, the $titlevar argument will be treated as a regular expression and
interpolated without escaping. Without this option, the $titlevar is treated as plaintext
and interpolated with \Q and \E for safety. Although the behavior is well-defined, it is not
recommended to use this option in conjunction with MiddleNameRule, as the behavior can be rather
confusing.

=back

=item * AdditionalLinkingVerbs (default: undef)

If this value is defined, it will be used as a fallback regex for linking verbs, if the default
linking verb expression fails.

=back

=cut

sub simple_linked_sentence {
    my $titlevar = shift;
    my $ptn = shift;
    my %options = %{+shift};

    my $rename_words = $options{'MoreRenameClauses'} ? $RENAME : 'or';
    my $skim_word = $options{'StrictSkimWords'} ? qr/[\w-]+/i : qr/[^ ]+/i;
    my $skim_count = 0+ ($options{'SkimWordCount'} // 9);
    my $title_intermediate = qr/(?:\Q$titlevar\E)/i;
    $title_intermediate = qr/(?:$titlevar)/i if $options{'TitleRegexp'};
    if ($options{'MiddleNameRule'} && $titlevar =~ /^([\w\"-]+\s)+([\w\"-]+)$/) {
        $title_intermediate = qr/$1(?:[\w"-]+\.? )?$2/i;
    }

    my $rename_clause = qr/(?:$rename_words (?:$skim_word ){1,3})/i;
    my $title_word = qr/(?:$title_intermediate )/i;
    my $link_word = qr/(?:$LINKVERB )/i;
    my $skim_clause = qr/(?:(?:$skim_word ){0,$skim_count})/i;
    my $ptn_clause = qr/(?:\b$ptn\b)/i;

    if (defined $options{'AdditionalLinkingVerbs'}) {
        $link_word = qr/(?:(?:$link_word)|(?:$options{'AdditionalLinkingVerbs'} ))/i;
    }

    return qr/$title_word $rename_clause? $link_word $skim_clause $ptn_clause/ix;
}

1;
