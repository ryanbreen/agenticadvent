#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(sum);
use File::Basename;
use File::Spec;

# Read input file
my $dir = dirname(__FILE__);
my $input_file = File::Spec->catfile($dir, '..', 'input.txt');
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close($fh);
chomp @lines;

# Parse cards into an array of [winning_hash, have_array] pairs
sub parse_cards {
    my ($lines_ref) = @_;
    my @cards;

    for my $line (@$lines_ref) {
        my ($numbers) = $line =~ /:\s*(.+)/;
        my ($winning_part, $have_part) = split /\|/, $numbers;

        # Extract winning numbers into a hash for O(1) lookup
        my %winning = map { $_ => 1 } ($winning_part =~ /(\d+)/g);

        # Extract have numbers as array (no need for hash)
        my @have = ($have_part =~ /(\d+)/g);

        push @cards, [\%winning, \@have];
    }

    return @cards;
}

# Count matching numbers using grep
sub count_matches {
    my ($winning_ref, $have_ref) = @_;
    return scalar grep { $winning_ref->{$_} } @$have_ref;
}

# Part 1: Calculate total points
# Score = 2^(matches-1) if matches > 0, else 0
sub part1 {
    my ($cards_ref) = @_;

    return sum map {
        my ($winning, $have) = @$_;
        my $matches = count_matches($winning, $have);
        $matches > 0 ? 2 ** ($matches - 1) : 0;
    } @$cards_ref;
}

# Part 2: Count total scratchcards after cascading copies
# Each card with M matches wins copies of the next M cards
sub part2 {
    my ($cards_ref) = @_;
    my $num_cards = scalar @$cards_ref;

    # Calculate matches for each card
    my @matches = map {
        my ($winning, $have) = @$_;
        count_matches($winning, $have);
    } @$cards_ref;

    # Track number of copies of each card (starting with 1 of each)
    my @copies = (1) x $num_cards;

    # Process each card and cascade copies
    for my $i (0 .. $num_cards - 1) {
        my $m = $matches[$i];
        my $end = $i + $m < $num_cards ? $i + $m : $num_cards - 1;
        $copies[$_] += $copies[$i] for ($i + 1 .. $end);
    }

    return sum @copies;
}

# Parse once, use for both parts
my @cards = parse_cards(\@lines);

print "Part 1: " . part1(\@cards) . "\n";
print "Part 2: " . part2(\@cards) . "\n";
