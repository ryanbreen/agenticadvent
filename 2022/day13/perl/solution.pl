#!/usr/bin/env perl
use strict;
use warnings;
use JSON;
use File::Basename;
use Cwd 'abs_path';

# Compare two values recursively
# Returns: -1 if left < right (correct order)
#           1 if left > right (wrong order)
#           0 if equal (continue)
sub compare {
    my ($left, $right) = @_;

    my $left_is_array = ref($left) eq 'ARRAY';
    my $right_is_array = ref($right) eq 'ARRAY';

    # Both integers
    if (!$left_is_array && !$right_is_array) {
        return $left <=> $right;
    }

    # Both lists
    if ($left_is_array && $right_is_array) {
        my $min_len = @$left < @$right ? @$left : @$right;
        for my $i (0 .. $min_len - 1) {
            my $result = compare($left->[$i], $right->[$i]);
            return $result if $result != 0;
        }
        # Check lengths
        return @$left <=> @$right;
    }

    # Mixed types - convert integer to list
    if (!$left_is_array) {
        return compare([$left], $right);
    } else {
        return compare($left, [$right]);
    }
}

sub part1 {
    my ($text) = @_;
    my @pairs = split /\n\n/, $text;
    my $total = 0;

    for my $i (0 .. $#pairs) {
        my @lines = split /\n/, $pairs[$i];
        my $left = decode_json($lines[0]);
        my $right = decode_json($lines[1]);

        if (compare($left, $right) == -1) {
            $total += $i + 1;  # 1-indexed
        }
    }

    return $total;
}

sub part2 {
    my ($text) = @_;

    # Parse all non-empty lines
    my @lines = grep { /\S/ } split /\n/, $text;
    my @packets = map { decode_json($_) } @lines;

    # Add divider packets
    my $divider1 = [[2]];
    my $divider2 = [[6]];
    push @packets, $divider1, $divider2;

    # Sort using comparison function
    @packets = sort { compare($a, $b) } @packets;

    # Find positions of dividers (1-indexed)
    my ($pos1, $pos2);
    for my $i (0 .. $#packets) {
        my $encoded = encode_json($packets[$i]);
        if ($encoded eq '[[2]]') {
            $pos1 = $i + 1;
        } elsif ($encoded eq '[[6]]') {
            $pos2 = $i + 1;
        }
    }

    return $pos1 * $pos2;
}

sub main {
    my $script_dir = dirname(abs_path(__FILE__));
    my $input_file = "$script_dir/../input.txt";

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    # Remove trailing whitespace
    $text =~ s/\s+$//;

    print "Part 1: ", part1($text), "\n";
    print "Part 2: ", part2($text), "\n";
}

main();
