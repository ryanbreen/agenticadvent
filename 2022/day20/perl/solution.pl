#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

sub parse_input {
    my ($text) = @_;
    my @numbers = map { int($_) } grep { /\S/ } split /\n/, $text;
    return \@numbers;
}

sub mix {
    my ($numbers, $times) = @_;
    $times //= 1;
    my $n = scalar @$numbers;

    # Store [original_index, value] pairs
    my @indexed = map { [$_, $numbers->[$_]] } (0 .. $n - 1);

    for my $round (1 .. $times) {
        for my $orig_idx (0 .. $n - 1) {
            # Find current position of this element
            my $curr_pos;
            for my $i (0 .. $#indexed) {
                if ($indexed[$i][0] == $orig_idx) {
                    $curr_pos = $i;
                    last;
                }
            }

            # Get the value
            my $val = $indexed[$curr_pos][1];

            # Remove from current position
            my $elem = splice(@indexed, $curr_pos, 1);

            # Calculate new position (modulo n-1 because we removed the element)
            my $new_pos = ($curr_pos + $val) % ($n - 1);

            # Insert at new position
            splice(@indexed, $new_pos, 0, $elem);
        }
    }

    return [map { $_->[1] } @indexed];
}

sub grove_coordinates {
    my ($mixed) = @_;
    my $n = scalar @$mixed;

    # Find index of 0
    my $zero_idx;
    for my $i (0 .. $#$mixed) {
        if ($mixed->[$i] == 0) {
            $zero_idx = $i;
            last;
        }
    }

    my $sum = 0;
    for my $offset (1000, 2000, 3000) {
        $sum += $mixed->[($zero_idx + $offset) % $n];
    }
    return $sum;
}

sub part1 {
    my ($text) = @_;
    my $numbers = parse_input($text);
    my $mixed = mix($numbers, 1);
    return grove_coordinates($mixed);
}

sub part2 {
    my ($text) = @_;
    my $numbers = parse_input($text);
    my $decryption_key = 811589153;
    my @scaled = map { $_ * $decryption_key } @$numbers;
    my $mixed = mix(\@scaled, 10);
    return grove_coordinates($mixed);
}

sub main {
    my $script_dir = dirname(__FILE__);
    my $input_file = "$script_dir/../input.txt";

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    print "Part 1: ", part1($text), "\n";
    print "Part 2: ", part2($text), "\n";
}

main();
