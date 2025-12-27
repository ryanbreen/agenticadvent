#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

# Read input file
my $script_dir = dirname(__FILE__);
my $input_path = File::Spec->catfile($script_dir, '..', 'input.txt');
open my $fh, '<', $input_path or die "Cannot open $input_path: $!";
my $text = do { local $/; <$fh> };
close $fh;

# Parse input
my @sections = split /\n\n/, $text;

# Parse seeds
my ($seed_line) = $sections[0] =~ /seeds:\s+(.+)/;
my @seeds = split /\s+/, $seed_line;

# Parse maps
my @maps;
for my $i (1 .. $#sections) {
    my @lines = split /\n/, $sections[$i];
    shift @lines;  # Skip header
    my @ranges;
    for my $line (@lines) {
        next unless $line =~ /\S/;
        my @nums = split /\s+/, $line;
        push @ranges, [$nums[0], $nums[1], $nums[2]];  # dst_start, src_start, length
    }
    push @maps, \@ranges;
}

# Part 1: Apply maps to each seed and find minimum location
sub apply_map {
    my ($value, $ranges) = @_;
    for my $range (@$ranges) {
        my ($dst_start, $src_start, $length) = @$range;
        if ($value >= $src_start && $value < $src_start + $length) {
            return $dst_start + ($value - $src_start);
        }
    }
    return $value;
}

sub seed_to_location {
    my ($seed, $maps) = @_;
    my $value = $seed;
    for my $map_ranges (@$maps) {
        $value = apply_map($value, $map_ranges);
    }
    return $value;
}

sub part1 {
    my ($seeds, $maps) = @_;
    my $min_loc;
    for my $seed (@$seeds) {
        my $loc = seed_to_location($seed, $maps);
        $min_loc = $loc if !defined($min_loc) || $loc < $min_loc;
    }
    return $min_loc;
}

# Part 2: Work with ranges
sub apply_map_to_ranges {
    my ($input_ranges, $map_ranges) = @_;
    my @result;

    for my $input_range (@$input_ranges) {
        my ($start, $end) = @$input_range;
        my @remaining = ([$start, $end]);

        for my $map_range (@$map_ranges) {
            my ($dst_start, $src_start, $length) = @$map_range;
            my $src_end = $src_start + $length;
            my @new_remaining;

            for my $rem (@remaining) {
                my ($r_start, $r_end) = @$rem;

                # Part before the map range (unmapped)
                if ($r_start < $src_start) {
                    push @new_remaining, [$r_start, $r_end < $src_start ? $r_end : $src_start];
                }

                # Part within the map range (mapped)
                my $overlap_start = $r_start > $src_start ? $r_start : $src_start;
                my $overlap_end = $r_end < $src_end ? $r_end : $src_end;
                if ($overlap_start < $overlap_end) {
                    my $offset = $dst_start - $src_start;
                    push @result, [$overlap_start + $offset, $overlap_end + $offset];
                }

                # Part after the map range (unmapped)
                if ($r_end > $src_end) {
                    my $after_start = $r_start > $src_end ? $r_start : $src_end;
                    push @new_remaining, [$after_start, $r_end];
                }
            }

            @remaining = @new_remaining;
        }

        # Any remaining parts are unmapped (identity)
        push @result, @remaining;
    }

    return \@result;
}

sub part2 {
    my ($seeds, $maps) = @_;

    # Convert seeds to ranges: pairs of (start, start + length)
    my @ranges;
    for (my $i = 0; $i < @$seeds; $i += 2) {
        my $start = $seeds->[$i];
        my $length = $seeds->[$i + 1];
        push @ranges, [$start, $start + $length];
    }

    # Apply each map to the ranges
    my $current_ranges = \@ranges;
    for my $map_ranges (@$maps) {
        $current_ranges = apply_map_to_ranges($current_ranges, $map_ranges);
    }

    # Find minimum start of any range
    my $min_start;
    for my $range (@$current_ranges) {
        my $start = $range->[0];
        $min_start = $start if !defined($min_start) || $start < $min_start;
    }

    return $min_start;
}

# Run both parts
print "Part 1: ", part1(\@seeds, \@maps), "\n";
print "Part 2: ", part2(\@seeds, \@maps), "\n";
