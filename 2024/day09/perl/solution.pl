#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Advent of Code 2024 Day 9: Disk Fragmenter
#
# Compact a fragmented disk by moving file blocks to fill gaps.
# Part 1: Move blocks one at a time from end to leftmost free space
# Part 2: Move whole files to leftmost span that fits

sub parse_disk_map {
    my ($filename) = @_;

    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    my $disk_map = <$fh>;
    close $fh;
    chomp $disk_map;

    my @blocks;
    my $file_id = 0;
    my $is_file = 1;

    for my $digit (split //, $disk_map) {
        my $length = int($digit);
        if ($is_file) {
            push @blocks, ($file_id) x $length;
            $file_id++;
        } else {
            push @blocks, (-1) x $length;  # -1 represents free space
        }
        $is_file = !$is_file;
    }

    return \@blocks;
}

sub compact_blocks {
    my ($blocks_ref) = @_;
    my @blocks = @$blocks_ref;  # Copy the array

    my $left = 0;
    my $right = $#blocks;

    while ($left < $right) {
        # Find leftmost free space
        while ($left < $right && $blocks[$left] != -1) {
            $left++;
        }
        # Find rightmost file block
        while ($left < $right && $blocks[$right] == -1) {
            $right--;
        }

        if ($left < $right) {
            # Swap
            $blocks[$left] = $blocks[$right];
            $blocks[$right] = -1;
            $left++;
            $right--;
        }
    }

    return \@blocks;
}

sub calculate_checksum {
    my ($blocks_ref) = @_;
    my @blocks = @$blocks_ref;

    my $checksum = 0;
    for my $pos (0 .. $#blocks) {
        if ($blocks[$pos] != -1) {
            $checksum += $pos * $blocks[$pos];
        }
    }

    return $checksum;
}

sub part1 {
    my $blocks = parse_disk_map('../input.txt');
    my $compacted = compact_blocks($blocks);
    return calculate_checksum($compacted);
}

sub part2 {
    my $blocks_ref = parse_disk_map('../input.txt');
    my @blocks = @$blocks_ref;

    # Find all files: file_id -> [start_pos, length]
    my %files;
    my $i = 0;
    while ($i < @blocks) {
        if ($blocks[$i] != -1) {
            my $file_id = $blocks[$i];
            my $start = $i;
            while ($i < @blocks && $blocks[$i] == $file_id) {
                $i++;
            }
            $files{$file_id} = [$start, $i - $start];
        } else {
            $i++;
        }
    }

    # Find max file ID
    my $max_file_id = (sort { $b <=> $a } keys %files)[0];

    # Process files in decreasing order of file ID
    for (my $file_id = $max_file_id; $file_id >= 0; $file_id--) {
        next unless exists $files{$file_id};

        my ($start, $length) = @{$files{$file_id}};

        # Find leftmost span of free space that fits this file
        # Must be to the left of current position
        my $free_start = undef;
        $i = 0;
        while ($i < $start) {
            if ($blocks[$i] == -1) {
                # Count consecutive free blocks
                my $span_start = $i;
                my $span_length = 0;
                while ($i < $start && $blocks[$i] == -1) {
                    $span_length++;
                    $i++;
                }
                if ($span_length >= $length) {
                    $free_start = $span_start;
                    last;
                }
            } else {
                $i++;
            }
        }

        # Move file if we found a suitable span
        if (defined $free_start) {
            # Clear old position
            for my $j ($start .. $start + $length - 1) {
                $blocks[$j] = -1;
            }
            # Write to new position
            for my $j ($free_start .. $free_start + $length - 1) {
                $blocks[$j] = $file_id;
            }
            # Update file position
            $files{$file_id} = [$free_start, $length];
        }
    }

    return calculate_checksum(\@blocks);
}

say "Part 1: ", part1();
say "Part 2: ", part2();
