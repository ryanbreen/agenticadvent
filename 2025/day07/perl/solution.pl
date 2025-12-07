#!/usr/bin/env perl
use strict;
use warnings;
use Math::BigInt;

# Read input from ../input.txt
my $input_file = '../input.txt';
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close($fh);
chomp(@lines);

sub part1 {
    my $rows = scalar(@lines);
    my $cols = $rows > 0 ? length($lines[0]) : 0;

    # Find starting position S in row 0
    my $start_col;
    for my $col (0..$cols-1) {
        if (substr($lines[0], $col, 1) eq 'S') {
            $start_col = $col;
            last;
        }
    }

    return 0 unless defined $start_col;

    # Track active beam columns at each row
    # Use a hash to handle beam merging (like a set)
    my %active_beams = ($start_col => 1);
    my $split_count = 0;

    # Process row by row starting from row 1 (below S)
    for my $row (1..$rows-1) {
        my %new_beams;

        for my $col (keys %active_beams) {
            if ($col >= 0 && $col < $cols) {
                my $cell = substr($lines[$row], $col, 1);

                if ($cell eq '^') {
                    # Beam hits splitter - count it and emit left/right
                    $split_count++;
                    # Left beam goes to col-1, right beam goes to col+1
                    $new_beams{$col - 1} = 1 if $col - 1 >= 0;
                    $new_beams{$col + 1} = 1 if $col + 1 < $cols;
                }
                elsif ($cell eq '.') {
                    # Beam continues straight down
                    $new_beams{$col} = 1;
                }
                else {
                    # If cell is something else (like S), beam continues
                    $new_beams{$col} = 1;
                }
            }
        }

        %active_beams = %new_beams;

        # If no more beams, stop
        last unless %active_beams;
    }

    return $split_count;
}

sub part2 {
    my $rows = scalar(@lines);
    my $cols = $rows > 0 ? length($lines[0]) : 0;

    # Find starting position S in row 0
    my $start_col;
    for my $col (0..$cols-1) {
        if (substr($lines[0], $col, 1) eq 'S') {
            $start_col = $col;
            last;
        }
    }

    return Math::BigInt->new(0) unless defined $start_col;

    # Track number of timelines at each column position
    # Use a hash: col -> count of timelines at that position
    my %timelines = ($start_col => Math::BigInt->new(1));

    # Process row by row starting from row 1 (below S)
    for my $row (1..$rows-1) {
        my %new_timelines;

        for my $col (keys %timelines) {
            my $count = $timelines{$col};

            if ($col >= 0 && $col < $cols) {
                my $cell = substr($lines[$row], $col, 1);

                if ($cell eq '^') {
                    # Each timeline splits into 2 (left and right)
                    if ($col - 1 >= 0) {
                        $new_timelines{$col - 1} = Math::BigInt->new(0) unless exists $new_timelines{$col - 1};
                        $new_timelines{$col - 1} += $count;
                    }
                    if ($col + 1 < $cols) {
                        $new_timelines{$col + 1} = Math::BigInt->new(0) unless exists $new_timelines{$col + 1};
                        $new_timelines{$col + 1} += $count;
                    }
                }
                elsif ($cell eq '.') {
                    # Timelines continue straight down
                    $new_timelines{$col} = Math::BigInt->new(0) unless exists $new_timelines{$col};
                    $new_timelines{$col} += $count;
                }
                else {
                    # Other characters - timelines continue
                    $new_timelines{$col} = Math::BigInt->new(0) unless exists $new_timelines{$col};
                    $new_timelines{$col} += $count;
                }
            }
        }

        %timelines = %new_timelines;

        # If no more timelines, stop
        last unless %timelines;
    }

    # Total number of timelines
    my $total = Math::BigInt->new(0);
    for my $count (values %timelines) {
        $total += $count;
    }

    return $total;
}

print "Part 1: ", part1(), "\n";
print "Part 2: ", part2(), "\n";
