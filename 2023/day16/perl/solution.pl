#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

# Directions: 0=right, 1=down, 2=left, 3=up
my @dr = (0, 1, 0, -1);
my @dc = (1, 0, -1, 0);

# Direction mappings for mirrors
my @slash_map = (3, 2, 1, 0);     # / mirror
my @backslash_map = (1, 0, 3, 2); # \ mirror

sub count_energized {
    my ($grid, $start_row, $start_col, $start_dir) = @_;
    my $rows = scalar @$grid;
    my $cols = length($grid->[0]);

    my %visited;
    my @queue = ([$start_row, $start_col, $start_dir]);

    while (@queue) {
        my $state = shift @queue;
        my ($r, $c, $d) = @$state;

        # Bounds check
        next if $r < 0 || $r >= $rows || $c < 0 || $c >= $cols;

        # Already visited this state
        my $key = "$r,$c,$d";
        next if exists $visited{$key};
        $visited{$key} = 1;

        my $cell = substr($grid->[$r], $c, 1);
        my @next_dirs;

        if ($cell eq '.') {
            @next_dirs = ($d);
        }
        elsif ($cell eq '/') {
            @next_dirs = ($slash_map[$d]);
        }
        elsif ($cell eq '\\') {
            @next_dirs = ($backslash_map[$d]);
        }
        elsif ($cell eq '|') {
            if ($d == 0 || $d == 2) {  # horizontal beam
                @next_dirs = (1, 3);   # split to up and down
            } else {
                @next_dirs = ($d);     # pass through
            }
        }
        elsif ($cell eq '-') {
            if ($d == 1 || $d == 3) {  # vertical beam
                @next_dirs = (0, 2);   # split to left and right
            } else {
                @next_dirs = ($d);     # pass through
            }
        }

        for my $nd (@next_dirs) {
            push @queue, [$r + $dr[$nd], $c + $dc[$nd], $nd];
        }
    }

    # Count unique positions (ignore direction)
    my %positions;
    for my $key (keys %visited) {
        my ($r, $c, $d) = split /,/, $key;
        $positions{"$r,$c"} = 1;
    }

    return scalar keys %positions;
}

sub part1 {
    my ($grid) = @_;
    return count_energized($grid, 0, 0, 0);  # Start at (0,0) heading right
}

sub part2 {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    my $cols = length($grid->[0]);
    my $max_energized = 0;

    # Top row, heading down
    for my $c (0 .. $cols - 1) {
        my $e = count_energized($grid, 0, $c, 1);
        $max_energized = $e if $e > $max_energized;
    }

    # Bottom row, heading up
    for my $c (0 .. $cols - 1) {
        my $e = count_energized($grid, $rows - 1, $c, 3);
        $max_energized = $e if $e > $max_energized;
    }

    # Left column, heading right
    for my $r (0 .. $rows - 1) {
        my $e = count_energized($grid, $r, 0, 0);
        $max_energized = $e if $e > $max_energized;
    }

    # Right column, heading left
    for my $r (0 .. $rows - 1) {
        my $e = count_energized($grid, $r, $cols - 1, 2);
        $max_energized = $e if $e > $max_energized;
    }

    return $max_energized;
}

sub main {
    my $dir = dirname(__FILE__);
    my $input_file = "$dir/../input.txt";

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my @grid;
    while (my $line = <$fh>) {
        chomp $line;
        push @grid, $line if $line ne '';
    }
    close $fh;

    print "Part 1: ", part1(\@grid), "\n";
    print "Part 2: ", part2(\@grid), "\n";
}

main();
