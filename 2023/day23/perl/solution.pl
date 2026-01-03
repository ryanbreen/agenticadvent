#!/usr/bin/env perl
# Day 23: A Long Walk - Longest path through hiking trails
use strict;
use warnings;

# Parse the grid
sub parse_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    my @grid;
    while (<$fh>) {
        chomp;
        push @grid, $_;
    }
    close $fh;
    return \@grid;
}

# Find all junction points (start, end, and intersections with 3+ neighbors)
sub find_junctions {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    my $cols = length $grid->[0];
    my %junctions;

    # Find start (first . in top row)
    my $start_col = index($grid->[0], '.');
    my $start = "0,$start_col";
    $junctions{$start} = 1;

    # Find end (first . in bottom row)
    my $end_col = index($grid->[$rows - 1], '.');
    my $end = ($rows - 1) . ",$end_col";
    $junctions{$end} = 1;

    # Find intersections (cells with 3+ walkable neighbors)
    my @dirs = ([-1, 0], [1, 0], [0, -1], [0, 1]);
    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            next if substr($grid->[$r], $c, 1) eq '#';
            my $neighbors = 0;
            for my $d (@dirs) {
                my ($nr, $nc) = ($r + $d->[0], $c + $d->[1]);
                if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
                    $neighbors++ if substr($grid->[$nr], $nc, 1) ne '#';
                }
            }
            $junctions{"$r,$c"} = 1 if $neighbors >= 3;
        }
    }

    return (\%junctions, $start, $end);
}

# Build graph of junctions with edge weights
sub build_graph {
    my ($grid, $junctions, $respect_slopes) = @_;
    my $rows = scalar @$grid;
    my $cols = length $grid->[0];

    # Slope direction mappings
    my %slope_dirs = (
        '^' => [-1, 0],
        'v' => [1, 0],
        '<' => [0, -1],
        '>' => [0, 1]
    );

    my @dirs = ([-1, 0], [1, 0], [0, -1], [0, 1]);
    my %graph;

    for my $start_junction (keys %$junctions) {
        my ($sr, $sc) = split /,/, $start_junction;

        # DFS from this junction to find reachable junctions
        my @stack = ([$sr, $sc, 0]);
        my %visited;
        $visited{$start_junction} = 1;

        while (@stack) {
            my $item = pop @stack;
            my ($r, $c, $dist) = @$item;
            my $pos = "$r,$c";

            if ($dist > 0 && exists $junctions->{$pos}) {
                # Found another junction
                $graph{$start_junction}{$pos} = $dist;
                next;
            }

            # Explore neighbors
            for my $d (@dirs) {
                my ($dr, $dc) = @$d;
                my ($nr, $nc) = ($r + $dr, $c + $dc);

                next if $nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols;

                my $cell_at_nr_nc = substr($grid->[$nr], $nc, 1);
                next if $cell_at_nr_nc eq '#';

                my $npos = "$nr,$nc";
                next if exists $visited{$npos};

                # Check slope constraints for Part 1
                if ($respect_slopes) {
                    my $cell = substr($grid->[$r], $c, 1);
                    if (exists $slope_dirs{$cell}) {
                        my ($req_dr, $req_dc) = @{$slope_dirs{$cell}};
                        next if $dr != $req_dr || $dc != $req_dc;
                    }
                }

                $visited{$npos} = 1;
                push @stack, [$nr, $nc, $dist + 1];
            }
        }
    }

    return \%graph;
}

# Find longest path using DFS with backtracking
sub longest_path_dfs {
    my ($graph, $start, $end) = @_;
    my %visited;

    my $dfs;
    $dfs = sub {
        my ($node) = @_;
        return 0 if $node eq $end;

        $visited{$node} = 1;
        my $max_dist = -1e18;

        for my $neighbor (keys %{$graph->{$node} // {}}) {
            next if exists $visited{$neighbor};
            my $dist = $graph->{$node}{$neighbor};
            my $result = $dfs->($neighbor);
            if ($result > -1e17) {
                my $total = $dist + $result;
                $max_dist = $total if $total > $max_dist;
            }
        }

        delete $visited{$node};
        return $max_dist;
    };

    return $dfs->($start);
}

# Solve for either part
sub solve {
    my ($grid, $respect_slopes) = @_;
    my ($junctions, $start, $end) = find_junctions($grid);
    my $graph = build_graph($grid, $junctions, $respect_slopes);
    return longest_path_dfs($graph, $start, $end);
}

sub part1 {
    my ($grid) = @_;
    return solve($grid, 1);
}

sub part2 {
    my ($grid) = @_;
    return solve($grid, 0);
}

sub main {
    my $input_path = $0;
    $input_path =~ s|[^/]+$||;
    $input_path .= '../input.txt';

    my $grid = parse_input($input_path);
    print "Part 1: ", part1($grid), "\n";
    print "Part 2: ", part2($grid), "\n";
}

main();
