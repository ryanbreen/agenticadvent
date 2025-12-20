#!/usr/bin/env perl
use strict;
use warnings;

sub parse_grid {
    my ($input) = @_;
    my @grid;
    my ($start, $end);

    my @lines = split /\n/, $input;
    for my $r (0 .. $#lines) {
        my @row = split //, $lines[$r];
        push @grid, \@row;
        for my $c (0 .. $#row) {
            if ($row[$c] eq 'S') {
                $start = [$r, $c];
            } elsif ($row[$c] eq 'E') {
                $end = [$r, $c];
            }
        }
    }

    return (\@grid, $start, $end);
}

sub trace_path {
    my ($grid, $start, $end) = @_;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};

    # dist stores distance from start for each track cell
    my %dist;
    my $start_key = "$start->[0],$start->[1]";
    $dist{$start_key} = 0;

    my @queue = ($start);
    my @dirs = ([-1, 0], [1, 0], [0, -1], [0, 1]);

    while (@queue) {
        my $pos = shift @queue;
        my ($r, $c) = @$pos;
        my $pos_key = "$r,$c";

        last if $r == $end->[0] && $c == $end->[1];

        for my $dir (@dirs) {
            my ($dr, $dc) = @$dir;
            my $nr = $r + $dr;
            my $nc = $c + $dc;
            my $nkey = "$nr,$nc";

            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols
                && $grid->[$nr][$nc] ne '#' && !exists $dist{$nkey}) {
                $dist{$nkey} = $dist{$pos_key} + 1;
                push @queue, [$nr, $nc];
            }
        }
    }

    return \%dist;
}

sub count_cheats {
    my ($dist, $max_cheat_time, $min_savings) = @_;
    my $count = 0;

    my @positions;
    for my $key (keys %$dist) {
        my ($r, $c) = split /,/, $key;
        push @positions, [$r, $c, $dist->{$key}];
    }

    for my $p1 (@positions) {
        my ($r1, $c1, $d1) = @$p1;
        for my $p2 (@positions) {
            my ($r2, $c2, $d2) = @$p2;

            # Manhattan distance is the cheat cost
            my $cheat_cost = abs($r2 - $r1) + abs($c2 - $c1);
            if ($cheat_cost <= $max_cheat_time) {
                my $savings = $d2 - $d1 - $cheat_cost;
                if ($savings >= $min_savings) {
                    $count++;
                }
            }
        }
    }

    return $count;
}

sub part1 {
    my ($grid, $start, $end) = @_;
    my $dist = trace_path($grid, $start, $end);
    return count_cheats($dist, 2, 100);
}

sub part2 {
    my ($grid, $start, $end) = @_;
    my $dist = trace_path($grid, $start, $end);
    return count_cheats($dist, 20, 100);
}

sub main {
    open my $fh, '<', '../input.txt' or die "Cannot open input: $!";
    my $input = do { local $/; <$fh> };
    close $fh;

    # Remove trailing newline if present
    chomp $input;

    my ($grid, $start, $end) = parse_grid($input);

    print "Part 1: ", part1($grid, $start, $end), "\n";
    print "Part 2: ", part2($grid, $start, $end), "\n";
}

main();
