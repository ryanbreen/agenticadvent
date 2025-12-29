#!/usr/bin/env perl
use strict;
use warnings;

sub parse_grid {
    my ($lines) = @_;
    my @galaxies;

    for my $r (0 .. $#$lines) {
        my @chars = split //, $lines->[$r];
        for my $c (0 .. $#chars) {
            if ($chars[$c] eq '#') {
                push @galaxies, [$r, $c];
            }
        }
    }

    return \@galaxies;
}

sub find_empty_rows_and_cols {
    my ($lines) = @_;
    my %empty_rows;
    my %empty_cols;

    my $rows = scalar @$lines;
    my $cols = length($lines->[0]) if $rows > 0;

    # Find empty rows
    for my $r (0 .. $rows - 1) {
        if ($lines->[$r] !~ /#/) {
            $empty_rows{$r} = 1;
        }
    }

    # Find empty columns
    for my $c (0 .. $cols - 1) {
        my $has_galaxy = 0;
        for my $r (0 .. $rows - 1) {
            if (substr($lines->[$r], $c, 1) eq '#') {
                $has_galaxy = 1;
                last;
            }
        }
        if (!$has_galaxy) {
            $empty_cols{$c} = 1;
        }
    }

    return (\%empty_rows, \%empty_cols);
}

sub calculate_distances {
    my ($galaxies, $empty_rows, $empty_cols, $expansion_factor) = @_;
    my $total = 0;

    my $n = scalar @$galaxies;

    for my $i (0 .. $n - 2) {
        for my $j ($i + 1 .. $n - 1) {
            my ($r1, $c1) = @{$galaxies->[$i]};
            my ($r2, $c2) = @{$galaxies->[$j]};

            # Calculate row distance with expansion
            my ($min_r, $max_r) = $r1 < $r2 ? ($r1, $r2) : ($r2, $r1);
            my $row_dist = $max_r - $min_r;
            for my $r ($min_r .. $max_r - 1) {
                if (exists $empty_rows->{$r}) {
                    $row_dist += $expansion_factor - 1;
                }
            }

            # Calculate column distance with expansion
            my ($min_c, $max_c) = $c1 < $c2 ? ($c1, $c2) : ($c2, $c1);
            my $col_dist = $max_c - $min_c;
            for my $c ($min_c .. $max_c - 1) {
                if (exists $empty_cols->{$c}) {
                    $col_dist += $expansion_factor - 1;
                }
            }

            $total += $row_dist + $col_dist;
        }
    }

    return $total;
}

sub part1 {
    my ($lines) = @_;
    my $galaxies = parse_grid($lines);
    my ($empty_rows, $empty_cols) = find_empty_rows_and_cols($lines);
    return calculate_distances($galaxies, $empty_rows, $empty_cols, 2);
}

sub part2 {
    my ($lines) = @_;
    my $galaxies = parse_grid($lines);
    my ($empty_rows, $empty_cols) = find_empty_rows_and_cols($lines);
    return calculate_distances($galaxies, $empty_rows, $empty_cols, 1000000);
}

sub main {
    my $input_file = $ARGV[0] // '../input.txt';

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my @lines = map { chomp; $_ } <$fh>;
    close $fh;

    # Remove trailing empty lines
    while (@lines && $lines[-1] eq '') {
        pop @lines;
    }

    print "Part 1: ", part1(\@lines), "\n";
    print "Part 2: ", part2(\@lines), "\n";
}

main();
