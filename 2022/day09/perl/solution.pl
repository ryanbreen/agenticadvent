#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use Cwd 'abs_path';

# Direction vectors
my %DIRECTIONS = (
    'U' => [0, 1],
    'D' => [0, -1],
    'L' => [-1, 0],
    'R' => [1, 0],
);

sub sign {
    my ($x) = @_;
    return 0 if $x == 0;
    return $x > 0 ? 1 : -1;
}

sub move_tail {
    my ($head, $tail) = @_;
    my $dx = $head->[0] - $tail->[0];
    my $dy = $head->[1] - $tail->[1];

    # If adjacent or overlapping, don't move
    if (abs($dx) <= 1 && abs($dy) <= 1) {
        return [$tail->[0], $tail->[1]];
    }

    # Move toward head
    return [$tail->[0] + sign($dx), $tail->[1] + sign($dy)];
}

sub simulate_rope {
    my ($moves, $rope_length) = @_;

    # Initialize all knots at origin
    my @knots;
    for (my $i = 0; $i < $rope_length; $i++) {
        push @knots, [0, 0];
    }

    # Track visited positions for tail
    my %visited;
    $visited{"0,0"} = 1;

    for my $line (@$moves) {
        my ($direction, $count) = split /\s+/, $line;
        my ($dx, $dy) = @{$DIRECTIONS{$direction}};

        for (my $step = 0; $step < $count; $step++) {
            # Move head
            $knots[0][0] += $dx;
            $knots[0][1] += $dy;

            # Move each subsequent knot
            for (my $i = 1; $i < $rope_length; $i++) {
                $knots[$i] = move_tail($knots[$i-1], $knots[$i]);
            }

            # Record tail position
            my $tail = $knots[-1];
            $visited{"$tail->[0],$tail->[1]"} = 1;
        }
    }

    return scalar keys %visited;
}

sub part1 {
    my ($moves) = @_;
    return simulate_rope($moves, 2);
}

sub part2 {
    my ($moves) = @_;
    return simulate_rope($moves, 10);
}

sub main {
    my $script_dir = dirname(abs_path($0));
    my $input_file = "$script_dir/../input.txt";

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my @moves = <$fh>;
    close $fh;

    chomp @moves;

    print "Part 1: ", part1(\@moves), "\n";
    print "Part 2: ", part2(\@moves), "\n";
}

main();
