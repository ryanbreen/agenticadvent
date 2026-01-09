#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

# Read input
my $script_dir = dirname(__FILE__);
my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');

open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my @instructions = <$fh>;
close $fh;
chomp @instructions;

# Simulate CPU and collect (cycle, X) pairs for each cycle
sub simulate_cpu {
    my ($instructions) = @_;
    my @cycles;
    my $x = 1;
    my $cycle = 0;

    for my $line (@$instructions) {
        if ($line eq 'noop') {
            $cycle++;
            push @cycles, [$cycle, $x];
        } else {
            # addx V
            my ($v) = $line =~ /addx\s+(-?\d+)/;
            $cycle++;
            push @cycles, [$cycle, $x];
            $cycle++;
            push @cycles, [$cycle, $x];
            $x += $v;
        }
    }

    return \@cycles;
}

# Part 1: Sum signal strengths at cycles 20, 60, 100, 140, 180, 220
sub part1 {
    my ($instructions) = @_;
    my %target_cycles = map { $_ => 1 } (20, 60, 100, 140, 180, 220);
    my $total = 0;

    my $cycles = simulate_cpu($instructions);
    for my $entry (@$cycles) {
        my ($cycle, $x) = @$entry;
        if (exists $target_cycles{$cycle}) {
            $total += $cycle * $x;
        }
    }

    return $total;
}

# Part 2: Render CRT display. Sprite is 3 pixels wide centered at X
sub part2 {
    my ($instructions) = @_;
    my @screen;
    my @row;

    my $cycles = simulate_cpu($instructions);
    for my $entry (@$cycles) {
        my ($cycle, $x) = @$entry;
        my $pos = ($cycle - 1) % 40;  # CRT position in current row

        if (abs($pos - $x) <= 1) {
            push @row, '#';
        } else {
            push @row, '.';
        }

        if ($cycle % 40 == 0) {
            push @screen, join('', @row);
            @row = ();
        }
    }

    return join("\n", @screen);
}

# Main
print "Part 1: ", part1(\@instructions), "\n";
print "Part 2:\n";
print part2(\@instructions), "\n";
