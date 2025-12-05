#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

sub part1 {
    my ($data) = @_;
    my $total = 0;

    # Find all valid mul(X,Y) instructions where X and Y are 1-3 digit numbers
    while ($data =~ /mul\((\d{1,3}),(\d{1,3})\)/g) {
        $total += $1 * $2;
    }

    return $total;
}

sub part2 {
    my ($data) = @_;
    my $total = 0;
    my $enabled = 1;

    # Build a list of all events with positions
    my @events;

    # Find all mul instructions
    while ($data =~ /mul\((\d{1,3}),(\d{1,3})\)/g) {
        push @events, [$-[0], 'mul', $1, $2];
    }

    # Find all do() instructions
    while ($data =~ /do\(\)/g) {
        push @events, [$-[0], 'do', 0, 0];
    }

    # Find all don't() instructions
    while ($data =~ /don't\(\)/g) {
        push @events, [$-[0], 'dont', 0, 0];
    }

    # Sort by position
    @events = sort { $a->[0] <=> $b->[0] } @events;

    # Process events in order
    for my $event (@events) {
        my ($pos, $type, $x, $y) = @$event;

        if ($type eq 'do') {
            $enabled = 1;
        } elsif ($type eq 'dont') {
            $enabled = 0;
        } elsif ($type eq 'mul' && $enabled) {
            $total += $x * $y;
        }
    }

    return $total;
}

sub main {
    # Get the directory of the script
    my $script_dir = dirname(__FILE__);
    my $input_path = File::Spec->catfile($script_dir, '..', 'input.txt');

    # Read the input file
    open my $fh, '<', $input_path or die "Cannot open $input_path: $!";
    my $data = do { local $/; <$fh> };
    close $fh;

    print "Part 1: ", part1($data), "\n";
    print "Part 2: ", part2($data), "\n";
}

main();
