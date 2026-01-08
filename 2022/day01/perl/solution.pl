#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

sub parse_input {
    my ($filename) = @_;

    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    my $content = do { local $/; <$fh> };
    close $fh;

    $content =~ s/\s+$//;  # Remove trailing whitespace

    my @elves;
    for my $group (split /\n\n/, $content) {
        my $total = 0;
        for my $line (split /\n/, $group) {
            $total += $line if $line =~ /^\d+$/;
        }
        push @elves, $total;
    }

    return @elves;
}

sub part1 {
    my @elves = @_;
    my $max = 0;
    for my $elf (@elves) {
        $max = $elf if $elf > $max;
    }
    return $max;
}

sub part2 {
    my @elves = @_;
    my @sorted = sort { $b <=> $a } @elves;
    return $sorted[0] + $sorted[1] + $sorted[2];
}

sub main {
    my $script_dir = dirname(File::Spec->rel2abs(__FILE__));
    my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');

    my @elves = parse_input($input_file);

    print "Part 1: ", part1(@elves), "\n";
    print "Part 2: ", part2(@elves), "\n";
}

main();
