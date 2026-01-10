#!/usr/bin/env perl
use strict;
use warnings;

my $input_path = $0;
$input_path =~ s|[^/]+$||;
$input_path .= '../input.txt';

open my $fh, '<', $input_path or die "Cannot open $input_path: $!";
my @commands;
while (my $line = <$fh>) {
    chomp $line;
    next unless $line =~ /\S/;
    my ($cmd, $val) = split /\s+/, $line;
    push @commands, [$cmd, $val];
}
close $fh;

sub part1 {
    my ($commands) = @_;
    my $horizontal = 0;
    my $depth = 0;

    for my $entry (@$commands) {
        my ($cmd, $val) = @$entry;
        if ($cmd eq 'forward') {
            $horizontal += $val;
        } elsif ($cmd eq 'down') {
            $depth += $val;
        } elsif ($cmd eq 'up') {
            $depth -= $val;
        }
    }
    return $horizontal * $depth;
}

sub part2 {
    my ($commands) = @_;
    my $horizontal = 0;
    my $depth = 0;
    my $aim = 0;

    for my $entry (@$commands) {
        my ($cmd, $val) = @$entry;
        if ($cmd eq 'forward') {
            $horizontal += $val;
            $depth += $aim * $val;
        } elsif ($cmd eq 'down') {
            $aim += $val;
        } elsif ($cmd eq 'up') {
            $aim -= $val;
        }
    }
    return $horizontal * $depth;
}

print "Part 1: " . part1(\@commands) . "\n";
print "Part 2: " . part2(\@commands) . "\n";
