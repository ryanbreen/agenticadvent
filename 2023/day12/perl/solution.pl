#!/usr/bin/env perl
# Advent of Code 2023 Day 12: Hot Springs

use strict;
use warnings;
no warnings 'recursion';

my %memo;

sub count_arrangements {
    my ($pattern, $groups) = @_;
    %memo = ();
    return dp($pattern, $groups, 0, 0, 0);
}

sub dp {
    my ($pattern, $groups, $pos, $group_idx, $current_run) = @_;

    my $key = "$pos,$group_idx,$current_run";
    return $memo{$key} if exists $memo{$key};

    my $pattern_len = length($pattern);
    my $num_groups = scalar(@$groups);

    # Base case: reached end of pattern
    if ($pos == $pattern_len) {
        # Valid if we've matched all groups and no partial run
        if ($group_idx == $num_groups && $current_run == 0) {
            $memo{$key} = 1;
            return 1;
        }
        # Or if we're on the last group and the run matches
        if ($group_idx == $num_groups - 1 && $groups->[$group_idx] == $current_run) {
            $memo{$key} = 1;
            return 1;
        }
        $memo{$key} = 0;
        return 0;
    }

    my $result = 0;
    my $char = substr($pattern, $pos, 1);

    # Option 1: Place operational spring (.)
    if ($char eq '.' || $char eq '?') {
        if ($current_run == 0) {
            # No active run, just move forward
            $result += dp($pattern, $groups, $pos + 1, $group_idx, 0);
        } elsif ($group_idx < $num_groups && $groups->[$group_idx] == $current_run) {
            # End current run if it matches expected group size
            $result += dp($pattern, $groups, $pos + 1, $group_idx + 1, 0);
        }
        # Otherwise invalid (run doesn't match group)
    }

    # Option 2: Place damaged spring (#)
    if ($char eq '#' || $char eq '?') {
        if ($group_idx < $num_groups && $current_run < $groups->[$group_idx]) {
            # Can extend current run
            $result += dp($pattern, $groups, $pos + 1, $group_idx, $current_run + 1);
        }
        # Otherwise invalid (exceeds group size or no more groups)
    }

    $memo{$key} = $result;
    return $result;
}

sub parse_line {
    my ($line) = @_;
    $line =~ s/^\s+|\s+$//g;
    my ($pattern, $groups_str) = split(/\s+/, $line);
    my @groups = split(/,/, $groups_str);
    return ($pattern, \@groups);
}

sub unfold {
    my ($pattern, $groups) = @_;
    my $unfolded_pattern = join('?', ($pattern) x 5);
    my @unfolded_groups = (@$groups) x 5;
    return ($unfolded_pattern, \@unfolded_groups);
}

sub part1 {
    my ($lines) = @_;
    my $total = 0;
    for my $line (@$lines) {
        next if $line =~ /^\s*$/;
        my ($pattern, $groups) = parse_line($line);
        $total += count_arrangements($pattern, $groups);
    }
    return $total;
}

sub part2 {
    my ($lines) = @_;
    my $total = 0;
    for my $line (@$lines) {
        next if $line =~ /^\s*$/;
        my ($pattern, $groups) = parse_line($line);
        my ($unfolded_pattern, $unfolded_groups) = unfold($pattern, $groups);
        $total += count_arrangements($unfolded_pattern, $unfolded_groups);
    }
    return $total;
}

sub main {
    open(my $fh, '<', '../input.txt') or die "Cannot open input.txt: $!";
    my @lines = <$fh>;
    close($fh);

    print "Part 1: " . part1(\@lines) . "\n";
    print "Part 2: " . part2(\@lines) . "\n";
}

main();
