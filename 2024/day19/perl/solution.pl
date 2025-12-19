#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

my $dir = dirname(__FILE__);
my $input_file = File::Spec->catfile($dir, '..', 'input.txt');

open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my $input = do { local $/; <$fh> };
close $fh;

$input =~ s/\s+$//;

my ($patterns_str, $designs_str) = split /\n\n/, $input, 2;
my @patterns = map { s/^\s+|\s+$//gr } split /,/, $patterns_str;
my @designs = split /\n/, $designs_str;

sub can_form {
    my ($design) = @_;
    my %memo;

    my $dp;
    $dp = sub {
        my ($pos) = @_;
        return 1 if $pos == length($design);
        return $memo{$pos} if exists $memo{$pos};

        for my $pattern (@patterns) {
            my $plen = length($pattern);
            if (substr($design, $pos, $plen) eq $pattern) {
                if ($dp->($pos + $plen)) {
                    $memo{$pos} = 1;
                    return 1;
                }
            }
        }
        $memo{$pos} = 0;
        return 0;
    };

    return $dp->(0);
}

sub count_ways {
    my ($design) = @_;
    my %memo;

    my $dp;
    $dp = sub {
        my ($pos) = @_;
        return 1 if $pos == length($design);
        return $memo{$pos} if exists $memo{$pos};

        my $total = 0;
        for my $pattern (@patterns) {
            my $plen = length($pattern);
            if (substr($design, $pos, $plen) eq $pattern) {
                $total += $dp->($pos + $plen);
            }
        }
        $memo{$pos} = $total;
        return $total;
    };

    return $dp->(0);
}

sub part1 {
    my $count = 0;
    for my $design (@designs) {
        $count++ if can_form($design);
    }
    return $count;
}

sub part2 {
    my $total = 0;
    for my $design (@designs) {
        $total += count_ways($design);
    }
    return $total;
}

print "Part 1: ", part1(), "\n";
print "Part 2: ", part2(), "\n";
