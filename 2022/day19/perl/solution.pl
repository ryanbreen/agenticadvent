#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use Cwd qw(abs_path);

# Read input
my $dir = dirname(abs_path($0));
open my $fh, '<', "$dir/../input.txt" or die "Cannot open input: $!";
my @lines = <$fh>;
close $fh;

# Parse blueprints
my @blueprints;
for my $line (@lines) {
    if ($line =~ /Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\./) {
        push @blueprints, [$1, $2, $3, $4, $5, $6, $7];
    }
}

sub max {
    my ($a, $b) = @_;
    return $a > $b ? $a : $b;
}

sub min {
    my ($a, $b) = @_;
    return $a < $b ? $a : $b;
}

sub max_geodes {
    my ($bp, $time_limit) = @_;
    my ($bp_id, $ore_ore, $clay_ore, $obs_ore, $obs_clay, $geo_ore, $geo_obs) = @$bp;

    # Max robots needed per type
    my $max_ore = max(max($ore_ore, $clay_ore), max($obs_ore, $geo_ore));
    my $max_clay = $obs_clay;
    my $max_obs = $geo_obs;

    my $best = 0;
    my %seen;

    # DFS with aggressive pruning
    my $dfs;
    $dfs = sub {
        my ($time, $ore, $clay, $obs, $geodes, $ore_r, $clay_r, $obs_r, $geo_r) = @_;

        # Pruning: upper bound on possible geodes
        my $remaining = $time_limit - $time;
        my $upper_bound = $geodes + $geo_r * $remaining + ($remaining * ($remaining - 1)) / 2;
        return if $upper_bound <= $best;

        if ($time == $time_limit) {
            $best = $geodes if $geodes > $best;
            return;
        }

        # Cap resources
        my $capped_ore = min($ore, $remaining * $max_ore);
        my $capped_clay = min($clay, $remaining * $max_clay);
        my $capped_obs = min($obs, $remaining * $max_obs);

        # State deduplication
        my $key = "$time,$capped_ore,$capped_clay,$capped_obs,$ore_r,$clay_r,$obs_r,$geo_r";
        return if exists $seen{$key} && $seen{$key} >= $geodes;
        $seen{$key} = $geodes;

        # Collect resources
        my $new_ore = $capped_ore + $ore_r;
        my $new_clay = $capped_clay + $clay_r;
        my $new_obs = $capped_obs + $obs_r;
        my $new_geodes = $geodes + $geo_r;

        # Try building geode robot (always do if possible)
        if ($capped_ore >= $geo_ore && $capped_obs >= $geo_obs) {
            $dfs->($time + 1, $new_ore - $geo_ore, $new_clay, $new_obs - $geo_obs, $new_geodes,
                   $ore_r, $clay_r, $obs_r, $geo_r + 1);
            return;  # If we can build geode, always do
        }

        # Try building obsidian robot
        if ($capped_ore >= $obs_ore && $capped_clay >= $obs_clay && $obs_r < $max_obs) {
            $dfs->($time + 1, $new_ore - $obs_ore, $new_clay - $obs_clay, $new_obs, $new_geodes,
                   $ore_r, $clay_r, $obs_r + 1, $geo_r);
        }

        # Try building clay robot
        if ($capped_ore >= $clay_ore && $clay_r < $max_clay) {
            $dfs->($time + 1, $new_ore - $clay_ore, $new_clay, $new_obs, $new_geodes,
                   $ore_r, $clay_r + 1, $obs_r, $geo_r);
        }

        # Try building ore robot
        if ($capped_ore >= $ore_ore && $ore_r < $max_ore) {
            $dfs->($time + 1, $new_ore - $ore_ore, $new_clay, $new_obs, $new_geodes,
                   $ore_r + 1, $clay_r, $obs_r, $geo_r);
        }

        # Do nothing (wait)
        $dfs->($time + 1, $new_ore, $new_clay, $new_obs, $new_geodes,
               $ore_r, $clay_r, $obs_r, $geo_r);
    };

    $dfs->(0, 0, 0, 0, 0, 1, 0, 0, 0);
    return $best;
}

sub part1 {
    my $total = 0;
    for my $bp (@blueprints) {
        my $geodes = max_geodes($bp, 24);
        $total += $bp->[0] * $geodes;
    }
    return $total;
}

sub part2 {
    my @first_three = @blueprints[0..2];
    my $result = 1;
    for my $bp (@first_three) {
        my $geodes = max_geodes($bp, 32);
        $result *= $geodes;
    }
    return $result;
}

print "Part 1: ", part1(), "\n";
print "Part 2: ", part2(), "\n";
