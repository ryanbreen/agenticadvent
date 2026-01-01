#!/usr/bin/env perl
# Day 19: Aplenty - Workflow processing and range analysis
use strict;
use warnings;
use File::Basename;

my $dir = dirname(__FILE__);
my $input_file = "$dir/../input.txt";

# Parse input
sub parse_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    local $/;
    my $text = <$fh>;
    close $fh;

    $text =~ s/\s+$//;
    my ($workflow_section, $parts_section) = split /\n\n/, $text;

    # Parse workflows
    my %workflows;
    for my $line (split /\n/, $workflow_section) {
        my ($name, $rules_str) = $line =~ /^(\w+)\{(.+)\}$/;
        my @rules;
        for my $rule (split /,/, $rules_str) {
            if ($rule =~ /:/) {
                my ($condition, $destination) = split /:/, $rule;
                my ($attr, $op, $value) = $condition =~ /^([xmas])([<>])(\d+)$/;
                push @rules, [$attr, $op, int($value), $destination];
            } else {
                # Default rule
                push @rules, [undef, undef, undef, $rule];
            }
        }
        $workflows{$name} = \@rules;
    }

    # Parse parts
    my @parts;
    for my $line (split /\n/, $parts_section) {
        my %part;
        while ($line =~ /([xmas])=(\d+)/g) {
            $part{$1} = int($2);
        }
        push @parts, \%part;
    }

    return (\%workflows, \@parts);
}

# Process a single part through workflows
sub process_part {
    my ($workflows, $part) = @_;
    my $current = 'in';

    while ($current ne 'A' && $current ne 'R') {
        for my $rule (@{$workflows->{$current}}) {
            my ($attr, $op, $value, $destination) = @$rule;
            if (!defined $attr) {
                # Default rule
                $current = $destination;
                last;
            } elsif ($op eq '<' && $part->{$attr} < $value) {
                $current = $destination;
                last;
            } elsif ($op eq '>' && $part->{$attr} > $value) {
                $current = $destination;
                last;
            }
        }
    }

    return $current eq 'A';
}

# Part 1: Sum ratings of accepted parts
sub part1 {
    my ($workflows, $parts) = @_;
    my $total = 0;

    for my $part (@$parts) {
        if (process_part($workflows, $part)) {
            $total += $part->{x} + $part->{m} + $part->{a} + $part->{s};
        }
    }

    return $total;
}

# Count accepted combinations using range splitting
sub count_accepted {
    my ($workflows, $workflow, $ranges) = @_;

    return 0 if $workflow eq 'R';

    if ($workflow eq 'A') {
        # Count all combinations in current ranges
        my $result = 1;
        for my $attr (qw(x m a s)) {
            my ($lo, $hi) = @{$ranges->{$attr}};
            my $range_size = $hi - $lo + 1;
            $result *= ($range_size > 0 ? $range_size : 0);
        }
        return $result;
    }

    my $total = 0;
    # Make a copy of ranges
    my %ranges_copy = map { $_ => [@{$ranges->{$_}}] } keys %$ranges;

    for my $rule (@{$workflows->{$workflow}}) {
        my ($attr, $op, $value, $destination) = @$rule;

        if (!defined $attr) {
            # Default rule
            $total += count_accepted($workflows, $destination, \%ranges_copy);
        } else {
            my ($lo, $hi) = @{$ranges_copy{$attr}};

            if ($op eq '<') {
                # Split: [lo, value-1] goes to destination, [value, hi] continues
                if ($lo < $value) {
                    # Part that matches the condition
                    my %new_ranges = map { $_ => [@{$ranges_copy{$_}}] } keys %ranges_copy;
                    $new_ranges{$attr} = [$lo, $hi < $value - 1 ? $hi : $value - 1];
                    $total += count_accepted($workflows, $destination, \%new_ranges);
                }
                # Remaining part continues to next rule
                if ($hi >= $value) {
                    $ranges_copy{$attr} = [$lo > $value ? $lo : $value, $hi];
                } else {
                    last;  # No remaining range
                }
            } else {
                # op eq '>'
                # Split: [value+1, hi] goes to destination, [lo, value] continues
                if ($hi > $value) {
                    # Part that matches the condition
                    my %new_ranges = map { $_ => [@{$ranges_copy{$_}}] } keys %ranges_copy;
                    $new_ranges{$attr} = [$lo > $value + 1 ? $lo : $value + 1, $hi];
                    $total += count_accepted($workflows, $destination, \%new_ranges);
                }
                # Remaining part continues to next rule
                if ($lo <= $value) {
                    $ranges_copy{$attr} = [$lo, $hi < $value ? $hi : $value];
                } else {
                    last;  # No remaining range
                }
            }
        }
    }

    return $total;
}

# Part 2: Count all possible accepted combinations
sub part2 {
    my ($workflows) = @_;
    my %initial_ranges = (
        x => [1, 4000],
        m => [1, 4000],
        a => [1, 4000],
        s => [1, 4000],
    );
    return count_accepted($workflows, 'in', \%initial_ranges);
}

# Main
my ($workflows, $parts) = parse_input($input_file);
print "Part 1: ", part1($workflows, $parts), "\n";
print "Part 2: ", part2($workflows), "\n";
