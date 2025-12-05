#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Read input file
my $input_file = '../input.txt';
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my $input_text = do { local $/; <$fh> };
close $fh;

# Remove trailing whitespace
$input_text =~ s/\s+$//;

# Parse input - split into rules and updates sections
my ($rules_section, $updates_section) = split /\n\n/, $input_text, 2;

# Parse rules: X|Y means X must come before Y
# Store as: rules{X} = set of pages that must come AFTER X
my %rules;
for my $rule (split /\n/, $rules_section) {
    my ($before, $after) = split /\|/, $rule;
    $rules{$before}{$after} = 1;
}

# Parse updates
my @updates;
for my $line (split /\n/, $updates_section) {
    my @pages = split /,/, $line;
    push @updates, \@pages;
}

sub is_valid_order {
    my ($update_ref) = @_;
    my @update = @$update_ref;

    # Create position map
    my %page_positions;
    for my $i (0 .. $#update) {
        $page_positions{$update[$i]} = $i;
    }

    # Check each page
    for my $i (0 .. $#update) {
        my $page = $update[$i];

        # Check all pages that must come after this page
        if (exists $rules{$page}) {
            for my $must_be_after (keys %{$rules{$page}}) {
                if (exists $page_positions{$must_be_after}) {
                    if ($page_positions{$must_be_after} < $i) {
                        return 0;
                    }
                }
            }
        }
    }
    return 1;
}

sub part1 {
    my $total = 0;
    for my $update (@updates) {
        if (is_valid_order($update)) {
            my $middle_idx = int(@$update / 2);
            $total += $update->[$middle_idx];
        }
    }
    return $total;
}

sub fix_order {
    my ($update_ref) = @_;
    my @update = @$update_ref;

    # Sort using custom comparator
    my @sorted = sort {
        # If a must come before b, return -1
        if (exists $rules{$a} && exists $rules{$a}{$b}) {
            return -1;
        }
        # If b must come before a, return 1
        if (exists $rules{$b} && exists $rules{$b}{$a}) {
            return 1;
        }
        return 0;
    } @update;

    return \@sorted;
}

sub part2 {
    my $total = 0;
    for my $update (@updates) {
        if (!is_valid_order($update)) {
            my $fixed = fix_order($update);
            my $middle_idx = int(@$fixed / 2);
            $total += $fixed->[$middle_idx];
        }
    }
    return $total;
}

print "Part 1: " . part1() . "\n";
print "Part 2: " . part2() . "\n";
