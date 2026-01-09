#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

# Global hashes for monkeys and memoization
my %monkeys;
my %eval_memo;
my %humn_memo;

sub parse_input {
    my ($text) = @_;
    %monkeys = ();

    for my $line (split /\n/, $text) {
        next unless $line =~ /\S/;
        my ($name, $job) = split /:\s*/, $line;
        my @parts = split /\s+/, $job;

        if (@parts == 1) {
            $monkeys{$name} = $parts[0] + 0;  # Store as number
        } else {
            $monkeys{$name} = [$parts[0], $parts[1], $parts[2]];  # [left, op, right]
        }
    }
}

sub evaluate {
    my ($name) = @_;

    return $eval_memo{$name} if exists $eval_memo{$name};

    my $job = $monkeys{$name};

    # If it's just a number
    unless (ref $job) {
        return $job;
    }

    my ($left, $op, $right) = @$job;
    my $left_val = evaluate($left);
    my $right_val = evaluate($right);

    my $result;
    if ($op eq '+') {
        $result = $left_val + $right_val;
    } elsif ($op eq '-') {
        $result = $left_val - $right_val;
    } elsif ($op eq '*') {
        $result = $left_val * $right_val;
    } elsif ($op eq '/') {
        $result = int($left_val / $right_val);
    }

    $eval_memo{$name} = $result;
    return $result;
}

sub contains_humn {
    my ($name) = @_;

    return $humn_memo{$name} if exists $humn_memo{$name};

    if ($name eq 'humn') {
        return 1;
    }

    my $job = $monkeys{$name};

    # If it's just a number
    unless (ref $job) {
        $humn_memo{$name} = 0;
        return 0;
    }

    my ($left, $op, $right) = @$job;
    my $result = contains_humn($left) || contains_humn($right);
    $humn_memo{$name} = $result;
    return $result;
}

sub solve_for_humn {
    my ($name, $target) = @_;

    if ($name eq 'humn') {
        return $target;
    }

    my $job = $monkeys{$name};

    # If it's just a number, can't solve
    unless (ref $job) {
        return undef;
    }

    my ($left, $op, $right) = @$job;

    my $left_has_humn = contains_humn($left);

    if ($left_has_humn) {
        # Evaluate right side to get its value
        my $right_val = evaluate($right);
        my $new_target;

        if ($op eq '+') {
            # left + right = target => left = target - right
            $new_target = $target - $right_val;
        } elsif ($op eq '-') {
            # left - right = target => left = target + right
            $new_target = $target + $right_val;
        } elsif ($op eq '*') {
            # left * right = target => left = target / right
            $new_target = int($target / $right_val);
        } elsif ($op eq '/') {
            # left / right = target => left = target * right
            $new_target = $target * $right_val;
        }

        return solve_for_humn($left, $new_target);
    } else {
        # Evaluate left side to get its value
        my $left_val = evaluate($left);
        my $new_target;

        if ($op eq '+') {
            # left + right = target => right = target - left
            $new_target = $target - $left_val;
        } elsif ($op eq '-') {
            # left - right = target => right = left - target
            $new_target = $left_val - $target;
        } elsif ($op eq '*') {
            # left * right = target => right = target / left
            $new_target = int($target / $left_val);
        } elsif ($op eq '/') {
            # left / right = target => right = left / target
            $new_target = int($left_val / $target);
        }

        return solve_for_humn($right, $new_target);
    }
}

sub part1 {
    my ($text) = @_;
    parse_input($text);
    %eval_memo = ();
    %humn_memo = ();
    return evaluate('root');
}

sub part2 {
    my ($text) = @_;
    parse_input($text);
    %eval_memo = ();
    %humn_memo = ();

    # root checks equality between its two children
    my $job = $monkeys{'root'};
    my ($left, $op, $right) = @$job;

    my $left_has_humn = contains_humn($left);

    if ($left_has_humn) {
        # Right side gives us the target value
        my $target = evaluate($right);
        return solve_for_humn($left, $target);
    } else {
        # Left side gives us the target value
        my $target = evaluate($left);
        return solve_for_humn($right, $target);
    }
}

sub main {
    my $script_dir = dirname(__FILE__);
    my $input_file = "$script_dir/../input.txt";

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    print "Part 1: ", part1($text), "\n";
    print "Part 2: ", part2($text), "\n";
}

main();
