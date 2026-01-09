#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use Cwd 'abs_path';

sub parse_input {
    my ($filename) = @_;

    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    my $content = do { local $/; <$fh> };
    close $fh;

    my ($stack_section, $move_section) = split /\n\n/, $content, 2;
    my @stack_lines = split /\n/, $stack_section;
    my @move_lines = split /\n/, $move_section;

    # Find number of stacks from the last line (the numbers)
    my $num_line = pop @stack_lines;
    my @nums = split /\s+/, $num_line;
    @nums = grep { $_ ne '' } @nums;
    my $num_stacks = scalar @nums;

    # Parse stacks (top-down, then reverse)
    my @stacks;
    for my $i (0 .. $num_stacks - 1) {
        $stacks[$i] = [];
    }

    for my $line (@stack_lines) {
        for my $i (0 .. $num_stacks - 1) {
            my $pos = 1 + $i * 4;  # Position of crate letter
            if ($pos < length($line)) {
                my $char = substr($line, $pos, 1);
                if ($char ne ' ') {
                    push @{$stacks[$i]}, $char;
                }
            }
        }
    }

    # Reverse so bottom is at index 0
    for my $stack (@stacks) {
        @$stack = reverse @$stack;
    }

    # Parse moves
    my @moves;
    for my $line (@move_lines) {
        if ($line =~ /move (\d+) from (\d+) to (\d+)/) {
            my $count = $1;
            my $from_stack = $2 - 1;  # 0-indexed
            my $to_stack = $3 - 1;
            push @moves, [$count, $from_stack, $to_stack];
        }
    }

    return (\@stacks, \@moves);
}

sub deep_copy_stacks {
    my ($stacks) = @_;
    my @copy;
    for my $stack (@$stacks) {
        push @copy, [@$stack];
    }
    return \@copy;
}

sub part1 {
    my ($stacks, $moves) = @_;
    $stacks = deep_copy_stacks($stacks);

    for my $move (@$moves) {
        my ($count, $from_stack, $to_stack) = @$move;
        for (1 .. $count) {
            my $crate = pop @{$stacks->[$from_stack]};
            push @{$stacks->[$to_stack]}, $crate;
        }
    }

    my $result = '';
    for my $stack (@$stacks) {
        $result .= $stack->[-1] if @$stack;
    }
    return $result;
}

sub part2 {
    my ($stacks, $moves) = @_;
    $stacks = deep_copy_stacks($stacks);

    for my $move (@$moves) {
        my ($count, $from_stack, $to_stack) = @$move;
        # Move multiple crates at once (preserve order)
        my @crates = splice @{$stacks->[$from_stack]}, -$count;
        push @{$stacks->[$to_stack]}, @crates;
    }

    my $result = '';
    for my $stack (@$stacks) {
        $result .= $stack->[-1] if @$stack;
    }
    return $result;
}

sub main {
    my $script_dir = dirname(abs_path(__FILE__));
    my $input_file = "$script_dir/../input.txt";

    my ($stacks, $moves) = parse_input($input_file);

    print "Part 1: ", part1($stacks, $moves), "\n";
    print "Part 2: ", part2($stacks, $moves), "\n";
}

main();
