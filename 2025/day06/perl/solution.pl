#!/usr/bin/env perl
use strict;
use warnings;
use Math::BigInt;

# Read input file
my $input_file = "../input.txt";
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close($fh);

# Remove trailing newlines but keep the content
chomp(@lines);

sub parse_problems {
    my ($lines_ref) = @_;
    my @lines = @$lines_ref;

    return () if @lines == 0;

    # Find the operator row (last non-empty row with only +, *, and spaces)
    my $op_row_idx = $#lines;
    while ($op_row_idx >= 0) {
        my $line = $lines[$op_row_idx];
        last if $line =~ /\S/ && $line =~ /^[+*\s]+$/;
        $op_row_idx--;
    }

    return () if $op_row_idx < 0;

    my $op_row = $lines[$op_row_idx];
    my @number_rows = @lines[0..$op_row_idx-1];

    # Find max width
    my $max_width = 0;
    for my $line (@lines) {
        my $len = length($line);
        $max_width = $len if $len > $max_width;
    }

    # Pad all rows to the same width
    my @padded_number_rows = map { sprintf("%-${max_width}s", $_) } @number_rows;
    my $padded_op_row = sprintf("%-${max_width}s", $op_row);

    # Find problem boundaries by looking for columns that are all spaces
    my @problems;
    my $col = 0;

    while ($col < $max_width) {
        # Skip separator columns (all spaces)
        while ($col < $max_width) {
            my $is_all_space = 1;
            for my $row (@padded_number_rows) {
                if (substr($row, $col, 1) ne ' ') {
                    $is_all_space = 0;
                    last;
                }
            }
            last unless $is_all_space && substr($padded_op_row, $col, 1) eq ' ';
            $col++;
        }

        last if $col >= $max_width;

        # Find the end of this problem
        my $start_col = $col;
        while ($col < $max_width) {
            # Check if this is a separator column
            my $is_separator = 1;
            for my $row (@padded_number_rows) {
                if (substr($row, $col, 1) ne ' ') {
                    $is_separator = 0;
                    last;
                }
            }
            $is_separator = 0 if substr($padded_op_row, $col, 1) ne ' ';
            last if $is_separator;
            $col++;
        }

        my $end_col = $col;

        # Extract numbers and operator for this problem
        my @numbers;
        for my $row (@padded_number_rows) {
            my $num_str = substr($row, $start_col, $end_col - $start_col);
            $num_str =~ s/^\s+|\s+$//g;  # trim
            if ($num_str ne '') {
                push @numbers, Math::BigInt->new($num_str);
            }
        }

        my $op_str = substr($padded_op_row, $start_col, $end_col - $start_col);
        $op_str =~ s/^\s+|\s+$//g;  # trim
        if ($op_str ne '' && @numbers) {
            push @problems, {
                numbers => \@numbers,
                op => $op_str
            };
        }
    }

    return @problems;
}

sub solve_problem {
    my ($numbers_ref, $op) = @_;
    my @numbers = @$numbers_ref;

    if ($op eq '+') {
        my $result = Math::BigInt->new(0);
        for my $n (@numbers) {
            $result->badd($n);
        }
        return $result;
    } elsif ($op eq '*') {
        my $result = Math::BigInt->new(1);
        for my $n (@numbers) {
            $result->bmul($n);
        }
        return $result;
    }
    return Math::BigInt->new(0);
}

sub part1 {
    my @problems = parse_problems(\@lines);
    my $total = Math::BigInt->new(0);

    for my $problem (@problems) {
        my $result = solve_problem($problem->{numbers}, $problem->{op});
        $total->badd($result);
    }

    return $total;
}

sub parse_problems_part2 {
    my ($lines_ref) = @_;
    my @lines = @$lines_ref;

    return () if @lines == 0;

    # Find the operator row (last non-empty row with only +, *, and spaces)
    my $op_row_idx = $#lines;
    while ($op_row_idx >= 0) {
        my $line = $lines[$op_row_idx];
        last if $line =~ /\S/ && $line =~ /^[+*\s]+$/;
        $op_row_idx--;
    }

    return () if $op_row_idx < 0;

    my $op_row = $lines[$op_row_idx];
    my @number_rows = @lines[0..$op_row_idx-1];

    # Find max width
    my $max_width = 0;
    for my $line (@lines) {
        my $len = length($line);
        $max_width = $len if $len > $max_width;
    }

    # Pad all rows to the same width
    my @padded_number_rows = map { sprintf("%-${max_width}s", $_) } @number_rows;
    my $padded_op_row = sprintf("%-${max_width}s", $op_row);

    # Find problem boundaries by looking for columns that are all spaces
    my @problems;
    my $col = 0;

    while ($col < $max_width) {
        # Skip separator columns (all spaces)
        while ($col < $max_width) {
            my $is_all_space = 1;
            for my $row (@padded_number_rows) {
                if (substr($row, $col, 1) ne ' ') {
                    $is_all_space = 0;
                    last;
                }
            }
            last unless $is_all_space && substr($padded_op_row, $col, 1) eq ' ';
            $col++;
        }

        last if $col >= $max_width;

        # Find the end of this problem
        my $start_col = $col;
        while ($col < $max_width) {
            # Check if this is a separator column
            my $is_separator = 1;
            for my $row (@padded_number_rows) {
                if (substr($row, $col, 1) ne ' ') {
                    $is_separator = 0;
                    last;
                }
            }
            $is_separator = 0 if substr($padded_op_row, $col, 1) ne ' ';
            last if $is_separator;
            $col++;
        }

        my $end_col = $col;

        # For Part 2: Read columns right-to-left, each column forms a number
        # reading top-to-bottom as most-to-least significant digit
        my @numbers;
        for (my $c = $end_col - 1; $c >= $start_col; $c--) {  # Right to left
            my @digits;
            for my $row (@padded_number_rows) {
                my $ch = substr($row, $c, 1);
                if ($ch =~ /\d/) {
                    push @digits, $ch;
                }
            }
            if (@digits) {
                # Join digits to form number (top=most significant, bottom=least)
                my $num_str = join('', @digits);
                push @numbers, Math::BigInt->new($num_str);
            }
        }

        my $op_str = substr($padded_op_row, $start_col, $end_col - $start_col);
        $op_str =~ s/^\s+|\s+$//g;  # trim
        if ($op_str ne '' && @numbers) {
            push @problems, {
                numbers => \@numbers,
                op => $op_str
            };
        }
    }

    return @problems;
}

sub part2 {
    my @problems = parse_problems_part2(\@lines);
    my $total = Math::BigInt->new(0);

    for my $problem (@problems) {
        my $result = solve_problem($problem->{numbers}, $problem->{op});
        $total->badd($result);
    }

    return $total;
}

# Main execution
print "Part 1: ", part1(), "\n";
print "Part 2: ", part2(), "\n";
