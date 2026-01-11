#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

sub parse_input {
    my $dir = dirname(__FILE__);
    my $input_path = "$dir/../input.txt";

    open(my $fh, '<', $input_path) or die "Cannot open $input_path: $!";
    my $content = do { local $/; <$fh> };
    close($fh);

    $content =~ s/\s+$//;  # trim trailing whitespace

    my @sections = split(/\n\n/, $content);
    my @numbers = split(/,/, shift @sections);

    my @boards;
    for my $section (@sections) {
        my @board;
        for my $line (split(/\n/, $section)) {
            $line =~ s/^\s+//;  # trim leading whitespace
            my @row = split(/\s+/, $line);
            push @board, \@row;
        }
        push @boards, \@board;
    }

    return (\@numbers, \@boards);
}

sub check_winner {
    my ($board, $marked) = @_;

    # Check rows
    for my $row (0..4) {
        my $all_marked = 1;
        for my $col (0..4) {
            if (!$marked->[$row][$col]) {
                $all_marked = 0;
                last;
            }
        }
        return 1 if $all_marked;
    }

    # Check columns
    for my $col (0..4) {
        my $all_marked = 1;
        for my $row (0..4) {
            if (!$marked->[$row][$col]) {
                $all_marked = 0;
                last;
            }
        }
        return 1 if $all_marked;
    }

    return 0;
}

sub calculate_score {
    my ($board, $marked, $last_number) = @_;

    my $unmarked_sum = 0;
    for my $row (0..4) {
        for my $col (0..4) {
            if (!$marked->[$row][$col]) {
                $unmarked_sum += $board->[$row][$col];
            }
        }
    }

    return $unmarked_sum * $last_number;
}

sub mark_number {
    my ($board, $marked, $number) = @_;

    for my $row (0..4) {
        for my $col (0..4) {
            if ($board->[$row][$col] == $number) {
                $marked->[$row][$col] = 1;
            }
        }
    }
}

sub part1 {
    my ($numbers, $boards) = @_;

    # Initialize marked arrays for each board
    my @marked;
    for my $i (0..$#$boards) {
        my @board_marked;
        for my $row (0..4) {
            push @board_marked, [(0) x 5];
        }
        push @marked, \@board_marked;
    }

    for my $number (@$numbers) {
        for my $i (0..$#$boards) {
            mark_number($boards->[$i], $marked[$i], $number);
            if (check_winner($boards->[$i], $marked[$i])) {
                return calculate_score($boards->[$i], $marked[$i], $number);
            }
        }
    }

    return undef;
}

sub part2 {
    my ($numbers, $boards) = @_;

    # Initialize marked arrays for each board
    my @marked;
    for my $i (0..$#$boards) {
        my @board_marked;
        for my $row (0..4) {
            push @board_marked, [(0) x 5];
        }
        push @marked, \@board_marked;
    }

    my @won = (0) x scalar(@$boards);
    my $last_score = undef;

    for my $number (@$numbers) {
        for my $i (0..$#$boards) {
            next if $won[$i];
            mark_number($boards->[$i], $marked[$i], $number);
            if (check_winner($boards->[$i], $marked[$i])) {
                $won[$i] = 1;
                $last_score = calculate_score($boards->[$i], $marked[$i], $number);
            }
        }
    }

    return $last_score;
}

my ($numbers, $boards) = parse_input();
print "Part 1: " . part1($numbers, $boards) . "\n";
print "Part 2: " . part2($numbers, $boards) . "\n";
