#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

sub parse_input {
    my ($filename) = @_;
    my @rounds;
    open(my $fh, '<', $filename) or die "Cannot open $filename: $!";
    while (my $line = <$fh>) {
        chomp $line;
        next unless $line =~ /\S/;
        my @parts = split(/\s+/, $line);
        push @rounds, \@parts;
    }
    close($fh);
    return \@rounds;
}

sub part1 {
    my ($rounds) = @_;
    # X=Rock, Y=Paper, Z=Scissors
    # Shape scores: Rock=1, Paper=2, Scissors=3
    my %shape_score = ('X' => 1, 'Y' => 2, 'Z' => 3);

    # Outcome: 0=loss, 3=draw, 6=win
    # A=Rock, B=Paper, C=Scissors
    my %outcomes = (
        'A,X' => 3,  # Rock vs Rock = draw
        'A,Y' => 6,  # Rock vs Paper = win
        'A,Z' => 0,  # Rock vs Scissors = loss
        'B,X' => 0,  # Paper vs Rock = loss
        'B,Y' => 3,  # Paper vs Paper = draw
        'B,Z' => 6,  # Paper vs Scissors = win
        'C,X' => 6,  # Scissors vs Rock = win
        'C,Y' => 0,  # Scissors vs Paper = loss
        'C,Z' => 3,  # Scissors vs Scissors = draw
    );

    my $total = 0;
    for my $round (@$rounds) {
        my ($opp, $me) = @$round;
        $total += $shape_score{$me} + $outcomes{"$opp,$me"};
    }
    return $total;
}

sub part2 {
    my ($rounds) = @_;
    # X=lose, Y=draw, Z=win
    # What shape to play given opponent and desired outcome
    # Returns the shape we play (1=Rock, 2=Paper, 3=Scissors)
    my %choices = (
        'A,X' => 3,  # Rock, need to lose -> Scissors
        'A,Y' => 1,  # Rock, need to draw -> Rock
        'A,Z' => 2,  # Rock, need to win -> Paper
        'B,X' => 1,  # Paper, need to lose -> Rock
        'B,Y' => 2,  # Paper, need to draw -> Paper
        'B,Z' => 3,  # Paper, need to win -> Scissors
        'C,X' => 2,  # Scissors, need to lose -> Paper
        'C,Y' => 3,  # Scissors, need to draw -> Scissors
        'C,Z' => 1,  # Scissors, need to win -> Rock
    );

    my %outcome_score = ('X' => 0, 'Y' => 3, 'Z' => 6);

    my $total = 0;
    for my $round (@$rounds) {
        my ($opp, $outcome) = @$round;
        $total += $choices{"$opp,$outcome"} + $outcome_score{$outcome};
    }
    return $total;
}

sub main {
    my $script_dir = dirname(File::Spec->rel2abs(__FILE__));
    my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');

    my $rounds = parse_input($input_file);

    print "Part 1: ", part1($rounds), "\n";
    print "Part 2: ", part2($rounds), "\n";
}

main();
