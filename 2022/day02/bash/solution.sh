#!/bin/bash

# Day 2: Rock Paper Scissors
# Run from bash directory with input at ../input.txt

INPUT_FILE="../input.txt"

# Part 1: X=Rock, Y=Paper, Z=Scissors
# Shape scores: Rock=1, Paper=2, Scissors=3
# Outcome: 0=loss, 3=draw, 6=win
part1() {
    awk '
    BEGIN {
        # Shape scores for our move
        shape["X"] = 1  # Rock
        shape["Y"] = 2  # Paper
        shape["Z"] = 3  # Scissors

        # Outcome scores: opponent, me -> outcome
        # A=Rock, B=Paper, C=Scissors
        outcome["A X"] = 3  # Rock vs Rock = draw
        outcome["A Y"] = 6  # Rock vs Paper = win
        outcome["A Z"] = 0  # Rock vs Scissors = loss
        outcome["B X"] = 0  # Paper vs Rock = loss
        outcome["B Y"] = 3  # Paper vs Paper = draw
        outcome["B Z"] = 6  # Paper vs Scissors = win
        outcome["C X"] = 6  # Scissors vs Rock = win
        outcome["C Y"] = 0  # Scissors vs Paper = loss
        outcome["C Z"] = 3  # Scissors vs Scissors = draw
    }
    {
        total += shape[$2] + outcome[$1 " " $2]
    }
    END {
        print total
    }
    ' "$INPUT_FILE"
}

# Part 2: X=lose, Y=draw, Z=win
# Figure out what shape to play
part2() {
    awk '
    BEGIN {
        # Outcome scores
        outcome_score["X"] = 0  # lose
        outcome_score["Y"] = 3  # draw
        outcome_score["Z"] = 6  # win

        # What shape to play (returns 1=Rock, 2=Paper, 3=Scissors)
        # Given opponent move and desired outcome
        choice["A X"] = 3  # Rock, lose -> Scissors
        choice["A Y"] = 1  # Rock, draw -> Rock
        choice["A Z"] = 2  # Rock, win -> Paper
        choice["B X"] = 1  # Paper, lose -> Rock
        choice["B Y"] = 2  # Paper, draw -> Paper
        choice["B Z"] = 3  # Paper, win -> Scissors
        choice["C X"] = 2  # Scissors, lose -> Paper
        choice["C Y"] = 3  # Scissors, draw -> Scissors
        choice["C Z"] = 1  # Scissors, win -> Rock
    }
    {
        total += choice[$1 " " $2] + outcome_score[$2]
    }
    END {
        print total
    }
    ' "$INPUT_FILE"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
