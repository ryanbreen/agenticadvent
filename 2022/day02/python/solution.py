#!/usr/bin/env python3
import os

def parse_input(filename):
    with open(filename) as f:
        return [line.strip().split() for line in f if line.strip()]

def part1(rounds):
    """X=Rock, Y=Paper, Z=Scissors"""
    # Shape scores: Rock=1, Paper=2, Scissors=3
    shape_score = {'X': 1, 'Y': 2, 'Z': 3}

    # Outcome: 0=loss, 3=draw, 6=win
    # A=Rock, B=Paper, C=Scissors
    outcomes = {
        ('A', 'X'): 3,  # Rock vs Rock = draw
        ('A', 'Y'): 6,  # Rock vs Paper = win
        ('A', 'Z'): 0,  # Rock vs Scissors = loss
        ('B', 'X'): 0,  # Paper vs Rock = loss
        ('B', 'Y'): 3,  # Paper vs Paper = draw
        ('B', 'Z'): 6,  # Paper vs Scissors = win
        ('C', 'X'): 6,  # Scissors vs Rock = win
        ('C', 'Y'): 0,  # Scissors vs Paper = loss
        ('C', 'Z'): 3,  # Scissors vs Scissors = draw
    }

    total = 0
    for opp, me in rounds:
        total += shape_score[me] + outcomes[(opp, me)]
    return total

def part2(rounds):
    """X=lose, Y=draw, Z=win"""
    # What shape to play given opponent and desired outcome
    # Returns the shape we play (1=Rock, 2=Paper, 3=Scissors)
    choices = {
        ('A', 'X'): 3,  # Rock, need to lose -> Scissors
        ('A', 'Y'): 1,  # Rock, need to draw -> Rock
        ('A', 'Z'): 2,  # Rock, need to win -> Paper
        ('B', 'X'): 1,  # Paper, need to lose -> Rock
        ('B', 'Y'): 2,  # Paper, need to draw -> Paper
        ('B', 'Z'): 3,  # Paper, need to win -> Scissors
        ('C', 'X'): 2,  # Scissors, need to lose -> Paper
        ('C', 'Y'): 3,  # Scissors, need to draw -> Scissors
        ('C', 'Z'): 1,  # Scissors, need to win -> Rock
    }

    outcome_score = {'X': 0, 'Y': 3, 'Z': 6}

    total = 0
    for opp, outcome in rounds:
        total += choices[(opp, outcome)] + outcome_score[outcome]
    return total

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    rounds = parse_input(input_file)

    print('Part 1:', part1(rounds))
    print('Part 2:', part2(rounds))

if __name__ == '__main__':
    main()
