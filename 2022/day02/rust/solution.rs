use std::fs;
use std::path::Path;

fn parse_input(filename: &Path) -> Vec<(char, char)> {
    fs::read_to_string(filename)
        .expect("Failed to read input file")
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
            let chars: Vec<char> = line.chars().collect();
            (chars[0], chars[2])
        })
        .collect()
}

fn part1(rounds: &[(char, char)]) -> i32 {
    // X=Rock, Y=Paper, Z=Scissors
    // Shape scores: Rock=1, Paper=2, Scissors=3
    // Outcome: 0=loss, 3=draw, 6=win
    // A=Rock, B=Paper, C=Scissors

    rounds.iter().map(|&(opp, me)| {
        let shape_score = match me {
            'X' => 1,
            'Y' => 2,
            'Z' => 3,
            _ => 0,
        };

        let outcome_score = match (opp, me) {
            ('A', 'X') => 3,  // Rock vs Rock = draw
            ('A', 'Y') => 6,  // Rock vs Paper = win
            ('A', 'Z') => 0,  // Rock vs Scissors = loss
            ('B', 'X') => 0,  // Paper vs Rock = loss
            ('B', 'Y') => 3,  // Paper vs Paper = draw
            ('B', 'Z') => 6,  // Paper vs Scissors = win
            ('C', 'X') => 6,  // Scissors vs Rock = win
            ('C', 'Y') => 0,  // Scissors vs Paper = loss
            ('C', 'Z') => 3,  // Scissors vs Scissors = draw
            _ => 0,
        };

        shape_score + outcome_score
    }).sum()
}

fn part2(rounds: &[(char, char)]) -> i32 {
    // X=lose, Y=draw, Z=win
    // What shape to play given opponent and desired outcome
    // Returns the shape we play (1=Rock, 2=Paper, 3=Scissors)

    rounds.iter().map(|&(opp, outcome)| {
        let shape_score = match (opp, outcome) {
            ('A', 'X') => 3,  // Rock, need to lose -> Scissors
            ('A', 'Y') => 1,  // Rock, need to draw -> Rock
            ('A', 'Z') => 2,  // Rock, need to win -> Paper
            ('B', 'X') => 1,  // Paper, need to lose -> Rock
            ('B', 'Y') => 2,  // Paper, need to draw -> Paper
            ('B', 'Z') => 3,  // Paper, need to win -> Scissors
            ('C', 'X') => 2,  // Scissors, need to lose -> Paper
            ('C', 'Y') => 3,  // Scissors, need to draw -> Scissors
            ('C', 'Z') => 1,  // Scissors, need to win -> Rock
            _ => 0,
        };

        let outcome_score = match outcome {
            'X' => 0,  // lose
            'Y' => 3,  // draw
            'Z' => 6,  // win
            _ => 0,
        };

        shape_score + outcome_score
    }).sum()
}

fn main() {
    let exe_path = std::env::current_exe().expect("Failed to get executable path");
    let exe_dir = exe_path.parent().expect("Failed to get executable directory");
    let input_path = exe_dir.join("../input.txt");

    let rounds = parse_input(&input_path);

    println!("Part 1: {}", part1(&rounds));
    println!("Part 2: {}", part2(&rounds));
}
