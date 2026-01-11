use std::fs;
use std::path::Path;

const SIZE: usize = 5;

fn parse_input() -> (Vec<i32>, Vec<[[i32; SIZE]; SIZE]>) {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let content = fs::read_to_string(&input_path).expect("Failed to read input file");

    let sections: Vec<&str> = content.trim().split("\n\n").collect();

    let numbers: Vec<i32> = sections[0]
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect();

    let mut boards = Vec::new();
    for section in &sections[1..] {
        let mut board = [[0i32; SIZE]; SIZE];
        for (row_idx, line) in section.trim().lines().enumerate() {
            for (col_idx, num) in line.split_whitespace().enumerate() {
                board[row_idx][col_idx] = num.parse().unwrap();
            }
        }
        boards.push(board);
    }

    (numbers, boards)
}

fn check_winner(marked: &[[bool; SIZE]; SIZE]) -> bool {
    // Check rows
    for row in 0..SIZE {
        if (0..SIZE).all(|col| marked[row][col]) {
            return true;
        }
    }
    // Check columns
    for col in 0..SIZE {
        if (0..SIZE).all(|row| marked[row][col]) {
            return true;
        }
    }
    false
}

fn calculate_score(board: &[[i32; SIZE]; SIZE], marked: &[[bool; SIZE]; SIZE], last_number: i32) -> i32 {
    let mut unmarked_sum = 0;
    for row in 0..SIZE {
        for col in 0..SIZE {
            if !marked[row][col] {
                unmarked_sum += board[row][col];
            }
        }
    }
    unmarked_sum * last_number
}

fn mark_number(board: &[[i32; SIZE]; SIZE], marked: &mut [[bool; SIZE]; SIZE], number: i32) {
    for row in 0..SIZE {
        for col in 0..SIZE {
            if board[row][col] == number {
                marked[row][col] = true;
            }
        }
    }
}

fn part1(numbers: &[i32], boards: &[[[i32; SIZE]; SIZE]]) -> i32 {
    let mut marked: Vec<[[bool; SIZE]; SIZE]> = vec![[[false; SIZE]; SIZE]; boards.len()];

    for &number in numbers {
        for (i, board) in boards.iter().enumerate() {
            mark_number(board, &mut marked[i], number);
            if check_winner(&marked[i]) {
                return calculate_score(board, &marked[i], number);
            }
        }
    }

    0
}

fn part2(numbers: &[i32], boards: &[[[i32; SIZE]; SIZE]]) -> i32 {
    let mut marked: Vec<[[bool; SIZE]; SIZE]> = vec![[[false; SIZE]; SIZE]; boards.len()];
    let mut won: Vec<bool> = vec![false; boards.len()];
    let mut last_score = 0;

    for &number in numbers {
        for (i, board) in boards.iter().enumerate() {
            if won[i] {
                continue;
            }
            mark_number(board, &mut marked[i], number);
            if check_winner(&marked[i]) {
                won[i] = true;
                last_score = calculate_score(board, &marked[i], number);
            }
        }
    }

    last_score
}

fn main() {
    let (numbers, boards) = parse_input();
    println!("Part 1: {}", part1(&numbers, &boards));
    println!("Part 2: {}", part2(&numbers, &boards));
}
