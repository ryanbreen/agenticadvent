use std::collections::HashMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let grid: Vec<Vec<char>> = input
        .lines()
        .map(|line| line.chars().collect())
        .collect();

    println!("Part 1: {}", part1(&grid));
    println!("Part 2: {}", part2(&grid));
}

fn part1(grid: &[Vec<char>]) -> u32 {
    let rows = grid.len();
    let cols = if rows > 0 { grid[0].len() } else { 0 };

    let mut sum = 0;

    // Find all numbers in the grid
    for row in 0..rows {
        let mut col = 0;
        while col < cols {
            if grid[row][col].is_ascii_digit() {
                // Found start of a number
                let mut num_str = String::new();
                let start_col = col;

                // Collect all digits
                while col < cols && grid[row][col].is_ascii_digit() {
                    num_str.push(grid[row][col]);
                    col += 1;
                }

                let number: u32 = num_str.parse().unwrap();

                // Check if any adjacent cell contains a symbol
                if is_adjacent_to_symbol(grid, row, start_col, col - 1) {
                    sum += number;
                }
            } else {
                col += 1;
            }
        }
    }

    sum
}

fn is_adjacent_to_symbol(grid: &[Vec<char>], row: usize, start_col: usize, end_col: usize) -> bool {
    let rows = grid.len();
    let cols = grid[0].len();

    // Check all cells adjacent to the number (including diagonals)
    for r in row.saturating_sub(1)..=(row + 1).min(rows - 1) {
        for c in start_col.saturating_sub(1)..=(end_col + 1).min(cols - 1) {
            let ch = grid[r][c];
            // A symbol is anything that's not a digit and not a period
            if !ch.is_ascii_digit() && ch != '.' {
                return true;
            }
        }
    }

    false
}

fn part2(grid: &[Vec<char>]) -> u32 {
    let rows = grid.len();
    let cols = if rows > 0 { grid[0].len() } else { 0 };

    // Map from gear position (row, col) to list of adjacent numbers
    let mut gear_numbers: HashMap<(usize, usize), Vec<u32>> = HashMap::new();

    // Find all numbers in the grid
    for row in 0..rows {
        let mut col = 0;
        while col < cols {
            if grid[row][col].is_ascii_digit() {
                // Found start of a number
                let mut num_str = String::new();
                let start_col = col;

                // Collect all digits
                while col < cols && grid[row][col].is_ascii_digit() {
                    num_str.push(grid[row][col]);
                    col += 1;
                }

                let number: u32 = num_str.parse().unwrap();

                // Find all adjacent gears (*) and add this number to their lists
                for gear_pos in find_adjacent_gears(grid, row, start_col, col - 1) {
                    gear_numbers.entry(gear_pos).or_insert_with(Vec::new).push(number);
                }
            } else {
                col += 1;
            }
        }
    }

    // Sum gear ratios for gears with exactly 2 adjacent numbers
    gear_numbers
        .values()
        .filter(|numbers| numbers.len() == 2)
        .map(|numbers| numbers[0] * numbers[1])
        .sum()
}

fn find_adjacent_gears(grid: &[Vec<char>], row: usize, start_col: usize, end_col: usize) -> Vec<(usize, usize)> {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut gears = Vec::new();

    // Check all cells adjacent to the number (including diagonals)
    for r in row.saturating_sub(1)..=(row + 1).min(rows - 1) {
        for c in start_col.saturating_sub(1)..=(end_col + 1).min(cols - 1) {
            if grid[r][c] == '*' {
                gears.push((r, c));
            }
        }
    }

    gears
}
