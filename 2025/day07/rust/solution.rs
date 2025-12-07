use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

fn read_input() -> Vec<String> {
    let path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let content = fs::read_to_string(&path).expect("Failed to read input file");
    content.trim().lines().map(|s| s.to_string()).collect()
}

fn part1(lines: &[String]) -> usize {
    let rows = lines.len();
    let cols = if rows > 0 { lines[0].len() } else { 0 };

    // Find starting position S
    let start_col = lines[0]
        .chars()
        .position(|c| c == 'S');

    if start_col.is_none() {
        return 0;
    }

    let start_col = start_col.unwrap();

    // Track active beam columns at each row
    // Use a set to handle beam merging
    let mut active_beams: HashSet<usize> = HashSet::new();
    active_beams.insert(start_col);
    let mut split_count = 0;

    // Process row by row starting from row 1 (below S)
    for row in 1..rows {
        let mut new_beams: HashSet<usize> = HashSet::new();

        for &col in &active_beams {
            if col < cols {
                let cell = lines[row].chars().nth(col).unwrap();
                if cell == '^' {
                    // Beam hits splitter - count it and emit left/right
                    split_count += 1;
                    // Left beam goes to col-1, right beam goes to col+1
                    if col > 0 {
                        new_beams.insert(col - 1);
                    }
                    if col + 1 < cols {
                        new_beams.insert(col + 1);
                    }
                } else if cell == '.' {
                    // Beam continues straight down
                    new_beams.insert(col);
                } else {
                    // If cell is something else (like S), beam continues
                    new_beams.insert(col);
                }
            }
        }

        active_beams = new_beams;

        // If no more beams, stop
        if active_beams.is_empty() {
            break;
        }
    }

    split_count
}

fn part2(lines: &[String]) -> u128 {
    let rows = lines.len();
    let cols = if rows > 0 { lines[0].len() } else { 0 };

    // Find starting position S
    let start_col = lines[0]
        .chars()
        .position(|c| c == 'S');

    if start_col.is_none() {
        return 0;
    }

    let start_col = start_col.unwrap();

    // Track number of timelines at each column position
    // Use a HashMap: col -> count of timelines at that position
    let mut timelines: HashMap<usize, u128> = HashMap::new();
    timelines.insert(start_col, 1);

    // Process row by row starting from row 1 (below S)
    for row in 1..rows {
        let mut new_timelines: HashMap<usize, u128> = HashMap::new();

        for (&col, &count) in &timelines {
            if col < cols {
                let cell = lines[row].chars().nth(col).unwrap();
                if cell == '^' {
                    // Each timeline splits into 2 (left and right)
                    if col > 0 {
                        *new_timelines.entry(col - 1).or_insert(0) += count;
                    }
                    if col + 1 < cols {
                        *new_timelines.entry(col + 1).or_insert(0) += count;
                    }
                } else if cell == '.' {
                    // Timelines continue straight down
                    *new_timelines.entry(col).or_insert(0) += count;
                } else {
                    // Other characters - timelines continue
                    *new_timelines.entry(col).or_insert(0) += count;
                }
            }
        }

        timelines = new_timelines;

        // If no more timelines, stop
        if timelines.is_empty() {
            break;
        }
    }

    // Total number of timelines
    timelines.values().sum()
}

fn main() {
    let lines = read_input();
    println!("Part 1: {}", part1(&lines));
    println!("Part 2: {}", part2(&lines));
}
