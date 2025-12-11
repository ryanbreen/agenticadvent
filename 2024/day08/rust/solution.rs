use std::collections::{HashMap, HashSet};
use std::fs;

const INPUT_FILE: &str = "../input.txt";

fn in_bounds(row: i32, col: i32, rows: usize, cols: usize) -> bool {
    row >= 0 && row < rows as i32 && col >= 0 && col < cols as i32
}

fn parse_input(filename: &str) -> (usize, usize, HashMap<char, Vec<(i32, i32)>>) {
    let content = fs::read_to_string(filename).expect("Failed to read input file");

    let grid: Vec<&str> = content.lines().collect();
    let rows = grid.len();
    let cols = grid.first().map_or(0, |line| line.len());

    let mut antennas: HashMap<char, Vec<(i32, i32)>> = HashMap::new();

    for (r, row) in grid.iter().enumerate() {
        for (c, ch) in row.chars().enumerate() {
            if ch != '.' {
                antennas
                    .entry(ch)
                    .or_insert_with(Vec::new)
                    .push((r as i32, c as i32));
            }
        }
    }

    (rows, cols, antennas)
}

fn part1() -> usize {
    let (rows, cols, antennas) = parse_input(INPUT_FILE);
    let mut antinodes: HashSet<(i32, i32)> = HashSet::new();

    for positions in antennas.values() {
        // For each pair of antennas with same frequency
        for i in 0..positions.len() {
            for j in (i + 1)..positions.len() {
                let (r1, c1) = positions[i];
                let (r2, c2) = positions[j];

                // Calculate the two antinodes
                // Antinode beyond antenna 1 (away from antenna 2)
                let ar1 = 2 * r1 - r2;
                let ac1 = 2 * c1 - c2;

                // Antinode beyond antenna 2 (away from antenna 1)
                let ar2 = 2 * r2 - r1;
                let ac2 = 2 * c2 - c1;

                // Add if within bounds
                if in_bounds(ar1, ac1, rows, cols) {
                    antinodes.insert((ar1, ac1));
                }
                if in_bounds(ar2, ac2, rows, cols) {
                    antinodes.insert((ar2, ac2));
                }
            }
        }
    }

    antinodes.len()
}

fn part2() -> usize {
    let (rows, cols, antennas) = parse_input(INPUT_FILE);
    let mut antinodes: HashSet<(i32, i32)> = HashSet::new();

    for positions in antennas.values() {
        // For each pair of antennas with same frequency
        for i in 0..positions.len() {
            for j in (i + 1)..positions.len() {
                let (r1, c1) = positions[i];
                let (r2, c2) = positions[j];

                let dr = r2 - r1;
                let dc = c2 - c1;

                // Extend in both directions along the line
                // Direction 1: from antenna 1 towards and beyond antenna 2
                let mut r = r1;
                let mut c = c1;
                while in_bounds(r, c, rows, cols) {
                    antinodes.insert((r, c));
                    r += dr;
                    c += dc;
                }

                // Direction 2: from antenna 1 away from antenna 2
                r = r1 - dr;
                c = c1 - dc;
                while in_bounds(r, c, rows, cols) {
                    antinodes.insert((r, c));
                    r -= dr;
                    c -= dc;
                }
            }
        }
    }

    antinodes.len()
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
