use std::collections::VecDeque;
use std::fs;

const MAX_SIZE: usize = 256;

// Directions: 0=right, 1=down, 2=left, 3=up
const DR: [i32; 4] = [0, 1, 0, -1];
const DC: [i32; 4] = [1, 0, -1, 0];

fn count_energized(grid: &[Vec<u8>], start_row: i32, start_col: i32, start_dir: usize) -> usize {
    let rows = grid.len() as i32;
    let cols = grid[0].len() as i32;

    let mut visited = [[[false; 4]; MAX_SIZE]; MAX_SIZE];
    let mut queue: VecDeque<(i32, i32, usize)> = VecDeque::new();
    queue.push_back((start_row, start_col, start_dir));

    while let Some((r, c, d)) = queue.pop_front() {
        if r < 0 || r >= rows || c < 0 || c >= cols {
            continue;
        }

        let ru = r as usize;
        let cu = c as usize;
        if visited[ru][cu][d] {
            continue;
        }
        visited[ru][cu][d] = true;

        let cell = grid[ru][cu];
        match cell {
            b'.' => {
                queue.push_back((r + DR[d], c + DC[d], d));
            }
            b'/' => {
                let nd = [3, 2, 1, 0][d];
                queue.push_back((r + DR[nd], c + DC[nd], nd));
            }
            b'\\' => {
                let nd = [1, 0, 3, 2][d];
                queue.push_back((r + DR[nd], c + DC[nd], nd));
            }
            b'|' => {
                if d == 0 || d == 2 {
                    queue.push_back((r + DR[1], c + DC[1], 1));
                    queue.push_back((r + DR[3], c + DC[3], 3));
                } else {
                    queue.push_back((r + DR[d], c + DC[d], d));
                }
            }
            b'-' => {
                if d == 1 || d == 3 {
                    queue.push_back((r + DR[0], c + DC[0], 0));
                    queue.push_back((r + DR[2], c + DC[2], 2));
                } else {
                    queue.push_back((r + DR[d], c + DC[d], d));
                }
            }
            _ => {
                queue.push_back((r + DR[d], c + DC[d], d));
            }
        }
    }

    let mut count = 0;
    for r in 0..rows as usize {
        for c in 0..cols as usize {
            if visited[r][c][0] || visited[r][c][1] || visited[r][c][2] || visited[r][c][3] {
                count += 1;
            }
        }
    }
    count
}

fn part1(grid: &[Vec<u8>]) -> usize {
    count_energized(grid, 0, 0, 0)
}

fn part2(grid: &[Vec<u8>]) -> usize {
    let rows = grid.len() as i32;
    let cols = grid[0].len() as i32;
    let mut max_energized = 0;

    // Top row, heading down
    for c in 0..cols {
        max_energized = max_energized.max(count_energized(grid, 0, c, 1));
    }

    // Bottom row, heading up
    for c in 0..cols {
        max_energized = max_energized.max(count_energized(grid, rows - 1, c, 3));
    }

    // Left column, heading right
    for r in 0..rows {
        max_energized = max_energized.max(count_energized(grid, r, 0, 0));
    }

    // Right column, heading left
    for r in 0..rows {
        max_energized = max_energized.max(count_energized(grid, r, cols - 1, 2));
    }

    max_energized
}

fn main() {
    let input = fs::read_to_string("../input.txt").expect("Failed to read input file");
    let grid: Vec<Vec<u8>> = input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| line.as_bytes().to_vec())
        .collect();

    println!("Part 1: {}", part1(&grid));
    println!("Part 2: {}", part2(&grid));
}
