use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::fs;

fn parse_grid(input: &str) -> (Vec<Vec<char>>, (usize, usize), (usize, usize)) {
    let mut start = (0, 0);
    let mut end = (0, 0);

    let grid: Vec<Vec<char>> = input
        .lines()
        .map(|line| line.chars().collect())
        .collect();

    for (r, row) in grid.iter().enumerate() {
        for (c, &ch) in row.iter().enumerate() {
            match ch {
                'S' => start = (r, c),
                'E' => end = (r, c),
                _ => {}
            }
        }
    }

    (grid, start, end)
}

fn trace_path(
    grid: &[Vec<char>],
    start: (usize, usize),
    end: (usize, usize),
) -> HashMap<(usize, usize), i32> {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut dist: HashMap<(usize, usize), i32> = HashMap::new();
    let mut queue: VecDeque<(usize, usize)> = VecDeque::new();

    dist.insert(start, 0);
    queue.push_back(start);

    const DIRECTIONS: [(i32, i32); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

    while let Some((r, c)) = queue.pop_front() {
        if (r, c) == end {
            break;
        }
        let current_dist = dist[&(r, c)];

        for &(dr, dc) in &DIRECTIONS {
            let nr = r as i32 + dr;
            let nc = c as i32 + dc;

            if nr >= 0 && nr < rows as i32 && nc >= 0 && nc < cols as i32 {
                let nr = nr as usize;
                let nc = nc as usize;
                if grid[nr][nc] != '#' {
                    if let Entry::Vacant(e) = dist.entry((nr, nc)) {
                        e.insert(current_dist + 1);
                        queue.push_back((nr, nc));
                    }
                }
            }
        }
    }

    dist
}

fn count_cheats(dist: &HashMap<(usize, usize), i32>, max_cheat_time: i32, min_savings: i32) -> i32 {
    let track_positions: Vec<((usize, usize), i32)> = dist.iter().map(|(&k, &v)| (k, v)).collect();
    let mut count = 0;

    for &((r1, c1), d1) in &track_positions {
        for &((r2, c2), d2) in &track_positions {
            let cheat_cost = (r2 as i32 - r1 as i32).abs() + (c2 as i32 - c1 as i32).abs();
            if cheat_cost <= max_cheat_time {
                let savings = d2 - d1 - cheat_cost;
                if savings >= min_savings {
                    count += 1;
                }
            }
        }
    }

    count
}

fn part1(dist: &HashMap<(usize, usize), i32>) -> i32 {
    count_cheats(dist, 2, 100)
}

fn part2(dist: &HashMap<(usize, usize), i32>) -> i32 {
    count_cheats(dist, 20, 100)
}

fn main() {
    let input = fs::read_to_string("../input.txt").expect("Failed to read input file");
    let (grid, start, end) = parse_grid(&input);
    let dist = trace_path(&grid, start, end);

    println!("Part 1: {}", part1(&dist));
    println!("Part 2: {}", part2(&dist));
}
