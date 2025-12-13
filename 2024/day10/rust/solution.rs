use std::collections::{HashSet, VecDeque};
use std::fs;

type Grid = Vec<Vec<u8>>;
type Position = (usize, usize);

const DIRS: [(isize, isize); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

fn parse_input(input: &str) -> Grid {
    input
        .lines()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap() as u8).collect())
        .collect()
}

fn find_trailheads(grid: &Grid) -> Vec<Position> {
    grid.iter()
        .enumerate()
        .flat_map(|(r, row)| {
            row.iter()
                .enumerate()
                .filter_map(move |(c, &height)| {
                    if height == 0 {
                        Some((r, c))
                    } else {
                        None
                    }
                })
        })
        .collect()
}

fn count_reachable_nines(grid: &Grid, start: Position) -> usize {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();
    let mut nines = HashSet::new();

    visited.insert(start);
    queue.push_back(start);

    while let Some((r, c)) = queue.pop_front() {
        let current_height = grid[r][c];

        if current_height == 9 {
            nines.insert((r, c));
            continue;
        }

        for &(dr, dc) in &DIRS {
            if let (Some(nr), Some(nc)) = (r.checked_add_signed(dr), c.checked_add_signed(dc)) {
                if nr < rows && nc < cols && !visited.contains(&(nr, nc)) && grid[nr][nc] == current_height + 1 {
                    visited.insert((nr, nc));
                    queue.push_back((nr, nc));
                }
            }
        }
    }

    nines.len()
}

fn count_distinct_trails(grid: &Grid, start: Position) -> usize {
    let rows = grid.len();
    let cols = grid[0].len();

    fn dfs(grid: &Grid, pos: Position, rows: usize, cols: usize) -> usize {
        let (r, c) = pos;
        let current_height = grid[r][c];

        if current_height == 9 {
            return 1;
        }

        DIRS.iter()
            .filter_map(|&(dr, dc)| {
                let nr = r.checked_add_signed(dr)?;
                let nc = c.checked_add_signed(dc)?;
                if nr < rows && nc < cols && grid[nr][nc] == current_height + 1 {
                    Some(dfs(grid, (nr, nc), rows, cols))
                } else {
                    None
                }
            })
            .sum()
    }

    dfs(grid, start, rows, cols)
}

fn part1(grid: &Grid, trailheads: &[Position]) -> usize {
    trailheads
        .iter()
        .map(|&pos| count_reachable_nines(grid, pos))
        .sum()
}

fn part2(grid: &Grid, trailheads: &[Position]) -> usize {
    trailheads
        .iter()
        .map(|&pos| count_distinct_trails(grid, pos))
        .sum()
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");
    let grid = parse_input(input.trim());
    let trailheads = find_trailheads(&grid);

    println!("Part 1: {}", part1(&grid, &trailheads));
    println!("Part 2: {}", part2(&grid, &trailheads));
}
