use std::collections::{HashSet, VecDeque};
use std::fs;

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let grid: Vec<Vec<char>> = input
        .trim()
        .lines()
        .map(|line| line.chars().collect())
        .collect();

    let regions = find_regions(&grid);

    println!("Part 1: {}", part1(&regions));
    println!("Part 2: {}", part2(&regions));
}

fn find_regions(grid: &[Vec<char>]) -> Vec<HashSet<(i32, i32)>> {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut visited = HashSet::new();
    let mut regions = Vec::new();

    for r in 0..rows {
        for c in 0..cols {
            let pos = (r as i32, c as i32);
            if visited.contains(&pos) {
                continue;
            }

            // BFS to find all cells in this region
            let plant = grid[r][c];
            let mut region = HashSet::new();
            let mut queue = VecDeque::new();
            queue.push_back(pos);

            while let Some((cr, cc)) = queue.pop_front() {
                if visited.contains(&(cr, cc)) {
                    continue;
                }
                if cr < 0 || cr >= rows as i32 || cc < 0 || cc >= cols as i32 {
                    continue;
                }
                if grid[cr as usize][cc as usize] != plant {
                    continue;
                }

                visited.insert((cr, cc));
                region.insert((cr, cc));

                for (dr, dc) in [(0, 1), (0, -1), (1, 0), (-1, 0)] {
                    let nr = cr + dr;
                    let nc = cc + dc;
                    if !visited.contains(&(nr, nc)) {
                        queue.push_back((nr, nc));
                    }
                }
            }

            regions.push(region);
        }
    }

    regions
}

fn calculate_perimeter(region: &HashSet<(i32, i32)>) -> usize {
    region.iter()
        .map(|(r, c)| {
            [(0, 1), (0, -1), (1, 0), (-1, 0)]
                .iter()
                .filter(|(dr, dc)| !region.contains(&(r + dr, c + dc)))
                .count()
        })
        .sum()
}

fn count_sides(region: &HashSet<(i32, i32)>) -> usize {
    let mut corners = 0;

    for (r, c) in region {
        // Check all 4 corners of this cell
        // Each corner is defined by checking two orthogonal neighbors and the diagonal
        // Convex: both orthogonal out
        // Concave: both orthogonal in, diagonal out

        let up = region.contains(&(r - 1, *c));
        let down = region.contains(&(r + 1, *c));
        let left = region.contains(&(*r, c - 1));
        let right = region.contains(&(*r, c + 1));
        let up_left = region.contains(&(r - 1, c - 1));
        let up_right = region.contains(&(r - 1, c + 1));
        let down_left = region.contains(&(r + 1, c - 1));
        let down_right = region.contains(&(r + 1, c + 1));

        // Top-left corner
        if !up && !left {
            corners += 1; // convex
        } else if up && left && !up_left {
            corners += 1; // concave
        }

        // Top-right corner
        if !up && !right {
            corners += 1; // convex
        } else if up && right && !up_right {
            corners += 1; // concave
        }

        // Bottom-left corner
        if !down && !left {
            corners += 1; // convex
        } else if down && left && !down_left {
            corners += 1; // concave
        }

        // Bottom-right corner
        if !down && !right {
            corners += 1; // convex
        } else if down && right && !down_right {
            corners += 1; // concave
        }
    }

    corners
}

fn part1(regions: &[HashSet<(i32, i32)>]) -> usize {
    regions.iter()
        .map(|region| region.len() * calculate_perimeter(region))
        .sum()
}

fn part2(regions: &[HashSet<(i32, i32)>]) -> usize {
    regions.iter()
        .map(|region| region.len() * count_sides(region))
        .sum()
}
