// Day 22: Sand Slabs - 3D falling bricks simulation

use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

#[derive(Debug, Clone, Copy)]
struct Brick {
    x1: i32,
    y1: i32,
    z1: i32,
    x2: i32,
    y2: i32,
    z2: i32,
}

impl Brick {
    fn new(x1: i32, y1: i32, z1: i32, x2: i32, y2: i32, z2: i32) -> Self {
        // Ensure z1 <= z2 for consistent processing
        if z1 > z2 {
            Brick { x1: x2, y1: y2, z1: z2, x2: x1, y2: y1, z2: z1 }
        } else {
            Brick { x1, y1, z1, x2, y2, z2 }
        }
    }

    fn min_x(&self) -> i32 { self.x1.min(self.x2) }
    fn max_x(&self) -> i32 { self.x1.max(self.x2) }
    fn min_y(&self) -> i32 { self.y1.min(self.y2) }
    fn max_y(&self) -> i32 { self.y1.max(self.y2) }
}

fn parse_input(content: &str) -> Vec<Brick> {
    content
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let parts: Vec<&str> = line.split('~').collect();
            let left: Vec<i32> = parts[0].split(',').map(|s| s.parse().unwrap()).collect();
            let right: Vec<i32> = parts[1].split(',').map(|s| s.parse().unwrap()).collect();
            Brick::new(left[0], left[1], left[2], right[0], right[1], right[2])
        })
        .collect()
}

fn settle_bricks(bricks: &[Brick]) -> (Vec<Brick>, Vec<HashSet<usize>>, Vec<HashSet<usize>>) {
    // Create indexed bricks sorted by minimum z
    let mut sorted_indices: Vec<usize> = (0..bricks.len()).collect();
    sorted_indices.sort_by_key(|&i| bricks[i].z1);

    // Track occupied cells: (x, y, z) -> brick index
    let mut occupied: HashMap<(i32, i32, i32), usize> = HashMap::new();
    let mut settled: Vec<Brick> = vec![Brick::new(0, 0, 0, 0, 0, 0); bricks.len()];

    // supports[i] = set of brick indices that brick i supports (bricks above)
    // supporters[i] = set of brick indices that support brick i (bricks below)
    let mut supports: Vec<HashSet<usize>> = vec![HashSet::new(); bricks.len()];
    let mut supporters: Vec<HashSet<usize>> = vec![HashSet::new(); bricks.len()];

    for &orig_idx in &sorted_indices {
        let brick = bricks[orig_idx];

        // Find the maximum drop (to z=1)
        let mut drop = brick.z1 - 1;

        // Get xy footprint of this brick
        for x in brick.min_x()..=brick.max_x() {
            for y in brick.min_y()..=brick.max_y() {
                // Check each z level below the brick
                for z in (1..brick.z1).rev() {
                    if occupied.contains_key(&(x, y, z)) {
                        drop = drop.min(brick.z1 - z - 1);
                        break;
                    }
                }
            }
        }

        // Drop the brick
        let new_z1 = brick.z1 - drop;
        let new_z2 = brick.z2 - drop;
        let new_brick = Brick::new(brick.x1, brick.y1, new_z1, brick.x2, brick.y2, new_z2);
        settled[orig_idx] = new_brick;

        // Mark cells as occupied and find supporters
        for x in brick.min_x()..=brick.max_x() {
            for y in brick.min_y()..=brick.max_y() {
                // Check if there's a brick directly below
                if let Some(&supporter_idx) = occupied.get(&(x, y, new_z1 - 1)) {
                    supporters[orig_idx].insert(supporter_idx);
                    supports[supporter_idx].insert(orig_idx);
                }

                // Mark all cells of this brick as occupied
                for z in new_z1..=new_z2 {
                    occupied.insert((x, y, z), orig_idx);
                }
            }
        }
    }

    (settled, supports, supporters)
}

fn part1(bricks: &[Brick]) -> usize {
    let (_settled, supports, supporters) = settle_bricks(bricks);

    let mut safe_count = 0;
    for i in 0..bricks.len() {
        // Brick i can be safely removed if every brick it supports
        // has at least one other supporter
        let can_remove = supports[i].iter().all(|&supported| supporters[supported].len() > 1);
        if can_remove {
            safe_count += 1;
        }
    }

    safe_count
}

fn part2(bricks: &[Brick]) -> usize {
    let (_settled, supports, supporters) = settle_bricks(bricks);

    let mut total_falls = 0;

    for i in 0..bricks.len() {
        // Simulate removing brick i and count chain reaction
        // BFS to find all bricks that would fall
        let mut falling: HashSet<usize> = HashSet::new();
        falling.insert(i);
        let mut queue: VecDeque<usize> = VecDeque::new();
        queue.push_back(i);

        while let Some(brick) = queue.pop_front() {
            // Check all bricks that this brick supports
            for &supported in &supports[brick] {
                if falling.contains(&supported) {
                    continue;
                }

                // This brick falls if all its supporters have fallen
                if supporters[supported].iter().all(|s| falling.contains(s)) {
                    falling.insert(supported);
                    queue.push_back(supported);
                }
            }
        }

        // Don't count the initial brick we removed
        total_falls += falling.len() - 1;
    }

    total_falls
}

fn main() {
    let content = fs::read_to_string("../input.txt").expect("Failed to read input file");
    let bricks = parse_input(&content);

    println!("Part 1: {}", part1(&bricks));
    println!("Part 2: {}", part2(&bricks));
}
