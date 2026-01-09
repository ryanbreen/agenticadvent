use std::collections::HashSet;
use std::fs;
use std::path::Path;

fn sign(x: i32) -> i32 {
    if x == 0 { 0 } else if x > 0 { 1 } else { -1 }
}

fn move_tail(head: (i32, i32), tail: (i32, i32)) -> (i32, i32) {
    let dx = head.0 - tail.0;
    let dy = head.1 - tail.1;

    // If adjacent or overlapping, don't move
    if dx.abs() <= 1 && dy.abs() <= 1 {
        return tail;
    }

    // Move toward head
    (tail.0 + sign(dx), tail.1 + sign(dy))
}

fn simulate_rope(moves: &[&str], rope_length: usize) -> usize {
    let mut knots: Vec<(i32, i32)> = vec![(0, 0); rope_length];
    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    visited.insert(knots[rope_length - 1]);

    for line in moves {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let direction = parts[0].chars().next().unwrap();
        let count: i32 = parts[1].parse().unwrap();

        let (dx, dy) = match direction {
            'U' => (0, 1),
            'D' => (0, -1),
            'L' => (-1, 0),
            'R' => (1, 0),
            _ => panic!("Unknown direction: {}", direction),
        };

        for _ in 0..count {
            // Move head
            knots[0] = (knots[0].0 + dx, knots[0].1 + dy);

            // Move each subsequent knot
            for i in 1..rope_length {
                knots[i] = move_tail(knots[i - 1], knots[i]);
            }

            visited.insert(knots[rope_length - 1]);
        }
    }

    visited.len()
}

fn part1(moves: &[&str]) -> usize {
    simulate_rope(moves, 2)
}

fn part2(moves: &[&str]) -> usize {
    simulate_rope(moves, 10)
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let exe_dir = exe_path.parent().unwrap();
    let input_path = exe_dir.join("../input.txt");

    let content = fs::read_to_string(&input_path)
        .or_else(|_| fs::read_to_string(Path::new("../input.txt")))
        .expect("Failed to read input.txt");

    let moves: Vec<&str> = content.trim().lines().collect();

    println!("Part 1: {}", part1(&moves));
    println!("Part 2: {}", part2(&moves));
}
