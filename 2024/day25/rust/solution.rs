use std::fs;

type Heights = [usize; 5];

fn parse_input(text: &str) -> (Vec<Heights>, Vec<Heights>) {
    let mut locks = Vec::new();
    let mut keys = Vec::new();

    let schematics: Vec<&str> = text.trim().split("\n\n").collect();

    for schematic in schematics {
        let lines: Vec<&str> = schematic.trim().lines().collect();

        if lines[0] == "#####" {
            // It's a lock - count # from top (excluding top row)
            let mut heights = [0; 5];
            for col in 0..5 {
                let mut height = 0;
                for row in 1..7 {
                    // rows 1-6
                    if lines[row].as_bytes()[col] == b'#' {
                        height += 1;
                    } else {
                        break;
                    }
                }
                heights[col] = height;
            }
            locks.push(heights);
        } else {
            // It's a key - count # from bottom (excluding bottom row)
            let mut heights = [0; 5];
            for col in 0..5 {
                let mut height = 0;
                for row in (0..=5).rev() {
                    // rows 5 down to 0
                    if lines[row].as_bytes()[col] == b'#' {
                        height += 1;
                    } else {
                        break;
                    }
                }
                heights[col] = height;
            }
            keys.push(heights);
        }
    }

    (locks, keys)
}

fn fits(lock: &Heights, key: &Heights) -> bool {
    for i in 0..5 {
        if lock[i] + key[i] > 5 {
            return false;
        }
    }
    true
}

fn part1(locks: &[Heights], keys: &[Heights]) -> usize {
    let mut count = 0;
    for lock in locks {
        for key in keys {
            if fits(lock, key) {
                count += 1;
            }
        }
    }
    count
}

fn main() {
    let text = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let (locks, keys) = parse_input(&text);

    let answer1 = part1(&locks, &keys);
    println!("Part 1: {}", answer1);

    // Day 25 typically only has Part 1
    println!("Part 2: Merry Christmas!");
}
