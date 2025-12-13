use std::collections::HashMap;
use std::fs;

/// Count how many stones result from a single stone after N blinks using memoization
fn count_stones(value: u64, blinks: u32, memo: &mut HashMap<(u64, u32), u64>) -> u64 {
    // Base case: no more blinks
    if blinks == 0 {
        return 1;
    }

    // Check memo
    let key = (value, blinks);
    if let Some(&result) = memo.get(&key) {
        return result;
    }

    let result = if value == 0 {
        // Rule 1: 0 becomes 1
        count_stones(1, blinks - 1, memo)
    } else {
        // Check if number has even number of digits
        let s = value.to_string();
        let len = s.len();

        if len % 2 == 0 {
            // Rule 2: Even number of digits -> split
            let mid = len / 2;
            let left = s[..mid].parse::<u64>().unwrap();
            let right = s[mid..].parse::<u64>().unwrap();
            count_stones(left, blinks - 1, memo) + count_stones(right, blinks - 1, memo)
        } else {
            // Rule 3: Multiply by 2024
            count_stones(value * 2024, blinks - 1, memo)
        }
    };

    // Store in memo
    memo.insert(key, result);
    result
}

fn part1(stones: &[u64]) -> u64 {
    let mut memo = HashMap::new();
    stones.iter()
        .map(|&stone| count_stones(stone, 25, &mut memo))
        .sum()
}

fn part2(stones: &[u64]) -> u64 {
    let mut memo = HashMap::new();
    stones.iter()
        .map(|&stone| count_stones(stone, 75, &mut memo))
        .sum()
}

fn main() {
    // Read input file
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    // Parse space-separated numbers
    let stones: Vec<u64> = input
        .trim()
        .split_whitespace()
        .map(|s| s.parse().expect("Failed to parse number"))
        .collect();

    println!("Part 1: {}", part1(&stones));
    println!("Part 2: {}", part2(&stones));
}
