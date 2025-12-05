use std::fs;

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input.txt");

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

fn part1(input: &str) -> usize {
    let lines: Vec<&str> = input.lines().collect();

    // Find the blank line separator
    let blank_idx = lines.iter()
        .position(|&line| line.is_empty())
        .expect("No blank line found");

    // Parse ranges from the first section
    let ranges: Vec<(i64, i64)> = lines[..blank_idx]
        .iter()
        .map(|line| {
            let parts: Vec<&str> = line.split('-').collect();
            let start = parts[0].parse().expect("Failed to parse start");
            let end = parts[1].parse().expect("Failed to parse end");
            (start, end)
        })
        .collect();

    // Parse ingredient IDs from the second section
    let ingredient_ids: Vec<i64> = lines[blank_idx + 1..]
        .iter()
        .filter(|line| !line.is_empty())
        .map(|line| line.parse().expect("Failed to parse ingredient ID"))
        .collect();

    // Count how many ingredient IDs fall within any range
    let mut fresh_count = 0;
    for ingredient_id in ingredient_ids {
        for &(start, end) in &ranges {
            if start <= ingredient_id && ingredient_id <= end {
                fresh_count += 1;
                break; // Found a match, no need to check other ranges
            }
        }
    }

    fresh_count
}

fn part2(input: &str) -> i64 {
    let lines: Vec<&str> = input.lines().collect();

    // Find the blank line separator
    let blank_idx = lines.iter()
        .position(|&line| line.is_empty())
        .expect("No blank line found");

    // Parse ranges from the first section
    let mut ranges: Vec<(i64, i64)> = lines[..blank_idx]
        .iter()
        .map(|line| {
            let parts: Vec<&str> = line.split('-').collect();
            let start = parts[0].parse().expect("Failed to parse start");
            let end = parts[1].parse().expect("Failed to parse end");
            (start, end)
        })
        .collect();

    // Sort ranges by start position
    ranges.sort_by_key(|&(start, _)| start);

    // Merge overlapping ranges
    let mut merged: Vec<(i64, i64)> = Vec::new();
    for (start, end) in ranges {
        if !merged.is_empty() && start <= merged.last().unwrap().1 + 1 {
            // Overlapping or adjacent - merge with the last range
            let last_idx = merged.len() - 1;
            merged[last_idx].1 = merged[last_idx].1.max(end);
        } else {
            // No overlap - add as new range
            merged.push((start, end));
        }
    }

    // Count total unique IDs covered by merged ranges
    let mut total_count = 0;
    for (start, end) in merged {
        total_count += end - start + 1;
    }

    total_count
}
