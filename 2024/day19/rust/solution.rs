use std::collections::HashMap;
use std::fs;
use std::path::Path;

fn count_ways(design: &str, patterns: &[&str], memo: &mut HashMap<usize, u64>) -> u64 {
    let design_bytes = design.as_bytes();

    fn dp(pos: usize, design_bytes: &[u8], patterns: &[&str], memo: &mut HashMap<usize, u64>) -> u64 {
        if pos == design_bytes.len() {
            return 1;
        }

        if let Some(&cached) = memo.get(&pos) {
            return cached;
        }

        let mut total: u64 = 0;
        for pattern in patterns {
            let plen = pattern.len();
            if pos + plen <= design_bytes.len() {
                if &design_bytes[pos..pos + plen] == pattern.as_bytes() {
                    total += dp(pos + plen, design_bytes, patterns, memo);
                }
            }
        }

        memo.insert(pos, total);
        total
    }

    dp(0, design_bytes, patterns, memo)
}

fn part1(patterns: &[&str], designs: &[&str]) -> u64 {
    let mut count = 0;
    for design in designs {
        let mut memo = HashMap::new();
        if count_ways(design, patterns, &mut memo) > 0 {
            count += 1;
        }
    }
    count
}

fn part2(patterns: &[&str], designs: &[&str]) -> u64 {
    let mut total: u64 = 0;
    for design in designs {
        let mut memo = HashMap::new();
        total += count_ways(design, patterns, &mut memo);
    }
    total
}

fn main() {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let input = fs::read_to_string(&input_path)
        .unwrap_or_else(|_| fs::read_to_string("../input.txt").expect("Failed to read input.txt"));
    let input = input.trim();

    let parts: Vec<&str> = input.split("\n\n").collect();
    let patterns: Vec<&str> = parts[0].split(',').map(|s| s.trim()).collect();
    let designs: Vec<&str> = parts[1].lines().collect();

    println!("Part 1: {}", part1(&patterns, &designs));
    println!("Part 2: {}", part2(&patterns, &designs));
}
