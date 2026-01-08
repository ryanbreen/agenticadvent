use std::fs;
use std::path::Path;

fn parse_input(filename: &Path) -> Vec<u64> {
    let content = fs::read_to_string(filename).expect("Failed to read input file");

    content
        .trim()
        .split("\n\n")
        .map(|group| {
            group
                .lines()
                .filter(|line| !line.is_empty())
                .map(|line| line.parse::<u64>().expect("Failed to parse number"))
                .sum()
        })
        .collect()
}

fn part1(elves: &[u64]) -> u64 {
    *elves.iter().max().expect("No elves found")
}

fn part2(elves: &[u64]) -> u64 {
    let mut sorted = elves.to_vec();
    sorted.sort_unstable_by(|a, b| b.cmp(a));
    sorted.iter().take(3).sum()
}

fn main() {
    let exe_path = std::env::current_exe().expect("Failed to get executable path");
    let exe_dir = exe_path.parent().expect("Failed to get executable directory");
    let input_path = exe_dir.join("../input.txt");

    let elves = parse_input(&input_path);

    println!("Part 1: {}", part1(&elves));
    println!("Part 2: {}", part2(&elves));
}
