use std::fs;
use std::path::Path;

fn parse_input(text: &str) -> Vec<Vec<&str>> {
    text.trim()
        .split("\n\n")
        .map(|block| block.lines().collect())
        .collect()
}

/// Count character differences between two strings
fn count_differences(s1: &str, s2: &str) -> usize {
    s1.chars()
        .zip(s2.chars())
        .filter(|(c1, c2)| c1 != c2)
        .count()
}

/// Find vertical reflection line where total differences across all rows equals target_diff.
/// For Part 1: target_diff = 0 (perfect reflection)
/// For Part 2: target_diff = 1 (exactly one smudge)
fn find_vertical_reflection(pattern: &[&str], target_diff: usize) -> Option<usize> {
    let width = pattern.first()?.len();

    (1..width).find(|&col| {
        let total_diff: usize = pattern
            .iter()
            .map(|row| {
                let left: String = row[..col].chars().rev().collect();
                let right = &row[col..];
                let min_len = left.len().min(right.len());
                count_differences(&left[..min_len], &right[..min_len])
            })
            .sum();

        total_diff == target_diff
    })
}

/// Find horizontal reflection line where total differences across mirrored rows equals target_diff.
/// For Part 1: target_diff = 0 (perfect reflection)
/// For Part 2: target_diff = 1 (exactly one smudge)
fn find_horizontal_reflection(pattern: &[&str], target_diff: usize) -> Option<usize> {
    let height = pattern.len();

    (1..height).find(|&row| {
        let total_diff: usize = pattern[..row]
            .iter()
            .rev()
            .zip(&pattern[row..])
            .map(|(top, bottom)| count_differences(top, bottom))
            .sum();

        total_diff == target_diff
    })
}

/// Find reflection and compute summary value.
/// Vertical reflection returns column count, horizontal returns row count * 100.
fn summarize_pattern(pattern: &[&str], target_diff: usize) -> usize {
    find_vertical_reflection(pattern, target_diff)
        .or_else(|| find_horizontal_reflection(pattern, target_diff).map(|h| h * 100))
        .unwrap_or(0)
}

fn part1(patterns: &[Vec<&str>]) -> usize {
    patterns.iter().map(|p| summarize_pattern(p, 0)).sum()
}

fn part2(patterns: &[Vec<&str>]) -> usize {
    patterns.iter().map(|p| summarize_pattern(p, 1)).sum()
}

fn main() {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let text = fs::read_to_string(input_path).expect("Failed to read input file");
    let patterns = parse_input(&text);

    println!("Part 1: {}", part1(&patterns));
    println!("Part 2: {}", part2(&patterns));
}
