use std::fs;
use std::path::Path;

fn parse_input(text: &str) -> Vec<Vec<String>> {
    text.trim()
        .split("\n\n")
        .map(|block| block.lines().map(String::from).collect())
        .collect()
}

fn find_vertical_reflection(pattern: &[String]) -> usize {
    if pattern.is_empty() {
        return 0;
    }
    let width = pattern[0].len();

    for col in 1..width {
        let mut is_reflection = true;
        for row in pattern {
            let left: String = row[..col].chars().rev().collect();
            let right = &row[col..];
            let min_len = left.len().min(right.len());
            if &left[..min_len] != &right[..min_len] {
                is_reflection = false;
                break;
            }
        }
        if is_reflection {
            return col;
        }
    }
    0
}

fn find_horizontal_reflection(pattern: &[String]) -> usize {
    if pattern.is_empty() {
        return 0;
    }
    let height = pattern.len();

    for row in 1..height {
        let mut is_reflection = true;
        let top: Vec<&String> = pattern[..row].iter().rev().collect();
        let bottom = &pattern[row..];
        let min_len = top.len().min(bottom.len());

        for i in 0..min_len {
            if top[i] != &bottom[i] {
                is_reflection = false;
                break;
            }
        }
        if is_reflection {
            return row;
        }
    }
    0
}

fn summarize_pattern(pattern: &[String]) -> usize {
    let v = find_vertical_reflection(pattern);
    if v > 0 {
        return v;
    }
    let h = find_horizontal_reflection(pattern);
    h * 100
}

fn part1(patterns: &[Vec<String>]) -> usize {
    patterns.iter().map(|p| summarize_pattern(p)).sum()
}

fn count_differences(s1: &str, s2: &str) -> usize {
    s1.chars()
        .zip(s2.chars())
        .filter(|(c1, c2)| c1 != c2)
        .count()
}

fn find_vertical_reflection_with_smudge(pattern: &[String]) -> usize {
    if pattern.is_empty() {
        return 0;
    }
    let width = pattern[0].len();

    for col in 1..width {
        let mut total_diff = 0;
        for row in pattern {
            let left: String = row[..col].chars().rev().collect();
            let right = &row[col..];
            let min_len = left.len().min(right.len());
            total_diff += count_differences(&left[..min_len], &right[..min_len]);
            if total_diff > 1 {
                break;
            }
        }
        if total_diff == 1 {
            return col;
        }
    }
    0
}

fn find_horizontal_reflection_with_smudge(pattern: &[String]) -> usize {
    if pattern.is_empty() {
        return 0;
    }
    let height = pattern.len();

    for row in 1..height {
        let mut total_diff = 0;
        let top: Vec<&String> = pattern[..row].iter().rev().collect();
        let bottom = &pattern[row..];
        let min_len = top.len().min(bottom.len());

        for i in 0..min_len {
            total_diff += count_differences(top[i], &bottom[i]);
            if total_diff > 1 {
                break;
            }
        }
        if total_diff == 1 {
            return row;
        }
    }
    0
}

fn summarize_pattern_with_smudge(pattern: &[String]) -> usize {
    let v = find_vertical_reflection_with_smudge(pattern);
    if v > 0 {
        return v;
    }
    let h = find_horizontal_reflection_with_smudge(pattern);
    h * 100
}

fn part2(patterns: &[Vec<String>]) -> usize {
    patterns.iter().map(|p| summarize_pattern_with_smudge(p)).sum()
}

fn main() {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let text = fs::read_to_string(input_path).expect("Failed to read input file");
    let patterns = parse_input(&text);

    println!("Part 1: {}", part1(&patterns));
    println!("Part 2: {}", part2(&patterns));
}
