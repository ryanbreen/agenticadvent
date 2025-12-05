use std::collections::{HashMap, HashSet};
use std::fs;

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let (rules, updates) = parse_input(&input);

    println!("Part 1: {}", part1(&rules, &updates));
    println!("Part 2: {}", part2(&rules, &updates));
}

fn parse_input(input: &str) -> (HashMap<i32, HashSet<i32>>, Vec<Vec<i32>>) {
    let sections: Vec<&str> = input.trim().split("\n\n").collect();

    // Parse rules: X|Y means X must come before Y
    // Store as: rules[X] = set of pages that must come AFTER X
    let mut rules: HashMap<i32, HashSet<i32>> = HashMap::new();
    for line in sections[0].lines() {
        let parts: Vec<&str> = line.split('|').collect();
        let before: i32 = parts[0].parse().unwrap();
        let after: i32 = parts[1].parse().unwrap();
        rules.entry(before).or_insert_with(HashSet::new).insert(after);
    }

    // Parse updates
    let updates: Vec<Vec<i32>> = sections[1]
        .lines()
        .map(|line| {
            line.split(',')
                .map(|n| n.parse().unwrap())
                .collect()
        })
        .collect();

    (rules, updates)
}

fn is_valid_order(update: &[i32], rules: &HashMap<i32, HashSet<i32>>) -> bool {
    let page_positions: HashMap<i32, usize> = update
        .iter()
        .enumerate()
        .map(|(i, &page)| (page, i))
        .collect();

    for (i, &page) in update.iter().enumerate() {
        // Check all pages that must come after this page
        if let Some(must_be_after_set) = rules.get(&page) {
            for &must_be_after in must_be_after_set {
                if let Some(&after_pos) = page_positions.get(&must_be_after) {
                    if after_pos < i {
                        return false;
                    }
                }
            }
        }
    }
    true
}

fn part1(rules: &HashMap<i32, HashSet<i32>>, updates: &[Vec<i32>]) -> i32 {
    let mut total = 0;
    for update in updates {
        if is_valid_order(update, rules) {
            let middle_idx = update.len() / 2;
            total += update[middle_idx];
        }
    }
    total
}

fn fix_order(update: &[i32], rules: &HashMap<i32, HashSet<i32>>) -> Vec<i32> {
    let mut sorted = update.to_vec();

    // Sort using a custom comparator
    sorted.sort_by(|&a, &b| {
        // If a must come before b
        if let Some(after_set) = rules.get(&a) {
            if after_set.contains(&b) {
                return std::cmp::Ordering::Less;
            }
        }
        // If b must come before a
        if let Some(after_set) = rules.get(&b) {
            if after_set.contains(&a) {
                return std::cmp::Ordering::Greater;
            }
        }
        std::cmp::Ordering::Equal
    });

    sorted
}

fn part2(rules: &HashMap<i32, HashSet<i32>>, updates: &[Vec<i32>]) -> i32 {
    let mut total = 0;
    for update in updates {
        if !is_valid_order(update, rules) {
            let fixed = fix_order(update, rules);
            let middle_idx = fixed.len() / 2;
            total += fixed[middle_idx];
        }
    }
    total
}
