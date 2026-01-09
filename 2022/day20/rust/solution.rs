use std::fs;
use std::path::Path;

fn parse_input(text: &str) -> Vec<i64> {
    text.trim()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect()
}

fn mix(numbers: &[i64], times: usize) -> Vec<i64> {
    let n = numbers.len();
    // Store (original_index, value) pairs
    let mut indexed: Vec<(usize, i64)> = numbers.iter().enumerate().map(|(i, &v)| (i, v)).collect();

    for _ in 0..times {
        for orig_idx in 0..n {
            // Find current position of this element
            let curr_pos = indexed.iter().position(|&(idx, _)| idx == orig_idx).unwrap();

            // Remove from current position
            let item = indexed.remove(curr_pos);

            // Calculate new position (modulo n-1 because we removed the element)
            let new_pos = (curr_pos as i64 + item.1).rem_euclid((n - 1) as i64) as usize;

            // Insert at new position
            indexed.insert(new_pos, item);
        }
    }

    indexed.iter().map(|&(_, val)| val).collect()
}

fn grove_coordinates(mixed: &[i64]) -> i64 {
    let n = mixed.len();
    let zero_idx = mixed.iter().position(|&x| x == 0).unwrap();
    [1000, 2000, 3000]
        .iter()
        .map(|&offset| mixed[(zero_idx + offset) % n])
        .sum()
}

fn part1(text: &str) -> i64 {
    let numbers = parse_input(text);
    let mixed = mix(&numbers, 1);
    grove_coordinates(&mixed)
}

fn part2(text: &str) -> i64 {
    let numbers = parse_input(text);
    let decryption_key: i64 = 811589153;
    let scaled: Vec<i64> = numbers.iter().map(|&n| n * decryption_key).collect();
    let mixed = mix(&scaled, 10);
    grove_coordinates(&mixed)
}

fn main() {
    let script_dir = Path::new(file!()).parent().unwrap();
    let input_path = script_dir.join("..").join("input.txt");
    let text = fs::read_to_string(&input_path)
        .unwrap_or_else(|_| fs::read_to_string("../input.txt").expect("Could not read input file"));

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));
}
