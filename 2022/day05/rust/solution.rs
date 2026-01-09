use std::fs;
use std::path::Path;

fn parse_input(filename: &Path) -> (Vec<Vec<char>>, Vec<(usize, usize, usize)>) {
    let content = fs::read_to_string(filename).expect("Failed to read input file");
    let parts: Vec<&str> = content.split("\n\n").collect();

    let stack_lines: Vec<&str> = parts[0].lines().collect();
    let move_lines: Vec<&str> = parts[1].trim().lines().collect();

    // Find number of stacks from the last line (the numbers)
    let num_stacks = stack_lines.last().unwrap().split_whitespace().count();

    // Parse stacks (top-down, excluding the number line)
    let mut stacks: Vec<Vec<char>> = vec![Vec::new(); num_stacks];
    for line in &stack_lines[..stack_lines.len() - 1] {
        let chars: Vec<char> = line.chars().collect();
        for i in 0..num_stacks {
            let pos = 1 + i * 4; // Position of crate letter
            if pos < chars.len() && chars[pos] != ' ' {
                stacks[i].push(chars[pos]);
            }
        }
    }

    // Reverse so bottom is at index 0
    for stack in &mut stacks {
        stack.reverse();
    }

    // Parse moves
    let mut moves = Vec::new();
    for line in move_lines {
        // Parse "move N from X to Y"
        let words: Vec<&str> = line.split_whitespace().collect();
        if words.len() >= 6 {
            let count: usize = words[1].parse().unwrap();
            let from_stack: usize = words[3].parse::<usize>().unwrap() - 1; // 0-indexed
            let to_stack: usize = words[5].parse::<usize>().unwrap() - 1;
            moves.push((count, from_stack, to_stack));
        }
    }

    (stacks, moves)
}

fn part1(stacks: &[Vec<char>], moves: &[(usize, usize, usize)]) -> String {
    let mut stacks: Vec<Vec<char>> = stacks.to_vec();

    for &(count, from_stack, to_stack) in moves {
        for _ in 0..count {
            if let Some(crate_char) = stacks[from_stack].pop() {
                stacks[to_stack].push(crate_char);
            }
        }
    }

    stacks
        .iter()
        .filter_map(|stack| stack.last())
        .collect()
}

fn part2(stacks: &[Vec<char>], moves: &[(usize, usize, usize)]) -> String {
    let mut stacks: Vec<Vec<char>> = stacks.to_vec();

    for &(count, from_stack, to_stack) in moves {
        // Move multiple crates at once (preserve order)
        let from_len = stacks[from_stack].len();
        let crates: Vec<char> = stacks[from_stack].drain(from_len - count..).collect();
        stacks[to_stack].extend(crates);
    }

    stacks
        .iter()
        .filter_map(|stack| stack.last())
        .collect()
}

fn main() {
    let script_dir = Path::new(file!()).parent().unwrap();
    let input_file = script_dir.join("..").join("input.txt");

    let (stacks, moves) = parse_input(&input_file);

    println!("Part 1: {}", part1(&stacks, &moves));
    println!("Part 2: {}", part2(&stacks, &moves));
}
