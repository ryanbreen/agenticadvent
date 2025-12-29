use std::collections::HashMap;
use std::fs;

fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 { a } else { gcd(b, a % b) }
}

fn lcm(a: u64, b: u64) -> u64 {
    a / gcd(a, b) * b
}

fn parse_input(text: &str) -> (&str, HashMap<&str, (&str, &str)>) {
    let mut lines = text.lines();
    let instructions = lines.next().unwrap();
    lines.next(); // Skip empty line

    let mut network = HashMap::new();
    for line in lines {
        if line.is_empty() {
            continue;
        }
        // Parse: AAA = (BBB, CCC)
        let (node, rest) = line.split_once(" = ").unwrap();
        let rest = &rest[1..rest.len() - 1]; // Remove parentheses
        let (left, right) = rest.split_once(", ").unwrap();
        network.insert(node, (left, right));
    }

    (instructions, network)
}

fn part1(instructions: &str, network: &HashMap<&str, (&str, &str)>) -> u64 {
    let mut current = "AAA";
    let mut steps = 0u64;
    let instruction_bytes = instructions.as_bytes();
    let instruction_len = instruction_bytes.len();

    while current != "ZZZ" {
        let instruction = instruction_bytes[steps as usize % instruction_len];
        let (left, right) = network.get(current).unwrap();
        current = if instruction == b'L' { left } else { right };
        steps += 1;
    }

    steps
}

fn part2(instructions: &str, network: &HashMap<&str, (&str, &str)>) -> u64 {
    let instruction_bytes = instructions.as_bytes();
    let instruction_len = instruction_bytes.len();

    // Find all starting nodes (ending in 'A')
    let starting_nodes: Vec<&str> = network
        .keys()
        .filter(|k| k.ends_with('A'))
        .copied()
        .collect();

    // For each starting node, find the cycle length to reach a Z node
    let cycle_lengths: Vec<u64> = starting_nodes
        .iter()
        .map(|&start| {
            let mut current = start;
            let mut steps = 0u64;
            while !current.ends_with('Z') {
                let instruction = instruction_bytes[steps as usize % instruction_len];
                let (left, right) = network.get(current).unwrap();
                current = if instruction == b'L' { left } else { right };
                steps += 1;
            }
            steps
        })
        .collect();

    // Find LCM of all cycle lengths
    cycle_lengths.iter().fold(1u64, |acc, &len| lcm(acc, len))
}

fn main() {
    let text = fs::read_to_string("../input.txt").expect("Failed to read input file");

    let (instructions, network) = parse_input(&text);

    println!("Part 1: {}", part1(instructions, &network));
    println!("Part 2: {}", part2(instructions, &network));
}
