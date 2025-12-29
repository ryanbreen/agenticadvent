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
    let instructions = lines.next().expect("Expected instructions line");
    lines.next(); // Skip empty line

    let mut network = HashMap::new();
    for line in lines {
        if line.is_empty() {
            continue;
        }
        // Parse: AAA = (BBB, CCC)
        let (node, rest) = line.split_once(" = ").expect("Expected ' = ' separator");
        let rest = rest.trim_start_matches('(').trim_end_matches(')');
        let (left, right) = rest.split_once(", ").expect("Expected ', ' separator");
        network.insert(node, (left, right));
    }

    (instructions, network)
}

/// Navigate from a starting node until the goal predicate is satisfied.
/// Returns the number of steps taken.
fn navigate<F>(
    instructions: &str,
    network: &HashMap<&str, (&str, &str)>,
    start: &str,
    is_goal: F,
) -> u64
where
    F: Fn(&str) -> bool,
{
    let mut current = start;
    let mut steps = 0u64;

    for instruction in instructions.chars().cycle() {
        if is_goal(current) {
            break;
        }
        let (left, right) = network.get(current).expect("Node not found in network");
        current = if instruction == 'L' { left } else { right };
        steps += 1;
    }

    steps
}

fn part1(instructions: &str, network: &HashMap<&str, (&str, &str)>) -> u64 {
    navigate(instructions, network, "AAA", |node| node == "ZZZ")
}

fn part2(instructions: &str, network: &HashMap<&str, (&str, &str)>) -> u64 {
    // Find all starting nodes (ending in 'A')
    let starting_nodes: Vec<&str> = network
        .keys()
        .filter(|k| k.ends_with('A'))
        .copied()
        .collect();

    // For each starting node, find the cycle length to reach a Z node
    let cycle_lengths: Vec<u64> = starting_nodes
        .iter()
        .map(|&start| navigate(instructions, network, start, |node| node.ends_with('Z')))
        .collect();

    // Find LCM of all cycle lengths
    cycle_lengths.into_iter().reduce(lcm).unwrap_or(1)
}

fn main() {
    let text = fs::read_to_string("../input.txt").expect("Failed to read input file");

    let (instructions, network) = parse_input(&text);

    println!("Part 1: {}", part1(instructions, &network));
    println!("Part 2: {}", part2(instructions, &network));
}
