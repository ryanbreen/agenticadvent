use std::fs;
use std::path::Path;

fn parse_input() -> Vec<(String, i64)> {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let content = fs::read_to_string(input_path).expect("Failed to read input file");

    content
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
            let parts: Vec<&str> = line.split_whitespace().collect();
            (parts[0].to_string(), parts[1].parse().unwrap())
        })
        .collect()
}

fn part1(commands: &[(String, i64)]) -> i64 {
    let mut horizontal = 0;
    let mut depth = 0;

    for (cmd, val) in commands {
        match cmd.as_str() {
            "forward" => horizontal += val,
            "down" => depth += val,
            "up" => depth -= val,
            _ => {}
        }
    }

    horizontal * depth
}

fn part2(commands: &[(String, i64)]) -> i64 {
    let mut horizontal = 0;
    let mut depth = 0;
    let mut aim = 0;

    for (cmd, val) in commands {
        match cmd.as_str() {
            "forward" => {
                horizontal += val;
                depth += aim * val;
            }
            "down" => aim += val,
            "up" => aim -= val,
            _ => {}
        }
    }

    horizontal * depth
}

fn main() {
    let commands = parse_input();
    println!("Part 1: {}", part1(&commands));
    println!("Part 2: {}", part2(&commands));
}
