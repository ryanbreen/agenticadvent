use std::fs;
use std::path::Path;

fn simulate_cpu(instructions: &[&str]) -> Vec<(i32, i32)> {
    let mut x: i32 = 1;
    let mut cycle: i32 = 0;
    let mut cycles = Vec::new();

    for line in instructions {
        if *line == "noop" {
            cycle += 1;
            cycles.push((cycle, x));
        } else {
            // addx V
            let v: i32 = line.split_whitespace().nth(1).unwrap().parse().unwrap();
            cycle += 1;
            cycles.push((cycle, x));
            cycle += 1;
            cycles.push((cycle, x));
            x += v;
        }
    }

    cycles
}

fn part1(instructions: &[&str]) -> i32 {
    let target_cycles = [20, 60, 100, 140, 180, 220];
    let mut total = 0;

    for (cycle, x) in simulate_cpu(instructions) {
        if target_cycles.contains(&cycle) {
            total += cycle * x;
        }
    }

    total
}

fn part2(instructions: &[&str]) -> String {
    let mut screen = Vec::new();
    let mut row = String::new();

    for (cycle, x) in simulate_cpu(instructions) {
        let pos = (cycle - 1) % 40;
        if (pos - x).abs() <= 1 {
            row.push('#');
        } else {
            row.push('.');
        }

        if cycle % 40 == 0 {
            screen.push(row.clone());
            row.clear();
        }
    }

    screen.join("\n")
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let exe_dir = exe_path.parent().unwrap();
    let input_path = exe_dir.join("../input.txt");

    // Fallback to relative path if exe-relative doesn't exist
    let input_path = if input_path.exists() {
        input_path
    } else {
        Path::new("../input.txt").to_path_buf()
    };

    let content = fs::read_to_string(&input_path).expect("Failed to read input file");
    let instructions: Vec<&str> = content.trim().split('\n').collect();

    println!("Part 1: {}", part1(&instructions));
    println!("Part 2:");
    println!("{}", part2(&instructions));
}
