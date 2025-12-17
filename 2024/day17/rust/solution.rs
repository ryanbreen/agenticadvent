//! Day 17: Chronospatial Computer - 3-bit VM emulator

use std::error::Error;
use std::fs;

/// Extract the numeric value after ": " in a register line
fn extract_register(line: &str) -> u64 {
    line.split(": ").nth(1).unwrap().parse().unwrap()
}

/// Parse the input file and return (A, B, C, program)
fn parse_input(text: &str) -> (u64, u64, u64, Vec<u64>) {
    let lines: Vec<&str> = text.trim().lines().collect();

    let a = extract_register(lines[0]);
    let b = extract_register(lines[1]);
    let c = extract_register(lines[2]);

    let program = lines[4]
        .split(": ")
        .nth(1)
        .unwrap()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();

    (a, b, c, program)
}

/// Resolve a combo operand to its value
fn combo(operand: u64, a: u64, b: u64, c: u64) -> u64 {
    match operand {
        0..=3 => operand,
        4 => a,
        5 => b,
        6 => c,
        _ => panic!("Invalid combo operand: {}", operand),
    }
}

/// Execute the 3-bit computer program and return output
fn run_program(mut a: u64, mut b: u64, mut c: u64, program: &[u64]) -> Vec<u64> {
    let mut ip = 0;
    let mut output = Vec::new();

    while ip < program.len() {
        let opcode = program[ip];
        let operand = program[ip + 1];

        match opcode {
            0 => {
                // adv - A = A >> combo
                a >>= combo(operand, a, b, c);
            }
            1 => {
                // bxl - B = B XOR literal
                b ^= operand;
            }
            2 => {
                // bst - B = combo % 8
                b = combo(operand, a, b, c) & 7;
            }
            3 => {
                // jnz - jump if A != 0
                if a != 0 {
                    ip = operand as usize;
                    continue;
                }
            }
            4 => {
                // bxc - B = B XOR C
                b ^= c;
            }
            5 => {
                // out - output combo % 8
                output.push(combo(operand, a, b, c) & 7);
            }
            6 => {
                // bdv - B = A >> combo
                b = a >> combo(operand, a, b, c);
            }
            7 => {
                // cdv - C = A >> combo
                c = a >> combo(operand, a, b, c);
            }
            _ => panic!("Invalid opcode: {}", opcode),
        }

        ip += 2;
    }

    output
}

/// Part 1: Run the program and return comma-separated output
fn part1(text: &str) -> String {
    let (a, b, c, program) = parse_input(text);
    let output = run_program(a, b, c, &program);
    output
        .iter()
        .map(|n| n.to_string())
        .collect::<Vec<_>>()
        .join(",")
}

/// Part 2: Find initial A value that makes program output itself
fn part2(text: &str) -> u64 {
    let (_, b, c, program) = parse_input(text);

    // The program loops, outputting one digit per iteration, dividing A by 8 each time.
    // We need to find A such that output == program.
    // Work backwards from the last digit - build A 3 bits at a time.

    fn search(
        target_idx: Option<usize>,
        current_a: u64,
        b: u64,
        c: u64,
        program: &[u64],
    ) -> Option<u64> {
        let idx = match target_idx {
            Some(i) => i,
            None => return Some(current_a),
        };

        let is_first_digit = idx == program.len() - 1;

        // Try all 8 possible 3-bit values for this position
        for bits in 0..8u64 {
            let candidate_a = (current_a << 3) | bits;

            // A can't be 0 at start (would halt immediately without output)
            if candidate_a == 0 && is_first_digit {
                continue;
            }

            let output = run_program(candidate_a, b, c, program);

            // Check if output matches the suffix of the program
            if output.as_slice() == &program[idx..] {
                if let Some(result) = search(idx.checked_sub(1), candidate_a, b, c, program) {
                    return Some(result);
                }
            }
        }

        None
    }

    search(Some(program.len() - 1), 0, b, c, &program).unwrap()
}

fn main() -> Result<(), Box<dyn Error>> {
    let text = fs::read_to_string("../input.txt")?;

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));

    Ok(())
}
