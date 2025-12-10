use std::fs;

#[derive(Clone, Copy)]
enum Operator {
    Add,
    Multiply,
    Concat,
}

fn parse_input(text: &str) -> Vec<(u64, Vec<u64>)> {
    text.trim()
        .lines()
        .map(|line| {
            let parts: Vec<&str> = line.split(": ").collect();
            let target = parts[0].parse::<u64>().unwrap();
            let nums: Vec<u64> = parts[1]
                .split_whitespace()
                .map(|n| n.parse::<u64>().unwrap())
                .collect();
            (target, nums)
        })
        .collect()
}

fn evaluate(nums: &[u64], ops: &[Operator]) -> u64 {
    let mut result = nums[0];
    for (i, op) in ops.iter().enumerate() {
        match op {
            Operator::Add => result += nums[i + 1],
            Operator::Multiply => result *= nums[i + 1],
            Operator::Concat => {
                // Concatenate digits: result || nums[i+1]
                let next = nums[i + 1];
                let mut multiplier = 1u64;
                let mut temp = next;
                while temp > 0 {
                    multiplier *= 10;
                    temp /= 10;
                }
                result = result * multiplier + next;
            }
        }
    }
    result
}

fn can_make_target(target: u64, nums: &[u64], operators: &[Operator]) -> bool {
    let n_ops = nums.len() - 1;
    let num_operators = operators.len();
    let total_combinations = num_operators.pow(n_ops as u32);

    for combo in 0..total_combinations {
        let mut ops = Vec::with_capacity(n_ops);
        let mut temp = combo;
        for _ in 0..n_ops {
            ops.push(operators[temp % num_operators]);
            temp /= num_operators;
        }

        if evaluate(nums, &ops) == target {
            return true;
        }
    }
    false
}

fn part1(equations: &[(u64, Vec<u64>)]) -> u64 {
    let operators = vec![Operator::Add, Operator::Multiply];
    equations
        .iter()
        .filter(|(target, nums)| can_make_target(*target, nums, &operators))
        .map(|(target, _)| target)
        .sum()
}

fn part2(equations: &[(u64, Vec<u64>)]) -> u64 {
    let operators = vec![Operator::Add, Operator::Multiply, Operator::Concat];
    equations
        .iter()
        .filter(|(target, nums)| can_make_target(*target, nums, &operators))
        .map(|(target, _)| target)
        .sum()
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");
    let equations = parse_input(&input);

    println!("Part 1: {}", part1(&equations));
    println!("Part 2: {}", part2(&equations));
}
