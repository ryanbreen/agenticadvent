use std::collections::HashMap;
use std::fs;
use std::path::Path;

#[derive(Clone)]
enum Job {
    Number(i64),
    Operation(String, char, String),
}

fn parse_input(text: &str) -> HashMap<String, Job> {
    let mut monkeys = HashMap::new();

    for line in text.trim().lines() {
        let parts: Vec<&str> = line.split(": ").collect();
        let name = parts[0].to_string();
        let job_parts: Vec<&str> = parts[1].split_whitespace().collect();

        let job = if job_parts.len() == 1 {
            Job::Number(job_parts[0].parse().unwrap())
        } else {
            let left = job_parts[0].to_string();
            let op = job_parts[1].chars().next().unwrap();
            let right = job_parts[2].to_string();
            Job::Operation(left, op, right)
        };

        monkeys.insert(name, job);
    }

    monkeys
}

fn evaluate(monkeys: &HashMap<String, Job>, name: &str, memo: &mut HashMap<String, i64>) -> i64 {
    if let Some(&val) = memo.get(name) {
        return val;
    }

    let result = match &monkeys[name] {
        Job::Number(n) => *n,
        Job::Operation(left, op, right) => {
            let left_val = evaluate(monkeys, left, memo);
            let right_val = evaluate(monkeys, right, memo);

            match op {
                '+' => left_val + right_val,
                '-' => left_val - right_val,
                '*' => left_val * right_val,
                '/' => left_val / right_val,
                _ => panic!("Unknown operator: {}", op),
            }
        }
    };

    memo.insert(name.to_string(), result);
    result
}

fn part1(text: &str) -> i64 {
    let monkeys = parse_input(text);
    let mut memo = HashMap::new();
    evaluate(&monkeys, "root", &mut memo)
}

fn contains_humn(monkeys: &HashMap<String, Job>, name: &str, memo: &mut HashMap<String, bool>) -> bool {
    if let Some(&val) = memo.get(name) {
        return val;
    }

    if name == "humn" {
        return true;
    }

    let result = match &monkeys[name] {
        Job::Number(_) => false,
        Job::Operation(left, _, right) => {
            contains_humn(monkeys, left, memo) || contains_humn(monkeys, right, memo)
        }
    };

    memo.insert(name.to_string(), result);
    result
}

fn solve_for_humn(
    monkeys: &HashMap<String, Job>,
    name: &str,
    target: i64,
    humn_memo: &mut HashMap<String, bool>,
    eval_memo: &mut HashMap<String, i64>,
) -> i64 {
    if name == "humn" {
        return target;
    }

    match &monkeys[name] {
        Job::Number(_) => panic!("Cannot solve for humn in a number node"),
        Job::Operation(left, op, right) => {
            let left_has_humn = contains_humn(monkeys, left, humn_memo);

            if left_has_humn {
                let right_val = evaluate(monkeys, right, eval_memo);
                let new_target = match op {
                    '+' => target - right_val,       // left + right = target => left = target - right
                    '-' => target + right_val,       // left - right = target => left = target + right
                    '*' => target / right_val,       // left * right = target => left = target / right
                    '/' => target * right_val,       // left / right = target => left = target * right
                    _ => panic!("Unknown operator: {}", op),
                };
                solve_for_humn(monkeys, left, new_target, humn_memo, eval_memo)
            } else {
                let left_val = evaluate(monkeys, left, eval_memo);
                let new_target = match op {
                    '+' => target - left_val,        // left + right = target => right = target - left
                    '-' => left_val - target,        // left - right = target => right = left - target
                    '*' => target / left_val,        // left * right = target => right = target / left
                    '/' => left_val / target,        // left / right = target => right = left / target
                    _ => panic!("Unknown operator: {}", op),
                };
                solve_for_humn(monkeys, right, new_target, humn_memo, eval_memo)
            }
        }
    }
}

fn part2(text: &str) -> i64 {
    let monkeys = parse_input(text);

    // Get root's children
    let (left, right) = match &monkeys["root"] {
        Job::Operation(l, _, r) => (l.clone(), r.clone()),
        Job::Number(_) => panic!("Root should be an operation"),
    };

    let mut humn_memo = HashMap::new();
    let mut eval_memo = HashMap::new();

    let left_has_humn = contains_humn(&monkeys, &left, &mut humn_memo);

    if left_has_humn {
        let target = evaluate(&monkeys, &right, &mut eval_memo);
        solve_for_humn(&monkeys, &left, target, &mut humn_memo, &mut eval_memo)
    } else {
        let target = evaluate(&monkeys, &left, &mut eval_memo);
        solve_for_humn(&monkeys, &right, target, &mut humn_memo, &mut eval_memo)
    }
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let exe_dir = exe_path.parent().unwrap();
    let input_path = exe_dir.join("../input.txt");

    // Try multiple possible input locations
    let input_path = if input_path.exists() {
        input_path
    } else {
        // Fallback to current directory parent
        Path::new("../input.txt").to_path_buf()
    };

    let text = fs::read_to_string(&input_path)
        .expect("Failed to read input file");

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));
}
