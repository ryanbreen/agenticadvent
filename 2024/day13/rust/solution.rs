use std::fs;

#[derive(Debug)]
struct Machine {
    ax: i64,
    ay: i64,
    bx: i64,
    by: i64,
    px: i64,
    py: i64,
}

fn parse_machines(text: &str) -> Vec<Machine> {
    let mut machines = Vec::new();

    for block in text.split("\n\n") {
        let lines: Vec<&str> = block.trim().split('\n').collect();
        if lines.len() < 3 {
            continue;
        }

        // Parse Button A: X+94, Y+34
        let a_line = lines[0];
        let ax = extract_number(a_line, "X+");
        let ay = extract_number(a_line, "Y+");

        // Parse Button B: X+22, Y+67
        let b_line = lines[1];
        let bx = extract_number(b_line, "X+");
        let by = extract_number(b_line, "Y+");

        // Parse Prize: X=8400, Y=5400
        let p_line = lines[2];
        let px = extract_number(p_line, "X=");
        let py = extract_number(p_line, "Y=");

        machines.push(Machine { ax, ay, bx, by, px, py });
    }

    machines
}

fn extract_number(line: &str, prefix: &str) -> i64 {
    let start = line.find(prefix).unwrap() + prefix.len();
    let rest = &line[start..];
    let end = rest.find(|c: char| !c.is_ascii_digit()).unwrap_or(rest.len());
    rest[..end].parse::<i64>().unwrap()
}

fn solve_machine(machine: &Machine, max_presses: Option<i64>) -> Option<i64> {
    // Solve using Cramer's rule
    // System of equations:
    //   a*ax + b*bx = px
    //   a*ay + b*by = py
    //
    // Solution:
    //   det = ax*by - ay*bx
    //   a = (px*by - py*bx) / det
    //   b = (ax*py - ay*px) / det

    let det = machine.ax * machine.by - machine.ay * machine.bx;

    if det == 0 {
        return None; // No unique solution
    }

    // Calculate using integer arithmetic
    let a_num = machine.px * machine.by - machine.py * machine.bx;
    let b_num = machine.ax * machine.py - machine.ay * machine.px;

    // Check if solutions are integers
    if a_num % det != 0 || b_num % det != 0 {
        return None;
    }

    let a = a_num / det;
    let b = b_num / det;

    // Check non-negative
    if a < 0 || b < 0 {
        return None;
    }

    // Check max presses constraint (Part 1)
    if let Some(max) = max_presses {
        if a > max || b > max {
            return None;
        }
    }

    Some(3 * a + b)
}

fn part1(machines: &[Machine]) -> i64 {
    let mut total = 0;

    for machine in machines {
        if let Some(cost) = solve_machine(machine, Some(100)) {
            total += cost;
        }
    }

    total
}

fn part2(machines: &[Machine]) -> i64 {
    let offset = 10_000_000_000_000i64;
    let mut total = 0;

    for machine in machines {
        // Shift prize coordinates
        let shifted_machine = Machine {
            ax: machine.ax,
            ay: machine.ay,
            bx: machine.bx,
            by: machine.by,
            px: machine.px + offset,
            py: machine.py + offset,
        };

        if let Some(cost) = solve_machine(&shifted_machine, None) {
            total += cost;
        }
    }

    total
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let machines = parse_machines(&input.trim());

    println!("Part 1: {}", part1(&machines));
    println!("Part 2: {}", part2(&machines));
}
