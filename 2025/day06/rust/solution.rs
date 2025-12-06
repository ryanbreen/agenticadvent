use std::fs;
use std::path::PathBuf;

fn read_input() -> String {
    let mut path = PathBuf::from(file!());
    path.pop();
    path.push("../input.txt");
    fs::read_to_string(path).expect("Failed to read input file")
}

fn parse_problems(lines: &[String]) -> Vec<(Vec<i64>, char)> {
    if lines.is_empty() {
        return Vec::new();
    }

    // Find the operator row (last non-empty row with only +, *, and spaces)
    let mut op_row_idx = lines.len();
    for (i, line) in lines.iter().enumerate().rev() {
        if line.trim().is_empty() {
            continue;
        }
        if line.chars().all(|c| c == '+' || c == '*' || c == ' ') {
            op_row_idx = i;
            break;
        }
    }

    if op_row_idx >= lines.len() {
        return Vec::new();
    }

    let op_row = &lines[op_row_idx];
    let number_rows = &lines[..op_row_idx];

    // Find max width
    let max_width = lines.iter().map(|line| line.len()).max().unwrap_or(0);

    // Pad all rows to the same width
    let padded_number_rows: Vec<String> = number_rows
        .iter()
        .map(|line| format!("{:width$}", line, width = max_width))
        .collect();
    let padded_op_row = format!("{:width$}", op_row, width = max_width);

    let mut problems = Vec::new();
    let mut col = 0;

    while col < max_width {
        // Skip separator columns (all spaces)
        while col < max_width {
            let all_spaces = padded_number_rows.iter().all(|row| {
                row.chars().nth(col).unwrap_or(' ') == ' '
            }) && padded_op_row.chars().nth(col).unwrap_or(' ') == ' ';

            if !all_spaces {
                break;
            }
            col += 1;
        }

        if col >= max_width {
            break;
        }

        // Find the end of this problem
        let start_col = col;
        while col < max_width {
            // Check if this is a separator column
            let is_separator = padded_number_rows.iter().all(|row| {
                row.chars().nth(col).unwrap_or(' ') == ' '
            }) && padded_op_row.chars().nth(col).unwrap_or(' ') == ' ';

            if is_separator {
                break;
            }
            col += 1;
        }

        let end_col = col;

        // Extract numbers and operator for this problem
        let mut numbers = Vec::new();
        for row in &padded_number_rows {
            let num_str: String = row
                .chars()
                .skip(start_col)
                .take(end_col - start_col)
                .collect::<String>()
                .trim()
                .to_string();

            if !num_str.is_empty() {
                if let Ok(num) = num_str.parse::<i64>() {
                    numbers.push(num);
                }
            }
        }

        let op_str: String = padded_op_row
            .chars()
            .skip(start_col)
            .take(end_col - start_col)
            .collect::<String>()
            .trim()
            .to_string();

        if !op_str.is_empty() && !numbers.is_empty() {
            let op = op_str.chars().next().unwrap();
            problems.push((numbers, op));
        }
    }

    problems
}

fn parse_problems_part2(lines: &[String]) -> Vec<(Vec<i64>, char)> {
    if lines.is_empty() {
        return Vec::new();
    }

    // Find the operator row (last non-empty row with only +, *, and spaces)
    let mut op_row_idx = lines.len();
    for (i, line) in lines.iter().enumerate().rev() {
        if line.trim().is_empty() {
            continue;
        }
        if line.chars().all(|c| c == '+' || c == '*' || c == ' ') {
            op_row_idx = i;
            break;
        }
    }

    if op_row_idx >= lines.len() {
        return Vec::new();
    }

    let op_row = &lines[op_row_idx];
    let number_rows = &lines[..op_row_idx];

    // Find max width
    let max_width = lines.iter().map(|line| line.len()).max().unwrap_or(0);

    // Pad all rows to the same width
    let padded_number_rows: Vec<String> = number_rows
        .iter()
        .map(|line| format!("{:width$}", line, width = max_width))
        .collect();
    let padded_op_row = format!("{:width$}", op_row, width = max_width);

    let mut problems = Vec::new();
    let mut col = 0;

    while col < max_width {
        // Skip separator columns (all spaces)
        while col < max_width {
            let all_spaces = padded_number_rows.iter().all(|row| {
                row.chars().nth(col).unwrap_or(' ') == ' '
            }) && padded_op_row.chars().nth(col).unwrap_or(' ') == ' ';

            if !all_spaces {
                break;
            }
            col += 1;
        }

        if col >= max_width {
            break;
        }

        // Find the end of this problem
        let start_col = col;
        while col < max_width {
            // Check if this is a separator column
            let is_separator = padded_number_rows.iter().all(|row| {
                row.chars().nth(col).unwrap_or(' ') == ' '
            }) && padded_op_row.chars().nth(col).unwrap_or(' ') == ' ';

            if is_separator {
                break;
            }
            col += 1;
        }

        let end_col = col;

        // For Part 2: Read columns right-to-left, each column forms a number
        // reading top-to-bottom as most-to-least significant digit
        let mut numbers = Vec::new();
        for c in (start_col..end_col).rev() {
            let mut digits = Vec::new();
            for row in &padded_number_rows {
                let ch = row.chars().nth(c).unwrap_or(' ');
                if ch.is_ascii_digit() {
                    digits.push(ch);
                }
            }

            if !digits.is_empty() {
                // Join digits to form number (top=most significant, bottom=least)
                let num_str: String = digits.iter().collect();
                if let Ok(num) = num_str.parse::<i64>() {
                    numbers.push(num);
                }
            }
        }

        let op_str: String = padded_op_row
            .chars()
            .skip(start_col)
            .take(end_col - start_col)
            .collect::<String>()
            .trim()
            .to_string();

        if !op_str.is_empty() && !numbers.is_empty() {
            let op = op_str.chars().next().unwrap();
            problems.push((numbers, op));
        }
    }

    problems
}

fn solve_problem(numbers: &[i64], op: char) -> i64 {
    match op {
        '+' => numbers.iter().sum(),
        '*' => numbers.iter().product(),
        _ => 0,
    }
}

fn part1(lines: &[String]) -> i64 {
    let problems = parse_problems(lines);
    problems
        .iter()
        .map(|(numbers, op)| solve_problem(numbers, *op))
        .sum()
}

fn part2(lines: &[String]) -> i64 {
    let problems = parse_problems_part2(lines);
    problems
        .iter()
        .map(|(numbers, op)| solve_problem(numbers, *op))
        .sum()
}

fn main() {
    let input = read_input();
    let lines: Vec<String> = input.trim().split('\n').map(String::from).collect();

    println!("Part 1: {}", part1(&lines));
    println!("Part 2: {}", part2(&lines));
}
