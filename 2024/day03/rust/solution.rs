use std::fs;

fn part1(data: &str) -> i32 {
    let mut total = 0;
    let bytes = data.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        // Look for "mul("
        if i + 4 < bytes.len()
            && bytes[i] == b'm'
            && bytes[i+1] == b'u'
            && bytes[i+2] == b'l'
            && bytes[i+3] == b'(' {

            i += 4; // Move past "mul("

            // Parse first number (1-3 digits)
            let mut x = 0;
            let mut digit_count = 0;
            while i < bytes.len() && bytes[i].is_ascii_digit() && digit_count < 3 {
                x = x * 10 + (bytes[i] - b'0') as i32;
                i += 1;
                digit_count += 1;
            }

            // Must have at least 1 digit
            if digit_count == 0 {
                continue;
            }

            // Must have comma
            if i >= bytes.len() || bytes[i] != b',' {
                continue;
            }
            i += 1; // Move past comma

            // Parse second number (1-3 digits)
            let mut y = 0;
            digit_count = 0;
            while i < bytes.len() && bytes[i].is_ascii_digit() && digit_count < 3 {
                y = y * 10 + (bytes[i] - b'0') as i32;
                i += 1;
                digit_count += 1;
            }

            // Must have at least 1 digit
            if digit_count == 0 {
                continue;
            }

            // Must have closing paren
            if i < bytes.len() && bytes[i] == b')' {
                total += x * y;
                i += 1; // Move past closing paren
            }
        } else {
            i += 1;
        }
    }

    total
}

fn part2(data: &str) -> i32 {
    let bytes = data.as_bytes();
    let mut i = 0;
    let mut total = 0;
    let mut enabled = true;

    while i < bytes.len() {
        // Check for "do()"
        if i + 4 <= bytes.len()
            && bytes[i] == b'd'
            && bytes[i+1] == b'o'
            && bytes[i+2] == b'('
            && bytes[i+3] == b')' {
            enabled = true;
            i += 4;
            continue;
        }

        // Check for "don't()"
        if i + 7 <= bytes.len()
            && bytes[i] == b'd'
            && bytes[i+1] == b'o'
            && bytes[i+2] == b'n'
            && bytes[i+3] == b'\''
            && bytes[i+4] == b't'
            && bytes[i+5] == b'('
            && bytes[i+6] == b')' {
            enabled = false;
            i += 7;
            continue;
        }

        // Check for "mul("
        if i + 4 < bytes.len()
            && bytes[i] == b'm'
            && bytes[i+1] == b'u'
            && bytes[i+2] == b'l'
            && bytes[i+3] == b'(' {

            let start = i;
            i += 4; // Move past "mul("

            // Parse first number (1-3 digits)
            let mut x = 0;
            let mut digit_count = 0;
            while i < bytes.len() && bytes[i].is_ascii_digit() && digit_count < 3 {
                x = x * 10 + (bytes[i] - b'0') as i32;
                i += 1;
                digit_count += 1;
            }

            // Must have at least 1 digit
            if digit_count == 0 {
                i = start + 1;
                continue;
            }

            // Must have comma
            if i >= bytes.len() || bytes[i] != b',' {
                i = start + 1;
                continue;
            }
            i += 1; // Move past comma

            // Parse second number (1-3 digits)
            let mut y = 0;
            digit_count = 0;
            while i < bytes.len() && bytes[i].is_ascii_digit() && digit_count < 3 {
                y = y * 10 + (bytes[i] - b'0') as i32;
                i += 1;
                digit_count += 1;
            }

            // Must have at least 1 digit
            if digit_count == 0 {
                i = start + 1;
                continue;
            }

            // Must have closing paren
            if i < bytes.len() && bytes[i] == b')' {
                if enabled {
                    total += x * y;
                }
                i += 1; // Move past closing paren
            } else {
                i = start + 1;
            }
        } else {
            i += 1;
        }
    }

    total
}

fn main() {
    let data = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    println!("Part 1: {}", part1(&data));
    println!("Part 2: {}", part2(&data));
}
