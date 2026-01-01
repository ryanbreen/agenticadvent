// Day 18: Lavaduct Lagoon - Polygon area with Shoelace formula and Pick's theorem

use std::fs;
use std::path::Path;

/// Parse a single instruction line into (direction, distance, hex_color)
fn parse_line(line: &str) -> (char, i64, &str) {
    let parts: Vec<&str> = line.split_whitespace().collect();
    let direction = parts[0].chars().next().unwrap();
    let distance = parts[1].parse::<i64>().unwrap();
    // Extract hex color: "(#70c710)" -> "70c710"
    let color = &parts[2][2..parts[2].len() - 1];
    (direction, distance, color)
}

/// Calculate total area using Shoelace formula and Pick's theorem.
/// Shoelace gives twice the signed area of the polygon.
/// Pick's theorem: A = i + b/2 - 1, where i = interior points, b = boundary points
/// We want: Total = i + b = A + b/2 + 1
fn calculate_area(vertices: &[(i64, i64)], perimeter: i64) -> i64 {
    let n = vertices.len();
    let mut area: i64 = 0;

    for i in 0..n {
        let j = (i + 1) % n;
        area += vertices[i].0 * vertices[j].1;
        area -= vertices[j].0 * vertices[i].1;
    }
    area = area.abs() / 2;

    // Total points = interior + boundary = area + boundary/2 + 1
    area + perimeter / 2 + 1
}

/// Part 1: Follow the dig plan directions
fn part1(lines: &[&str]) -> i64 {
    let mut vertices = vec![(0i64, 0i64)];
    let mut perimeter = 0i64;
    let mut r = 0i64;
    let mut c = 0i64;

    for line in lines {
        let (direction, distance, _) = parse_line(line);
        let (dr, dc) = match direction {
            'R' => (0, 1),
            'D' => (1, 0),
            'L' => (0, -1),
            'U' => (-1, 0),
            _ => panic!("Unknown direction: {}", direction),
        };

        r += dr * distance;
        c += dc * distance;
        vertices.push((r, c));
        perimeter += distance;
    }

    calculate_area(&vertices, perimeter)
}

/// Part 2: Decode instructions from hex color codes
/// Last digit of hex: 0=R, 1=D, 2=L, 3=U
/// First 5 digits: distance in hex
fn part2(lines: &[&str]) -> i64 {
    let mut vertices = vec![(0i64, 0i64)];
    let mut perimeter = 0i64;
    let mut r = 0i64;
    let mut c = 0i64;

    for line in lines {
        let (_, _, color) = parse_line(line);

        // First 5 hex digits are the distance
        let distance = i64::from_str_radix(&color[..5], 16).unwrap();
        // Last digit is the direction
        let dir_code = color.chars().nth(5).unwrap();

        let (dr, dc) = match dir_code {
            '0' => (0, 1),   // R
            '1' => (1, 0),   // D
            '2' => (0, -1),  // L
            '3' => (-1, 0),  // U
            _ => panic!("Unknown direction code: {}", dir_code),
        };

        r += dr * distance;
        c += dc * distance;
        vertices.push((r, c));
        perimeter += distance;
    }

    calculate_area(&vertices, perimeter)
}

fn main() {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let content = fs::read_to_string(&input_path)
        .unwrap_or_else(|_| fs::read_to_string("../input.txt").expect("Failed to read input.txt"));

    let lines: Vec<&str> = content.trim().lines().collect();

    println!("Part 1: {}", part1(&lines));
    println!("Part 2: {}", part2(&lines));
}
