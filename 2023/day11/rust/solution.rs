use std::collections::HashSet;
use std::fs;

fn parse_grid(lines: &[&str]) -> Vec<(usize, usize)> {
    let mut galaxies = Vec::new();
    for (r, line) in lines.iter().enumerate() {
        for (c, ch) in line.chars().enumerate() {
            if ch == '#' {
                galaxies.push((r, c));
            }
        }
    }
    galaxies
}

fn find_empty_rows_and_cols(lines: &[&str]) -> (HashSet<usize>, HashSet<usize>) {
    let rows = lines.len();
    let cols = if rows > 0 { lines[0].len() } else { 0 };

    let mut empty_rows = HashSet::new();
    let mut empty_cols = HashSet::new();

    // Find empty rows
    for (r, line) in lines.iter().enumerate() {
        if !line.contains('#') {
            empty_rows.insert(r);
        }
    }

    // Find empty columns
    for c in 0..cols {
        let is_empty = (0..rows).all(|r| {
            lines[r].chars().nth(c).map_or(true, |ch| ch != '#')
        });
        if is_empty {
            empty_cols.insert(c);
        }
    }

    (empty_rows, empty_cols)
}

fn calculate_distances(
    galaxies: &[(usize, usize)],
    empty_rows: &HashSet<usize>,
    empty_cols: &HashSet<usize>,
    expansion_factor: u64,
) -> u64 {
    let mut total: u64 = 0;

    for i in 0..galaxies.len() {
        for j in (i + 1)..galaxies.len() {
            let (r1, c1) = galaxies[i];
            let (r2, c2) = galaxies[j];

            // Calculate row distance with expansion
            let (min_r, max_r) = if r1 < r2 { (r1, r2) } else { (r2, r1) };
            let mut row_dist: u64 = (max_r - min_r) as u64;
            for r in min_r..max_r {
                if empty_rows.contains(&r) {
                    row_dist += expansion_factor - 1;
                }
            }

            // Calculate column distance with expansion
            let (min_c, max_c) = if c1 < c2 { (c1, c2) } else { (c2, c1) };
            let mut col_dist: u64 = (max_c - min_c) as u64;
            for c in min_c..max_c {
                if empty_cols.contains(&c) {
                    col_dist += expansion_factor - 1;
                }
            }

            total += row_dist + col_dist;
        }
    }

    total
}

fn part1(lines: &[&str]) -> u64 {
    let galaxies = parse_grid(lines);
    let (empty_rows, empty_cols) = find_empty_rows_and_cols(lines);
    calculate_distances(&galaxies, &empty_rows, &empty_cols, 2)
}

fn part2(lines: &[&str]) -> u64 {
    let galaxies = parse_grid(lines);
    let (empty_rows, empty_cols) = find_empty_rows_and_cols(lines);
    calculate_distances(&galaxies, &empty_rows, &empty_cols, 1_000_000)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let input_file = if args.len() > 1 {
        &args[1]
    } else {
        "../input.txt"
    };

    let content = fs::read_to_string(input_file).expect("Failed to read input file");
    let lines: Vec<&str> = content.lines().filter(|line| !line.is_empty()).collect();

    println!("Part 1: {}", part1(&lines));
    println!("Part 2: {}", part2(&lines));
}
