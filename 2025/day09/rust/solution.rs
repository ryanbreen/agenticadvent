use std::collections::HashMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let points = parse_input(&input);

    println!("Part 1: {}", part1(&points));
    println!("Part 2: {}", part2(&points));
}

fn parse_input(input: &str) -> Vec<(i32, i32)> {
    input
        .trim()
        .lines()
        .map(|line| {
            let parts: Vec<&str> = line.split(',').collect();
            let x = parts[0].parse::<i32>().unwrap();
            let y = parts[1].parse::<i32>().unwrap();
            (x, y)
        })
        .collect()
}

fn part1(points: &[(i32, i32)]) -> i64 {
    let mut max_area: i64 = 0;
    let n = points.len();

    // Check all pairs of points as opposite corners
    for i in 0..n {
        let (x1, y1) = points[i];
        for j in (i + 1)..n {
            let (x2, y2) = points[j];
            // Rectangle area = width * height (inclusive of both corners)
            let width = ((x2 - x1).abs() + 1) as i64;
            let height = ((y2 - y1).abs() + 1) as i64;
            let area = width * height;
            max_area = max_area.max(area);
        }
    }

    max_area
}

fn part2(points: &[(i32, i32)]) -> i64 {
    let n = points.len();
    let mut horizontal_edges = Vec::new();  // (y, x_min, x_max)
    let mut vertical_edges = Vec::new();    // (x, y_min, y_max)

    // Build edges from consecutive points (forming a polygon)
    for i in 0..n {
        let (x1, y1) = points[i];
        let (x2, y2) = points[(i + 1) % n];

        if y1 == y2 {
            // Horizontal edge
            horizontal_edges.push((y1, x1.min(x2), x1.max(x2)));
        } else {
            // Vertical edge
            vertical_edges.push((x1, y1.min(y2), y1.max(y2)));
        }
    }

    // Build lookup maps for efficient edge queries
    let mut vert_by_x: HashMap<i32, Vec<(i32, i32)>> = HashMap::new();
    for &(x, y_min, y_max) in &vertical_edges {
        vert_by_x.entry(x).or_insert_with(Vec::new).push((y_min, y_max));
    }

    let mut horiz_by_y: HashMap<i32, Vec<(i32, i32)>> = HashMap::new();
    for &(y, x_min, x_max) in &horizontal_edges {
        horiz_by_y.entry(y).or_insert_with(Vec::new).push((x_min, x_max));
    }

    // Helper function to check if a point is inside the polygon using ray casting
    let is_inside_polygon = |x: f64, y: f64| -> bool {
        let mut crossings = 0.0;

        // Get sorted list of vertical edge x-coordinates
        let mut vx_coords: Vec<i32> = vert_by_x.keys().copied().collect();
        vx_coords.sort();

        // Cast ray to the right
        for vx in vx_coords {
            if (vx as f64) <= x {
                continue;
            }
            if let Some(edges) = vert_by_x.get(&vx) {
                for &(y_min, y_max) in edges {
                    if (y_min as f64) < y && y < (y_max as f64) {
                        // Ray crosses edge
                        crossings += 1.0;
                    } else if y == (y_min as f64) || y == (y_max as f64) {
                        // On corner - count as 0.5 crossing
                        crossings += 0.5;
                    }
                }
            }
        }

        crossings % 2.0 == 1.0
    };

    // Helper function to check if rectangle is entirely inside polygon
    let rectangle_valid = |x1: i32, y1: i32, x2: i32, y2: i32| -> bool {
        let min_x = x1.min(x2);
        let max_x = x1.max(x2);
        let min_y = y1.min(y2);
        let max_y = y1.max(y2);

        // Check if any vertical edge crosses through the rectangle interior
        for (&vx, edges) in &vert_by_x {
            if min_x < vx && vx < max_x {
                for &(y_min, y_max) in edges {
                    // Check if this edge segment overlaps with rectangle's y range
                    if !(y_max <= min_y || y_min >= max_y) {
                        return false;
                    }
                }
            }
        }

        // Check if any horizontal edge crosses through the rectangle interior
        for (&hy, edges) in &horiz_by_y {
            if min_y < hy && hy < max_y {
                for &(x_min, x_max) in edges {
                    // Check if this edge segment overlaps with rectangle's x range
                    if !(x_max <= min_x || x_min >= max_x) {
                        return false;
                    }
                }
            }
        }

        // Finally, check that we're inside the polygon (not outside)
        // Check center point
        let center_x = (min_x + max_x) as f64 / 2.0;
        let center_y = (min_y + max_y) as f64 / 2.0;
        is_inside_polygon(center_x, center_y)
    };

    // Find largest valid rectangle with red corners
    let mut max_area: i64 = 0;

    for i in 0..n {
        let (x1, y1) = points[i];
        for j in (i + 1)..n {
            let (x2, y2) = points[j];

            if rectangle_valid(x1, y1, x2, y2) {
                let width = ((x2 - x1).abs() + 1) as i64;
                let height = ((y2 - y1).abs() + 1) as i64;
                let area = width * height;
                max_area = max_area.max(area);
            }
        }
    }

    max_area
}
