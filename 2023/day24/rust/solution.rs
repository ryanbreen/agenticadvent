use std::fs;

#[derive(Clone, Copy, Debug)]
struct Hailstone {
    px: i64,
    py: i64,
    pz: i64,
    vx: i64,
    vy: i64,
    vz: i64,
}

fn parse_input(filename: &str) -> Vec<Hailstone> {
    let content = fs::read_to_string(filename).expect("Failed to read input file");
    content
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let parts: Vec<&str> = line.split('@').collect();
            let pos: Vec<i64> = parts[0]
                .split(',')
                .map(|s| s.trim().parse().unwrap())
                .collect();
            let vel: Vec<i64> = parts[1]
                .split(',')
                .map(|s| s.trim().parse().unwrap())
                .collect();
            Hailstone {
                px: pos[0],
                py: pos[1],
                pz: pos[2],
                vx: vel[0],
                vy: vel[1],
                vz: vel[2],
            }
        })
        .collect()
}

/// Find intersection of two hailstone paths in 2D (XY plane).
/// Returns Some((x, y, t1, t2)) or None if parallel.
fn find_intersection_2d(h1: &Hailstone, h2: &Hailstone) -> Option<(f64, f64, f64, f64)> {
    let vx1 = h1.vx as f64;
    let vy1 = h1.vy as f64;
    let vx2 = h2.vx as f64;
    let vy2 = h2.vy as f64;
    let px1 = h1.px as f64;
    let py1 = h1.py as f64;
    let px2 = h2.px as f64;
    let py2 = h2.py as f64;

    // Solve for t1 and t2 using Cramer's rule:
    // vx1*t1 - vx2*t2 = px2 - px1
    // vy1*t1 - vy2*t2 = py2 - py1
    let det = vx1 * (-vy2) - (-vx2) * vy1;
    if det.abs() < 1e-10 {
        return None; // Parallel lines
    }

    let dx = px2 - px1;
    let dy = py2 - py1;

    let t1 = (dx * (-vy2) - (-vx2) * dy) / det;
    let t2 = (vx1 * dy - dx * vy1) / det;

    let x = px1 + vx1 * t1;
    let y = py1 + vy1 * t1;

    Some((x, y, t1, t2))
}

fn part1(hailstones: &[Hailstone]) -> usize {
    const MIN_COORD: f64 = 200000000000000.0;
    const MAX_COORD: f64 = 400000000000000.0;
    let mut count = 0;

    for i in 0..hailstones.len() {
        for j in (i + 1)..hailstones.len() {
            if let Some((x, y, t1, t2)) = find_intersection_2d(&hailstones[i], &hailstones[j]) {
                // Check if intersection is in the future for both hailstones
                if t1 < 0.0 || t2 < 0.0 {
                    continue;
                }

                // Check if intersection is within test area
                if x >= MIN_COORD && x <= MAX_COORD && y >= MIN_COORD && y <= MAX_COORD {
                    count += 1;
                }
            }
        }
    }

    count
}

/// Solve a system of linear equations using Gaussian elimination with f64.
/// Uses partial pivoting for numerical stability.
fn solve_system(matrix: &[[f64; 4]; 4], rhs: &[f64; 4]) -> [f64; 4] {
    const N: usize = 4;
    // Create augmented matrix
    let mut aug: [[f64; 5]; 4] = [[0.0; 5]; 4];
    for i in 0..N {
        for j in 0..N {
            aug[i][j] = matrix[i][j];
        }
        aug[i][N] = rhs[i];
    }

    // Forward elimination with partial pivoting
    for col in 0..N {
        // Find pivot (row with largest absolute value in this column)
        let mut max_row = col;
        for row in (col + 1)..N {
            if aug[row][col].abs() > aug[max_row][col].abs() {
                max_row = row;
            }
        }
        aug.swap(col, max_row);

        if aug[col][col].abs() < 1e-15 {
            continue;
        }

        // Eliminate column below pivot
        for row in (col + 1)..N {
            let factor = aug[row][col] / aug[col][col];
            for j in col..=N {
                aug[row][j] -= factor * aug[col][j];
            }
        }
    }

    // Back substitution
    let mut solution = [0.0; 4];
    for i in (0..N).rev() {
        solution[i] = aug[i][N];
        for j in (i + 1)..N {
            solution[i] -= aug[i][j] * solution[j];
        }
        solution[i] /= aug[i][i];
    }

    solution
}

fn part2(hailstones: &[Hailstone]) -> i64 {
    // Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
    //
    // The key insight is that for each hailstone i, if the rock hits it at time ti:
    // rx + rvx*ti = pxi + vxi*ti
    // ry + rvy*ti = pyi + vyi*ti
    //
    // Eliminating ti gives:
    // (rx - pxi) * (vyi - rvy) = (ry - pyi) * (vxi - rvx)
    //
    // Expanding: rx*vyi - rx*rvy - pxi*vyi + pxi*rvy = ry*vxi - ry*rvx - pyi*vxi + pyi*rvx
    //
    // The terms rx*rvy and ry*rvx are nonlinear. However, if we take two hailstones i and j
    // and subtract their equations, these nonlinear terms cancel:
    //
    // (vyi - vyj)*rx + (vxj - vxi)*ry + (pyj - pyi)*rvx + (pxi - pxj)*rvy
    //     = pxi*vyi - pyi*vxi - (pxj*vyj - pyj*vxj)
    //
    // This gives us linear equations! We need 4 equations (from 5 hailstones) for 4 unknowns.

    let h = &hailstones[..5];

    let mut matrix_xy: [[f64; 4]; 4] = [[0.0; 4]; 4];
    let mut rhs_xy: [f64; 4] = [0.0; 4];

    for i in 0..4 {
        let px1 = h[i].px as f64;
        let py1 = h[i].py as f64;
        let vx1 = h[i].vx as f64;
        let vy1 = h[i].vy as f64;
        let px2 = h[i + 1].px as f64;
        let py2 = h[i + 1].py as f64;
        let vx2 = h[i + 1].vx as f64;
        let vy2 = h[i + 1].vy as f64;

        // Coefficients for rx, ry, rvx, rvy
        matrix_xy[i] = [vy1 - vy2, vx2 - vx1, py2 - py1, px1 - px2];
        rhs_xy[i] = px1 * vy1 - py1 * vx1 - (px2 * vy2 - py2 * vx2);
    }

    let solution_xy = solve_system(&matrix_xy, &rhs_xy);
    let rx = solution_xy[0];
    let ry = solution_xy[1];

    // Build system for XZ plane to get rz
    let mut matrix_xz: [[f64; 4]; 4] = [[0.0; 4]; 4];
    let mut rhs_xz: [f64; 4] = [0.0; 4];

    for i in 0..4 {
        let px1 = h[i].px as f64;
        let pz1 = h[i].pz as f64;
        let vx1 = h[i].vx as f64;
        let vz1 = h[i].vz as f64;
        let px2 = h[i + 1].px as f64;
        let pz2 = h[i + 1].pz as f64;
        let vx2 = h[i + 1].vx as f64;
        let vz2 = h[i + 1].vz as f64;

        // Same structure as XY but with Z instead of Y
        matrix_xz[i] = [vz1 - vz2, vx2 - vx1, pz2 - pz1, px1 - px2];
        rhs_xz[i] = px1 * vz1 - pz1 * vx1 - (px2 * vz2 - pz2 * vx2);
    }

    let solution_xz = solve_system(&matrix_xz, &rhs_xz);
    let rz = solution_xz[1];

    // Round to nearest integer (the solution should be exact integers)
    (rx.round() + ry.round() + rz.round()) as i64
}

fn main() {
    let input_file = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "../input.txt".to_string());
    let hailstones = parse_input(&input_file);

    println!("Part 1: {}", part1(&hailstones));
    println!("Part 2: {}", part2(&hailstones));
}
