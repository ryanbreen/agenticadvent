use std::fs;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Fraction {
    num: i64,
    den: i64,
}

impl Fraction {
    fn new(num: i64, den: i64) -> Self {
        if den == 0 {
            panic!("Denominator cannot be zero");
        }
        let g = gcd(num.abs(), den.abs());
        let num = num / g;
        let den = den / g;
        if den < 0 {
            Fraction { num: -num, den: -den }
        } else {
            Fraction { num, den }
        }
    }

    fn zero() -> Self {
        Fraction { num: 0, den: 1 }
    }

    fn from_int(n: i64) -> Self {
        Fraction { num: n, den: 1 }
    }

    fn is_integer(&self) -> bool {
        self.den == 1
    }

    fn to_int(&self) -> i64 {
        self.num / self.den
    }

    fn add(&self, other: &Fraction) -> Fraction {
        Fraction::new(
            self.num * other.den + other.num * self.den,
            self.den * other.den,
        )
    }

    fn sub(&self, other: &Fraction) -> Fraction {
        Fraction::new(
            self.num * other.den - other.num * self.den,
            self.den * other.den,
        )
    }

    fn mul(&self, other: &Fraction) -> Fraction {
        Fraction::new(self.num * other.num, self.den * other.den)
    }

    fn div(&self, other: &Fraction) -> Fraction {
        if other.num == 0 {
            panic!("Division by zero");
        }
        Fraction::new(self.num * other.den, self.den * other.num)
    }

    fn neg(&self) -> Fraction {
        Fraction { num: -self.num, den: self.den }
    }

    fn to_f64(&self) -> f64 {
        self.num as f64 / self.den as f64
    }
}

fn gcd(a: i64, b: i64) -> i64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn parse_line(line: &str) -> (usize, u64, Vec<u64>) {
    // Parse indicator pattern [.##.]
    let indicator_start = line.find('[').unwrap() + 1;
    let indicator_end = line.find(']').unwrap();
    let indicator = &line[indicator_start..indicator_end];
    let n_lights = indicator.len();

    // Build target state
    let mut target = 0u64;
    for (i, c) in indicator.chars().enumerate() {
        if c == '#' {
            target |= 1u64 << i;
        }
    }

    // Extract button schematics
    let mut buttons = Vec::new();
    let mut pos = indicator_end;
    while let Some(start) = line[pos..].find('(') {
        let abs_start = pos + start + 1;
        if let Some(end) = line[abs_start..].find(')') {
            let button_str = &line[abs_start..abs_start + end];
            let mut mask = 0u64;
            for num_str in button_str.split(',') {
                if let Ok(idx) = num_str.parse::<usize>() {
                    mask |= 1u64 << idx;
                }
            }
            buttons.push(mask);
            pos = abs_start + end;
        } else {
            break;
        }
    }

    (n_lights, target, buttons)
}

fn parse_line_part2(line: &str) -> (usize, Vec<i64>, Vec<Vec<usize>>) {
    // Extract joltage requirements {3,5,4,7}
    let joltage_start = line.find('{').unwrap() + 1;
    let joltage_end = line.find('}').unwrap();
    let joltage_str = &line[joltage_start..joltage_end];
    let joltage: Vec<i64> = joltage_str
        .split(',')
        .map(|s| s.parse::<i64>().unwrap())
        .collect();
    let n_counters = joltage.len();

    // Extract button schematics
    let mut buttons = Vec::new();
    let mut pos = 0;
    while let Some(start) = line[pos..].find('(') {
        let abs_start = pos + start + 1;
        if let Some(end) = line[abs_start..].find(')') {
            let button_str = &line[abs_start..abs_start + end];
            let indices: Vec<usize> = button_str
                .split(',')
                .filter_map(|s| s.parse::<usize>().ok())
                .collect();
            buttons.push(indices);
            pos = abs_start + end;
        } else {
            break;
        }
    }

    (n_counters, joltage, buttons)
}

fn solve_machine_brute(_n_lights: usize, target: u64, buttons: &[u64]) -> usize {
    let n_buttons = buttons.len();
    let mut min_presses = usize::MAX;

    // Try all 2^n combinations
    for mask in 0..(1u64 << n_buttons) {
        let mut state = 0u64;
        let mut presses = 0;
        for i in 0..n_buttons {
            if mask & (1u64 << i) != 0 {
                state ^= buttons[i];
                presses += 1;
            }
        }

        if state == target {
            min_presses = min_presses.min(presses);
        }
    }

    if min_presses == usize::MAX {
        0
    } else {
        min_presses
    }
}

fn solve_machine(n_lights: usize, target: u64, buttons: &[u64]) -> usize {
    if buttons.len() <= 20 {
        solve_machine_brute(n_lights, target, buttons)
    } else {
        // For larger problems, would need Gaussian elimination over GF(2)
        // For now, fallback to brute force or return 0
        0
    }
}

fn part1(lines: &[String]) -> usize {
    let mut total = 0;
    for line in lines {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let (n_lights, target, buttons) = parse_line(line);
        let min_presses = solve_machine(n_lights, target, &buttons);
        total += min_presses;
    }
    total
}

fn solve_machine_part2(n_counters: usize, joltage: &[i64], buttons: &[Vec<usize>]) -> usize {
    let n_buttons = buttons.len();

    if n_buttons == 0 {
        return if joltage.iter().all(|&j| j == 0) { 0 } else { usize::MAX };
    }

    // Build matrix A (n_counters x n_buttons)
    let mut a = vec![vec![Fraction::zero(); n_buttons]; n_counters];
    for (j, indices) in buttons.iter().enumerate() {
        for &idx in indices {
            if idx < n_counters {
                a[idx][j] = Fraction::from_int(1);
            }
        }
    }

    let b: Vec<Fraction> = joltage.iter().map(|&x| Fraction::from_int(x)).collect();

    // Augmented matrix [A | b]
    let mut aug: Vec<Vec<Fraction>> = a
        .iter()
        .enumerate()
        .map(|(i, row)| {
            let mut r = row.clone();
            r.push(b[i]);
            r
        })
        .collect();

    let n_rows = n_counters;
    let n_cols = n_buttons;

    // Gaussian elimination with partial pivoting
    let mut pivot_cols = Vec::new();
    let mut pivot_row = 0;

    for col in 0..n_cols {
        // Find non-zero entry in this column
        let mut found = None;
        for row in pivot_row..n_rows {
            if aug[row][col].num != 0 {
                found = Some(row);
                break;
            }
        }

        if found.is_none() {
            continue;
        }

        let found = found.unwrap();

        // Swap rows
        aug.swap(pivot_row, found);
        pivot_cols.push((col, pivot_row));

        // Scale pivot row
        let scale = aug[pivot_row][col];
        for c in 0..=n_cols {
            aug[pivot_row][c] = aug[pivot_row][c].div(&scale);
        }

        // Eliminate column in other rows
        for row in 0..n_rows {
            if row != pivot_row && aug[row][col].num != 0 {
                let factor = aug[row][col];
                for c in 0..=n_cols {
                    let temp = aug[pivot_row][c].mul(&factor);
                    aug[row][c] = aug[row][c].sub(&temp);
                }
            }
        }

        pivot_row += 1;
    }

    // Check for inconsistency
    for row in pivot_row..n_rows {
        if aug[row][n_cols].num != 0 {
            return usize::MAX; // No solution
        }
    }

    // Identify free variables
    let pivot_col_set: std::collections::HashSet<usize> =
        pivot_cols.iter().map(|&(col, _)| col).collect();
    let free_vars: Vec<usize> = (0..n_cols).filter(|c| !pivot_col_set.contains(c)).collect();

    // If no free variables, unique solution
    if free_vars.is_empty() {
        let mut solution = vec![Fraction::zero(); n_buttons];
        for &(col, row) in &pivot_cols {
            solution[col] = aug[row][n_cols];
        }

        let mut total = 0;
        for val in &solution {
            if val.num < 0 || !val.is_integer() {
                return usize::MAX;
            }
            total += val.to_int() as usize;
        }
        return total;
    }

    // Extract null space vectors
    let mut null_vectors = Vec::new();
    for &fv in &free_vars {
        let mut vec = vec![Fraction::zero(); n_buttons];
        vec[fv] = Fraction::from_int(1);
        for &(col, row) in &pivot_cols {
            vec[col] = aug[row][fv].neg();
        }
        null_vectors.push(vec);
    }

    // Extract particular solution
    let mut particular = vec![Fraction::zero(); n_buttons];
    for &(col, row) in &pivot_cols {
        particular[col] = aug[row][n_cols];
    }

    let n_free = free_vars.len();
    let max_j = joltage.iter().max().copied().unwrap_or(100);

    // Search for optimal non-negative integer solution
    if n_free == 1 {
        // 1D search
        let mut t_low = f64::NEG_INFINITY;
        let mut t_high = f64::INFINITY;

        for j in 0..n_buttons {
            let p = particular[j].to_f64();
            let nv = null_vectors[0][j].to_f64();

            if nv == 0.0 {
                if p < 0.0 {
                    return usize::MAX;
                }
            } else if nv > 0.0 {
                t_low = t_low.max(-p / nv);
            } else {
                t_high = t_high.min(-p / nv);
            }
        }

        if t_low > t_high {
            return usize::MAX;
        }

        let t_low_int = t_low.ceil() as i64;
        let t_high_int = t_high.floor() as i64;

        let mut min_total = usize::MAX;
        for t in t_low_int..=t_high_int {
            let t_frac = Fraction::from_int(t);
            let mut total = 0;
            let mut valid = true;
            for j in 0..n_buttons {
                let val = particular[j].add(&t_frac.mul(&null_vectors[0][j]));
                if val.num < 0 || !val.is_integer() {
                    valid = false;
                    break;
                }
                total += val.to_int() as usize;
            }
            if valid {
                min_total = min_total.min(total);
            }
        }

        return if min_total == usize::MAX { 0 } else { min_total };
    } else if n_free == 2 {
        // 2D search
        let bound = max_j * 2;
        let mut min_total = usize::MAX;

        for t0 in -bound..=bound {
            let t0_frac = Fraction::from_int(t0);
            let intermediate: Vec<Fraction> = (0..n_buttons)
                .map(|j| particular[j].add(&t0_frac.mul(&null_vectors[0][j])))
                .collect();

            // Compute bounds for t1
            let mut t1_low = f64::NEG_INFINITY;
            let mut t1_high = f64::INFINITY;
            for j in 0..n_buttons {
                let p = intermediate[j].to_f64();
                let nv = null_vectors[1][j].to_f64();
                if nv > 0.0 {
                    t1_low = t1_low.max(-p / nv);
                } else if nv < 0.0 {
                    t1_high = t1_high.min(-p / nv);
                }
            }

            let t1_low_int = t1_low.ceil() as i64;
            let t1_high_int = t1_high.floor() as i64;

            for t1 in t1_low_int..=t1_high_int {
                let t1_frac = Fraction::from_int(t1);
                let mut valid = true;
                let mut total = 0;
                for j in 0..n_buttons {
                    let val = intermediate[j].add(&t1_frac.mul(&null_vectors[1][j]));
                    if val.num < 0 || !val.is_integer() {
                        valid = false;
                        break;
                    }
                    total += val.to_int() as usize;
                }
                if valid {
                    min_total = min_total.min(total);
                }
            }
        }

        return if min_total == usize::MAX { 0 } else { min_total };
    } else if n_free == 3 {
        // 3D search
        let bound = max_j;
        let mut min_total = usize::MAX;

        for t0 in -bound..=bound {
            let t0_frac = Fraction::from_int(t0);
            let inter0: Vec<Fraction> = (0..n_buttons)
                .map(|j| particular[j].add(&t0_frac.mul(&null_vectors[0][j])))
                .collect();

            for t1 in -bound..=bound {
                let t1_frac = Fraction::from_int(t1);
                let inter1: Vec<Fraction> = (0..n_buttons)
                    .map(|j| inter0[j].add(&t1_frac.mul(&null_vectors[1][j])))
                    .collect();

                // Compute bounds for t2
                let mut t2_low = f64::NEG_INFINITY;
                let mut t2_high = f64::INFINITY;
                for j in 0..n_buttons {
                    let p = inter1[j].to_f64();
                    let nv = null_vectors[2][j].to_f64();
                    if nv > 0.0 {
                        t2_low = t2_low.max(-p / nv);
                    } else if nv < 0.0 {
                        t2_high = t2_high.min(-p / nv);
                    }
                }

                let t2_low_int = t2_low.ceil() as i64;
                let t2_high_int = t2_high.floor() as i64;

                for t2 in t2_low_int..=t2_high_int {
                    let t2_frac = Fraction::from_int(t2);
                    let mut valid = true;
                    let mut total = 0;
                    for j in 0..n_buttons {
                        let val = inter1[j].add(&t2_frac.mul(&null_vectors[2][j]));
                        if val.num < 0 || !val.is_integer() {
                            valid = false;
                            break;
                        }
                        total += val.to_int() as usize;
                    }
                    if valid {
                        min_total = min_total.min(total);
                    }
                }
            }
        }

        return if min_total == usize::MAX { 0 } else { min_total };
    }

    // For more free variables, use recursive search
    fn search_recursive(
        idx: usize,
        n_free: usize,
        n_buttons: usize,
        partial: &[Fraction],
        null_vectors: &[Vec<Fraction>],
        max_j: i64,
        min_total: &mut usize,
    ) {
        if idx == n_free {
            let mut valid = true;
            let mut total = 0;
            for val in partial {
                if val.num < 0 || !val.is_integer() {
                    valid = false;
                    break;
                }
                total += val.to_int() as usize;
            }
            if valid {
                *min_total = (*min_total).min(total);
            }
            return;
        }

        // Compute bounds for current free var
        let mut t_low = f64::NEG_INFINITY;
        let mut t_high = f64::INFINITY;
        for j in 0..n_buttons {
            let p = partial[j].to_f64();
            let nv = null_vectors[idx][j].to_f64();
            if nv > 0.0 {
                t_low = t_low.max(-p / nv);
            } else if nv < 0.0 {
                t_high = t_high.min(-p / nv);
            }
        }

        if t_low > t_high {
            return;
        }

        let t_low_int = (t_low.ceil() as i64 - max_j).max(-max_j * 2);
        let t_high_int = (t_high.floor() as i64 + max_j).min(max_j * 2);

        for t in t_low_int..=t_high_int {
            let t_frac = Fraction::from_int(t);
            let new_partial: Vec<Fraction> = (0..n_buttons)
                .map(|j| partial[j].add(&t_frac.mul(&null_vectors[idx][j])))
                .collect();
            search_recursive(idx + 1, n_free, n_buttons, &new_partial, null_vectors, max_j, min_total);
        }
    }

    let mut min_total = usize::MAX;
    search_recursive(0, n_free, n_buttons, &particular, &null_vectors, max_j, &mut min_total);

    if min_total == usize::MAX {
        0
    } else {
        min_total
    }
}

fn part2(lines: &[String]) -> usize {
    let mut total = 0;
    for line in lines {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let (n_counters, joltage, buttons) = parse_line_part2(line);
        let min_presses = solve_machine_part2(n_counters, &joltage, &buttons);
        total += min_presses;
    }
    total
}

fn main() {
    let input_path = "../input.txt";
    let contents = fs::read_to_string(input_path).expect("Failed to read input file");
    let lines: Vec<String> = contents.lines().map(|s| s.to_string()).collect();

    println!("Part 1: {}", part1(&lines));
    println!("Part 2: {}", part2(&lines));
}
