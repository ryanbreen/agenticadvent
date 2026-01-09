use std::collections::HashMap;
use std::fs;
use std::path::Path;

// Rock shapes as list of (dx, dy) offsets from bottom-left
const ROCKS: [&[(i64, i64)]; 5] = [
    &[(0, 0), (1, 0), (2, 0), (3, 0)],           // Horizontal line
    &[(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],   // Plus
    &[(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],   // L shape
    &[(0, 0), (0, 1), (0, 2), (0, 3)],           // Vertical line
    &[(0, 0), (1, 0), (0, 1), (1, 1)],           // Square
];

const WIDTH: i64 = 7;

fn simulate(jets: &[u8], num_rocks: u64) -> u64 {
    let mut occupied = std::collections::HashSet::new();
    let mut height: i64 = 0;
    let mut jet_idx: usize = 0;

    // For cycle detection
    let mut states: HashMap<(usize, usize, Vec<(i64, i64)>), (u64, i64)> = HashMap::new();
    let mut heights: Vec<i64> = Vec::new();

    for rock_num in 0..num_rocks {
        let rock_type = (rock_num % 5) as usize;
        let rock = ROCKS[rock_type];

        // Starting position: left edge at x=2, bottom at y=height+3
        let mut x: i64 = 2;
        let mut y: i64 = height + 3;

        loop {
            // Jet push
            let jet = jets[jet_idx];
            jet_idx = (jet_idx + 1) % jets.len();

            let dx: i64 = if jet == b'>' { 1 } else { -1 };

            // Check if can move horizontally
            let can_move = rock.iter().all(|&(rx, ry)| {
                let nx = x + rx + dx;
                let ny = y + ry;
                nx >= 0 && nx < WIDTH && !occupied.contains(&(nx, ny))
            });

            if can_move {
                x += dx;
            }

            // Fall down
            let can_fall = rock.iter().all(|&(rx, ry)| {
                let nx = x + rx;
                let ny = y + ry - 1;
                ny >= 0 && !occupied.contains(&(nx, ny))
            });

            if can_fall {
                y -= 1;
            } else {
                // Rock stops
                for &(rx, ry) in rock {
                    occupied.insert((x + rx, y + ry));
                    height = height.max(y + ry + 1);
                }
                break;
            }
        }

        heights.push(height);

        // Cycle detection for Part 2
        if num_rocks > 10000 {
            // Create state key from surface profile
            let profile_depth = 30;
            let mut profile: Vec<(i64, i64)> = Vec::new();
            for col in 0..WIDTH {
                let mut found = false;
                for row in 0..profile_depth {
                    if occupied.contains(&(col, height - 1 - row)) {
                        profile.push((col, row));
                        found = true;
                        break;
                    }
                }
                if !found {
                    profile.push((col, profile_depth));
                }
            }

            let state = (rock_type, jet_idx, profile);

            if let Some(&(cycle_start, start_height)) = states.get(&state) {
                // Found cycle
                let cycle_len = rock_num - cycle_start;
                let cycle_height = height - start_height;

                // Calculate final height
                let remaining = num_rocks - rock_num - 1;
                let full_cycles = remaining / cycle_len;
                let leftover = remaining % cycle_len;

                let mut final_height = height as u64 + full_cycles * cycle_height as u64;
                if leftover > 0 {
                    let idx = (cycle_start + leftover) as usize;
                    final_height += (heights[idx] - start_height) as u64;
                }

                return final_height;
            }

            states.insert(state, (rock_num, height));
        }
    }

    height as u64
}

fn part1(jets: &[u8]) -> u64 {
    simulate(jets, 2022)
}

fn part2(jets: &[u8]) -> u64 {
    simulate(jets, 1_000_000_000_000)
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let dir = exe_path.parent().unwrap();
    let input_path = dir.join("../input.txt");

    let input = fs::read_to_string(&input_path)
        .or_else(|_| fs::read_to_string(Path::new("../input.txt")))
        .expect("Failed to read input.txt");

    let jets: Vec<u8> = input.trim().bytes().collect();

    println!("Part 1: {}", part1(&jets));
    println!("Part 2: {}", part2(&jets));
}
