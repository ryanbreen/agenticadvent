use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

type Pos = (i32, i32);

fn parse_input(text: &str) -> HashSet<Pos> {
    let mut elves = HashSet::new();
    for (r, line) in text.trim().lines().enumerate() {
        for (c, ch) in line.chars().enumerate() {
            if ch == '#' {
                elves.insert((r as i32, c as i32));
            }
        }
    }
    elves
}

fn get_dir_check(d: char) -> ([(i32, i32); 3], (i32, i32)) {
    match d {
        'N' => ([(-1, -1), (-1, 0), (-1, 1)], (-1, 0)),
        'S' => ([(1, -1), (1, 0), (1, 1)], (1, 0)),
        'W' => ([(-1, -1), (0, -1), (1, -1)], (0, -1)),
        'E' => ([(-1, 1), (0, 1), (1, 1)], (0, 1)),
        _ => panic!("Invalid direction"),
    }
}

fn simulate_round(elves: &HashSet<Pos>, directions: &[char]) -> (HashSet<Pos>, bool) {

    // All 8 neighbors
    let all_neighbors: [(i32, i32); 8] = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];

    // Phase 1: Each elf proposes a move
    let mut proposals: HashMap<Pos, Pos> = HashMap::new();
    let mut proposal_counts: HashMap<Pos, i32> = HashMap::new();

    for &(r, c) in elves.iter() {
        // Check if any neighbors
        let has_neighbor = all_neighbors
            .iter()
            .any(|&(dr, dc)| elves.contains(&(r + dr, c + dc)));

        if !has_neighbor {
            continue; // Don't move
        }

        // Try each direction
        for &d in directions.iter() {
            let (checks, (dr, dc)) = get_dir_check(d);
            if checks
                .iter()
                .all(|&(cr, cc)| !elves.contains(&(r + cr, c + cc)))
            {
                let new_pos = (r + dr, c + dc);
                proposals.insert((r, c), new_pos);
                *proposal_counts.entry(new_pos).or_insert(0) += 1;
                break;
            }
        }
    }

    // Phase 2: Execute moves (only if unique proposal)
    let mut new_elves = HashSet::new();
    let mut moved = false;

    for &elf in elves.iter() {
        if let Some(&new_pos) = proposals.get(&elf) {
            if proposal_counts[&new_pos] == 1 {
                new_elves.insert(new_pos);
                moved = true;
            } else {
                new_elves.insert(elf);
            }
        } else {
            new_elves.insert(elf);
        }
    }

    (new_elves, moved)
}

fn bounding_rect_empty(elves: &HashSet<Pos>) -> i32 {
    let min_r = elves.iter().map(|&(r, _)| r).min().unwrap();
    let max_r = elves.iter().map(|&(r, _)| r).max().unwrap();
    let min_c = elves.iter().map(|&(_, c)| c).min().unwrap();
    let max_c = elves.iter().map(|&(_, c)| c).max().unwrap();

    let area = (max_r - min_r + 1) * (max_c - min_c + 1);
    area - elves.len() as i32
}

fn part1(text: &str) -> i32 {
    let mut elves = parse_input(text);
    let mut directions = vec!['N', 'S', 'W', 'E'];

    for _ in 0..10 {
        let (new_elves, _) = simulate_round(&elves, &directions);
        elves = new_elves;
        // Rotate directions
        let first = directions.remove(0);
        directions.push(first);
    }

    bounding_rect_empty(&elves)
}

fn part2(text: &str) -> i32 {
    let mut elves = parse_input(text);
    let mut directions = vec!['N', 'S', 'W', 'E'];
    let mut round_num = 0;

    loop {
        round_num += 1;
        let (new_elves, moved) = simulate_round(&elves, &directions);
        elves = new_elves;

        if !moved {
            return round_num;
        }

        // Rotate directions
        let first = directions.remove(0);
        directions.push(first);
    }
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let dir = exe_path.parent().unwrap();
    let input_path = dir.join("../input.txt");

    // Try multiple input paths
    let text = if Path::new(&input_path).exists() {
        fs::read_to_string(&input_path).expect("Failed to read input")
    } else {
        // Try from current directory
        let alt_path = Path::new("../input.txt");
        if alt_path.exists() {
            fs::read_to_string(alt_path).expect("Failed to read input")
        } else {
            // Try absolute path based on source location
            fs::read_to_string("/Users/wrb/fun/code/agenticadvent/2022/day23/input.txt")
                .expect("Failed to read input")
        }
    };

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));
}
