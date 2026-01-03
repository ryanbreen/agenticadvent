// Day 23: A Long Walk - Longest path through hiking trails
use std::collections::{HashMap, HashSet};
use std::fs;

type Pos = (i32, i32);

fn parse_input(filename: &str) -> Vec<Vec<char>> {
    fs::read_to_string(filename)
        .expect("Failed to read input file")
        .trim()
        .lines()
        .map(|line| line.chars().collect())
        .collect()
}

fn find_junctions(grid: &[Vec<char>]) -> HashSet<Pos> {
    let rows = grid.len() as i32;
    let cols = grid[0].len() as i32;
    let mut junctions = HashSet::new();

    // Start and end points
    let start_col = grid[0].iter().position(|&c| c == '.').unwrap() as i32;
    let end_col = grid[(rows - 1) as usize]
        .iter()
        .position(|&c| c == '.')
        .unwrap() as i32;
    junctions.insert((0, start_col));
    junctions.insert((rows - 1, end_col));

    // Find intersections (cells with 3+ walkable neighbors)
    for r in 0..rows {
        for c in 0..cols {
            if grid[r as usize][c as usize] == '#' {
                continue;
            }
            let mut neighbors = 0;
            for (dr, dc) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
                let nr = r + dr;
                let nc = c + dc;
                if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
                    if grid[nr as usize][nc as usize] != '#' {
                        neighbors += 1;
                    }
                }
            }
            if neighbors >= 3 {
                junctions.insert((r, c));
            }
        }
    }

    junctions
}

fn get_slope_dir(c: char) -> Option<(i32, i32)> {
    match c {
        '^' => Some((-1, 0)),
        'v' => Some((1, 0)),
        '<' => Some((0, -1)),
        '>' => Some((0, 1)),
        _ => None,
    }
}

fn build_graph(
    grid: &[Vec<char>],
    junctions: &HashSet<Pos>,
    respect_slopes: bool,
) -> HashMap<Pos, HashMap<Pos, i32>> {
    let rows = grid.len() as i32;
    let cols = grid[0].len() as i32;
    let mut graph: HashMap<Pos, HashMap<Pos, i32>> = HashMap::new();

    for &start_junction in junctions {
        // BFS from each junction to find reachable junctions
        let mut stack = vec![(start_junction, 0i32)];
        let mut visited = HashSet::new();
        visited.insert(start_junction);

        while let Some(((r, c), dist)) = stack.pop() {
            if dist > 0 && junctions.contains(&(r, c)) {
                // Found another junction
                graph
                    .entry(start_junction)
                    .or_insert_with(HashMap::new)
                    .insert((r, c), dist);
                continue;
            }

            // Explore neighbors
            for (dr, dc) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
                let nr = r + dr;
                let nc = c + dc;
                if nr < 0 || nr >= rows || nc < 0 || nc >= cols {
                    continue;
                }
                if grid[nr as usize][nc as usize] == '#' {
                    continue;
                }
                if visited.contains(&(nr, nc)) {
                    continue;
                }

                // Check slope constraints for Part 1
                if respect_slopes {
                    let cell = grid[r as usize][c as usize];
                    if let Some((req_dr, req_dc)) = get_slope_dir(cell) {
                        if (dr, dc) != (req_dr, req_dc) {
                            continue;
                        }
                    }
                }

                visited.insert((nr, nc));
                stack.push(((nr, nc), dist + 1));
            }
        }
    }

    graph
}

fn longest_path_dfs(
    graph: &HashMap<Pos, HashMap<Pos, i32>>,
    junctions: &HashSet<Pos>,
    start: Pos,
    end: Pos,
) -> i32 {
    // Create index mapping for bitmask - use all junctions
    let nodes: Vec<Pos> = junctions.iter().cloned().collect();
    let node_to_idx: HashMap<Pos, usize> = nodes.iter().enumerate().map(|(i, &p)| (p, i)).collect();

    // Convert graph to adjacency list with indices
    let mut adj: Vec<Vec<(usize, i32)>> = vec![Vec::new(); nodes.len()];
    for (&from, neighbors) in graph {
        let from_idx = node_to_idx[&from];
        for (&to, &dist) in neighbors {
            if let Some(&to_idx) = node_to_idx.get(&to) {
                adj[from_idx].push((to_idx, dist));
            }
        }
    }

    let start_idx = node_to_idx[&start];
    let end_idx = node_to_idx[&end];

    fn dfs(
        node: usize,
        end: usize,
        visited: u64,
        adj: &[Vec<(usize, i32)>],
    ) -> i32 {
        if node == end {
            return 0;
        }

        let new_visited = visited | (1u64 << node);
        let mut max_dist = i32::MIN;

        for &(neighbor, dist) in &adj[node] {
            if (visited & (1u64 << neighbor)) == 0 {
                let result = dfs(neighbor, end, new_visited, adj);
                if result != i32::MIN {
                    max_dist = max_dist.max(dist + result);
                }
            }
        }

        max_dist
    }

    dfs(start_idx, end_idx, 0, &adj)
}

fn solve(grid: &[Vec<char>], respect_slopes: bool) -> i32 {
    let rows = grid.len() as i32;
    let start_col = grid[0].iter().position(|&c| c == '.').unwrap() as i32;
    let end_col = grid[(rows - 1) as usize]
        .iter()
        .position(|&c| c == '.')
        .unwrap() as i32;
    let start = (0, start_col);
    let end = (rows - 1, end_col);

    let junctions = find_junctions(grid);
    let graph = build_graph(grid, &junctions, respect_slopes);

    longest_path_dfs(&graph, &junctions, start, end)
}

fn part1(grid: &[Vec<char>]) -> i32 {
    solve(grid, true)
}

fn part2(grid: &[Vec<char>]) -> i32 {
    solve(grid, false)
}

fn main() {
    let grid = parse_input("../input.txt");
    println!("Part 1: {}", part1(&grid));
    println!("Part 2: {}", part2(&grid));
}
