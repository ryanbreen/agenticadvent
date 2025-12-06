use std::fs;
use std::cmp::max;

#[derive(Debug, Default)]
struct CubeSet {
    red: u32,
    green: u32,
    blue: u32,
}

#[derive(Debug)]
struct Game {
    id: u32,
    sets: Vec<CubeSet>,
}

fn parse_game(line: &str) -> Option<Game> {
    // Parse "Game X: ..." format
    let parts: Vec<&str> = line.split(':').collect();
    if parts.len() != 2 {
        return None;
    }

    // Extract game ID
    let id = parts[0].trim_start_matches("Game ").trim().parse::<u32>().ok()?;

    // Parse sets separated by semicolons
    let sets_str = parts[1];
    let mut sets = Vec::new();

    for set_str in sets_str.split(';') {
        let mut cube_set = CubeSet::default();

        // Parse individual cube counts: "3 blue, 4 red"
        for cube_info in set_str.split(',') {
            let parts: Vec<&str> = cube_info.trim().split_whitespace().collect();
            if parts.len() != 2 {
                continue;
            }

            let count = parts[0].parse::<u32>().ok()?;
            let color = parts[1];

            match color {
                "red" => cube_set.red = count,
                "green" => cube_set.green = count,
                "blue" => cube_set.blue = count,
                _ => {}
            }
        }

        sets.push(cube_set);
    }

    Some(Game { id, sets })
}

fn is_game_possible(game: &Game, max_red: u32, max_green: u32, max_blue: u32) -> bool {
    // Check if all sets in the game are possible
    for set in &game.sets {
        if set.red > max_red || set.green > max_green || set.blue > max_blue {
            return false;
        }
    }
    true
}

fn find_minimum_cubes(game: &Game) -> CubeSet {
    // Find the minimum number of cubes needed for this game
    let mut min_set = CubeSet::default();

    for set in &game.sets {
        min_set.red = max(min_set.red, set.red);
        min_set.green = max(min_set.green, set.green);
        min_set.blue = max(min_set.blue, set.blue);
    }

    min_set
}

fn calculate_power(cube_set: &CubeSet) -> u32 {
    cube_set.red * cube_set.green * cube_set.blue
}

fn part1(games: &[Game]) -> u32 {
    let max_red = 12;
    let max_green = 13;
    let max_blue = 14;

    games.iter()
        .filter(|game| is_game_possible(game, max_red, max_green, max_blue))
        .map(|game| game.id)
        .sum()
}

fn part2(games: &[Game]) -> u32 {
    games.iter()
        .map(|game| {
            let min_cubes = find_minimum_cubes(game);
            calculate_power(&min_cubes)
        })
        .sum()
}

fn main() {
    // Read input file
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    // Parse all games
    let games: Vec<Game> = input.lines()
        .filter_map(|line| parse_game(line))
        .collect();

    // Solve both parts
    let part1_result = part1(&games);
    let part2_result = part2(&games);

    println!("Part 1: {}", part1_result);
    println!("Part 2: {}", part2_result);
}
