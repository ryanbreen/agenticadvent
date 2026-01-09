use std::collections::HashMap;
use std::fs;

#[derive(Clone, Copy)]
struct Blueprint {
    id: u32,
    ore_ore: u32,
    clay_ore: u32,
    obs_ore: u32,
    obs_clay: u32,
    geo_ore: u32,
    geo_obs: u32,
}

fn parse_input(input: &str) -> Vec<Blueprint> {
    let mut blueprints = Vec::new();

    for line in input.lines() {
        if line.is_empty() {
            continue;
        }

        let nums: Vec<u32> = line
            .split(|c: char| !c.is_ascii_digit())
            .filter(|s| !s.is_empty())
            .map(|s| s.parse().unwrap())
            .collect();

        blueprints.push(Blueprint {
            id: nums[0],
            ore_ore: nums[1],
            clay_ore: nums[2],
            obs_ore: nums[3],
            obs_clay: nums[4],
            geo_ore: nums[5],
            geo_obs: nums[6],
        });
    }

    blueprints
}

fn max_geodes(bp: &Blueprint, time_limit: u32) -> u32 {
    let max_ore = bp.ore_ore.max(bp.clay_ore).max(bp.obs_ore).max(bp.geo_ore);
    let max_clay = bp.obs_clay;
    let max_obs = bp.geo_obs;

    let mut best = 0u32;
    let mut seen: HashMap<(u32, u32, u32, u32, u32, u32, u32, u32), u32> = HashMap::new();

    dfs(
        0, 0, 0, 0, 0,
        1, 0, 0, 0,
        time_limit, bp, max_ore, max_clay, max_obs,
        &mut best, &mut seen
    );

    best
}

fn dfs(
    time: u32, ore: u32, clay: u32, obs: u32, geodes: u32,
    ore_r: u32, clay_r: u32, obs_r: u32, geo_r: u32,
    time_limit: u32, bp: &Blueprint,
    max_ore: u32, max_clay: u32, max_obs: u32,
    best: &mut u32,
    seen: &mut HashMap<(u32, u32, u32, u32, u32, u32, u32, u32), u32>
) {
    let remaining = time_limit - time;

    // Upper bound pruning: geodes + geo_r * remaining + remaining*(remaining-1)/2
    let upper_bound = geodes + geo_r * remaining + (remaining * remaining.saturating_sub(1)) / 2;
    if upper_bound <= *best {
        return;
    }

    if time == time_limit {
        *best = (*best).max(geodes);
        return;
    }

    // Cap resources to what could possibly be used
    let capped_ore = ore.min(remaining * max_ore);
    let capped_clay = clay.min(remaining * max_clay);
    let capped_obs = obs.min(remaining * max_obs);

    // State deduplication
    let key = (time, capped_ore, capped_clay, capped_obs, ore_r, clay_r, obs_r, geo_r);
    if let Some(&prev_geodes) = seen.get(&key) {
        if prev_geodes >= geodes {
            return;
        }
    }
    seen.insert(key, geodes);

    // Collect resources
    let new_ore = capped_ore + ore_r;
    let new_clay = capped_clay + clay_r;
    let new_obs = capped_obs + obs_r;
    let new_geodes = geodes + geo_r;

    // Try building geode robot (always do if possible)
    if capped_ore >= bp.geo_ore && capped_obs >= bp.geo_obs {
        dfs(
            time + 1, new_ore - bp.geo_ore, new_clay, new_obs - bp.geo_obs, new_geodes,
            ore_r, clay_r, obs_r, geo_r + 1,
            time_limit, bp, max_ore, max_clay, max_obs, best, seen
        );
        return; // If we can build geode, always do it
    }

    // Try building obsidian robot
    if capped_ore >= bp.obs_ore && capped_clay >= bp.obs_clay && obs_r < max_obs {
        dfs(
            time + 1, new_ore - bp.obs_ore, new_clay - bp.obs_clay, new_obs, new_geodes,
            ore_r, clay_r, obs_r + 1, geo_r,
            time_limit, bp, max_ore, max_clay, max_obs, best, seen
        );
    }

    // Try building clay robot
    if capped_ore >= bp.clay_ore && clay_r < max_clay {
        dfs(
            time + 1, new_ore - bp.clay_ore, new_clay, new_obs, new_geodes,
            ore_r, clay_r + 1, obs_r, geo_r,
            time_limit, bp, max_ore, max_clay, max_obs, best, seen
        );
    }

    // Try building ore robot
    if capped_ore >= bp.ore_ore && ore_r < max_ore {
        dfs(
            time + 1, new_ore - bp.ore_ore, new_clay, new_obs, new_geodes,
            ore_r + 1, clay_r, obs_r, geo_r,
            time_limit, bp, max_ore, max_clay, max_obs, best, seen
        );
    }

    // Do nothing (wait)
    dfs(
        time + 1, new_ore, new_clay, new_obs, new_geodes,
        ore_r, clay_r, obs_r, geo_r,
        time_limit, bp, max_ore, max_clay, max_obs, best, seen
    );
}

fn part1(blueprints: &[Blueprint]) -> u32 {
    blueprints.iter()
        .map(|bp| bp.id * max_geodes(bp, 24))
        .sum()
}

fn part2(blueprints: &[Blueprint]) -> u32 {
    blueprints.iter()
        .take(3)
        .map(|bp| max_geodes(bp, 32))
        .product()
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let blueprints = parse_input(&input);

    println!("Part 1: {}", part1(&blueprints));
    println!("Part 2: {}", part2(&blueprints));
}
