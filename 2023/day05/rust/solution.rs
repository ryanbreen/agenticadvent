use std::fs;

/// A mapping range: (destination_start, source_start, length)
type MapRange = (u64, u64, u64);

/// A complete map is a list of ranges
type Map = Vec<MapRange>;

/// An interval range for Part 2: (start, end) where end is exclusive
type Interval = (u64, u64);

/// Parse the input into seeds and a list of maps
fn parse_input(text: &str) -> (Vec<u64>, Vec<Map>) {
    let sections: Vec<&str> = text.trim().split("\n\n").collect();

    // Parse seeds
    let seeds: Vec<u64> = sections[0]
        .split(": ")
        .nth(1)
        .unwrap()
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();

    // Parse maps
    let maps: Vec<Map> = sections[1..]
        .iter()
        .map(|section| {
            section
                .lines()
                .skip(1) // Skip header line
                .map(|line| {
                    let nums: Vec<u64> = line
                        .split_whitespace()
                        .map(|s| s.parse().unwrap())
                        .collect();
                    (nums[0], nums[1], nums[2])
                })
                .collect()
        })
        .collect();

    (seeds, maps)
}

/// Apply a single map to transform a value
fn apply_map(value: u64, ranges: &Map) -> u64 {
    for &(dst_start, src_start, length) in ranges {
        if src_start <= value && value < src_start + length {
            return dst_start + (value - src_start);
        }
    }
    value
}

/// Convert a seed number to a location number through all maps
fn seed_to_location(seed: u64, maps: &[Map]) -> u64 {
    let mut value = seed;
    for map_ranges in maps {
        value = apply_map(value, map_ranges);
    }
    value
}

/// Find the lowest location number for any initial seed (Part 1)
fn part1(seeds: &[u64], maps: &[Map]) -> u64 {
    seeds
        .iter()
        .map(|&seed| seed_to_location(seed, maps))
        .min()
        .unwrap()
}

/// Apply a map to a list of intervals, returning new intervals
fn apply_map_to_ranges(input_ranges: &[Interval], map_ranges: &Map) -> Vec<Interval> {
    let mut result = Vec::new();

    for &(start, end) in input_ranges {
        let mut remaining = vec![(start, end)];

        for &(dst_start, src_start, length) in map_ranges {
            let src_end = src_start + length;
            let mut new_remaining = Vec::new();

            for (r_start, r_end) in remaining {
                // Part before the map range (unmapped)
                if r_start < src_start {
                    new_remaining.push((r_start, r_end.min(src_start)));
                }

                // Part within the map range (mapped)
                let overlap_start = r_start.max(src_start);
                let overlap_end = r_end.min(src_end);
                if overlap_start < overlap_end {
                    let offset = dst_start as i64 - src_start as i64;
                    result.push((
                        (overlap_start as i64 + offset) as u64,
                        (overlap_end as i64 + offset) as u64,
                    ));
                }

                // Part after the map range (unmapped)
                if r_end > src_end {
                    new_remaining.push((r_start.max(src_end), r_end));
                }
            }

            remaining = new_remaining;
        }

        // Any remaining parts are unmapped (identity)
        result.extend(remaining);
    }

    result
}

/// Find the lowest location for seed ranges (Part 2)
fn part2(seeds: &[u64], maps: &[Map]) -> u64 {
    // Convert seeds to ranges: pairs of (start, start + length)
    let mut ranges: Vec<Interval> = seeds
        .chunks(2)
        .map(|chunk| (chunk[0], chunk[0] + chunk[1]))
        .collect();

    // Apply each map to the ranges
    for map_ranges in maps {
        ranges = apply_map_to_ranges(&ranges, map_ranges);
    }

    // Find minimum start of any range
    ranges.iter().map(|&(start, _)| start).min().unwrap()
}

fn main() {
    let input_path = std::path::Path::new(file!())
        .parent()
        .unwrap()
        .join("../input.txt");
    let text = fs::read_to_string(&input_path)
        .unwrap_or_else(|_| fs::read_to_string("../input.txt").unwrap());

    let (seeds, maps) = parse_input(&text);

    println!("Part 1: {}", part1(&seeds, &maps));
    println!("Part 2: {}", part2(&seeds, &maps));
}
