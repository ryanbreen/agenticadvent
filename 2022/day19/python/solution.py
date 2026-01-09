#!/usr/bin/env python3
import os
import re
from collections import deque

def parse_input(text):
    """Parse blueprints from input."""
    blueprints = []
    pattern = r'Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\.'

    for line in text.strip().split('\n'):
        m = re.match(pattern, line)
        if m:
            bp_id, ore_ore, clay_ore, obs_ore, obs_clay, geo_ore, geo_obs = map(int, m.groups())
            blueprints.append((bp_id, ore_ore, clay_ore, obs_ore, obs_clay, geo_ore, geo_obs))
    return blueprints

def max_geodes(bp, time_limit):
    """Find maximum geodes using BFS with state pruning."""
    bp_id, ore_ore, clay_ore, obs_ore, obs_clay, geo_ore, geo_obs = bp

    # Max robots needed per type
    max_ore = max(ore_ore, clay_ore, obs_ore, geo_ore)
    max_clay = obs_clay
    max_obs = geo_obs

    # State: (time, ore, clay, obs, geodes, ore_r, clay_r, obs_r, geo_r)
    # Start: time=0, 0 resources, 1 ore robot
    initial = (0, 0, 0, 0, 0, 1, 0, 0, 0)

    best = 0
    seen = {}  # (ore_r, clay_r, obs_r, geo_r) -> best geodes at time

    queue = deque([initial])

    while queue:
        state = queue.popleft()
        time, ore, clay, obs, geodes, ore_r, clay_r, obs_r, geo_r = state

        # Pruning: upper bound on possible geodes
        remaining = time_limit - time
        upper_bound = geodes + geo_r * remaining + (remaining * (remaining - 1)) // 2
        if upper_bound <= best:
            continue

        if time == time_limit:
            best = max(best, geodes)
            continue

        # Cap resources at what we could possibly use
        # Can only spend max_ore per minute, so cap at time_remaining * max_ore
        ore = min(ore, remaining * max_ore)
        clay = min(clay, remaining * max_clay)
        obs = min(obs, remaining * max_obs)

        # State deduplication key
        key = (time, ore, clay, obs, ore_r, clay_r, obs_r, geo_r)
        if key in seen and seen[key] >= geodes:
            continue
        seen[key] = geodes

        # Collect resources
        new_ore = ore + ore_r
        new_clay = clay + clay_r
        new_obs = obs + obs_r
        new_geodes = geodes + geo_r

        # Try building geode robot (highest priority)
        if ore >= geo_ore and obs >= geo_obs:
            queue.append((time + 1, new_ore - geo_ore, new_clay, new_obs - geo_obs, new_geodes,
                         ore_r, clay_r, obs_r, geo_r + 1))
            # Still try other options - don't return early

        # Try building obsidian robot
        if ore >= obs_ore and clay >= obs_clay and obs_r < max_obs:
            queue.append((time + 1, new_ore - obs_ore, new_clay - obs_clay, new_obs, new_geodes,
                         ore_r, clay_r, obs_r + 1, geo_r))

        # Try building clay robot
        if ore >= clay_ore and clay_r < max_clay:
            queue.append((time + 1, new_ore - clay_ore, new_clay, new_obs, new_geodes,
                         ore_r, clay_r + 1, obs_r, geo_r))

        # Try building ore robot
        if ore >= ore_ore and ore_r < max_ore:
            queue.append((time + 1, new_ore - ore_ore, new_clay, new_obs, new_geodes,
                         ore_r + 1, clay_r, obs_r, geo_r))

        # Do nothing (wait)
        queue.append((time + 1, new_ore, new_clay, new_obs, new_geodes,
                     ore_r, clay_r, obs_r, geo_r))

    return best

def part1(text):
    """Sum of quality levels (blueprint_id * max_geodes) for 24 minutes."""
    blueprints = parse_input(text)
    total = 0
    for bp in blueprints:
        geodes = max_geodes(bp, 24)
        total += bp[0] * geodes
    return total

def part2(text):
    """Product of max geodes for first 3 blueprints with 32 minutes."""
    blueprints = parse_input(text)[:3]
    result = 1
    for bp in blueprints:
        geodes = max_geodes(bp, 32)
        result *= geodes
    return result

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        text = f.read()

    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
