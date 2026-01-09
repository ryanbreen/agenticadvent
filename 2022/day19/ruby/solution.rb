#!/usr/bin/env ruby

input = File.read(File.join(__dir__, '..', 'input.txt')).strip

def parse_input(text)
  blueprints = []
  pattern = /Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\./

  text.each_line do |line|
    m = line.match(pattern)
    if m
      blueprints << m.captures.map(&:to_i)
    end
  end
  blueprints
end

def max_geodes(bp, time_limit)
  bp_id, ore_ore, clay_ore, obs_ore, obs_clay, geo_ore, geo_obs = bp

  # Max robots needed per type
  max_ore = [ore_ore, clay_ore, obs_ore, geo_ore].max
  max_clay = obs_clay
  max_obs = geo_obs

  best = [0]
  seen = {}

  dfs = lambda do |time, ore, clay, obs, geodes, ore_r, clay_r, obs_r, geo_r|
    # Pruning: upper bound on possible geodes
    remaining = time_limit - time
    upper_bound = geodes + geo_r * remaining + (remaining * (remaining - 1)) / 2
    return if upper_bound <= best[0]

    if time == time_limit
      best[0] = [best[0], geodes].max
      return
    end

    # Cap resources
    capped_ore = [ore, remaining * max_ore].min
    capped_clay = [clay, remaining * max_clay].min
    capped_obs = [obs, remaining * max_obs].min

    # State deduplication
    key = [time, capped_ore, capped_clay, capped_obs, ore_r, clay_r, obs_r, geo_r]
    if seen.key?(key) && seen[key] >= geodes
      return
    end
    seen[key] = geodes

    # Collect resources
    new_ore = capped_ore + ore_r
    new_clay = capped_clay + clay_r
    new_obs = capped_obs + obs_r
    new_geodes = geodes + geo_r

    # Try building geode robot (always do if possible)
    if capped_ore >= geo_ore && capped_obs >= geo_obs
      dfs.call(time + 1, new_ore - geo_ore, new_clay, new_obs - geo_obs, new_geodes,
               ore_r, clay_r, obs_r, geo_r + 1)
      return # If we can build geode, always do
    end

    # Try building obsidian robot
    if capped_ore >= obs_ore && capped_clay >= obs_clay && obs_r < max_obs
      dfs.call(time + 1, new_ore - obs_ore, new_clay - obs_clay, new_obs, new_geodes,
               ore_r, clay_r, obs_r + 1, geo_r)
    end

    # Try building clay robot
    if capped_ore >= clay_ore && clay_r < max_clay
      dfs.call(time + 1, new_ore - clay_ore, new_clay, new_obs, new_geodes,
               ore_r, clay_r + 1, obs_r, geo_r)
    end

    # Try building ore robot
    if capped_ore >= ore_ore && ore_r < max_ore
      dfs.call(time + 1, new_ore - ore_ore, new_clay, new_obs, new_geodes,
               ore_r + 1, clay_r, obs_r, geo_r)
    end

    # Do nothing (wait)
    dfs.call(time + 1, new_ore, new_clay, new_obs, new_geodes,
             ore_r, clay_r, obs_r, geo_r)
  end

  dfs.call(0, 0, 0, 0, 0, 1, 0, 0, 0)
  best[0]
end

def part1(input)
  blueprints = parse_input(input)
  total = 0
  blueprints.each do |bp|
    geodes = max_geodes(bp, 24)
    total += bp[0] * geodes
  end
  total
end

def part2(input)
  blueprints = parse_input(input).take(3)
  result = 1
  blueprints.each do |bp|
    geodes = max_geodes(bp, 32)
    result *= geodes
  end
  result
end

puts "Part 1: #{part1(input)}"
puts "Part 2: #{part2(input)}"
