#!/usr/bin/env ruby
# Advent of Code 2023 Day 12: Hot Springs

def count_arrangements(pattern, groups)
  memo = {}

  dp = lambda do |pos, group_idx, current_run|
    key = [pos, group_idx, current_run]
    return memo[key] if memo.key?(key)

    # Base case: reached end of pattern
    if pos == pattern.length
      if group_idx == groups.length && current_run == 0
        return memo[key] = 1
      end
      if group_idx == groups.length - 1 && groups[group_idx] == current_run
        return memo[key] = 1
      end
      return memo[key] = 0
    end

    result = 0
    char = pattern[pos]

    # Option 1: Place operational spring (.)
    if char == '.' || char == '?'
      if current_run == 0
        # No active run, just move forward
        result += dp.call(pos + 1, group_idx, 0)
      elsif group_idx < groups.length && groups[group_idx] == current_run
        # End current run if it matches expected group size
        result += dp.call(pos + 1, group_idx + 1, 0)
      end
      # Otherwise invalid (run doesn't match group)
    end

    # Option 2: Place damaged spring (#)
    if char == '#' || char == '?'
      if group_idx < groups.length && current_run < groups[group_idx]
        # Can extend current run
        result += dp.call(pos + 1, group_idx, current_run + 1)
      end
      # Otherwise invalid (exceeds group size or no more groups)
    end

    memo[key] = result
  end

  dp.call(0, 0, 0)
end

def parse_line(line)
  parts = line.strip.split
  pattern = parts[0]
  groups = parts[1].split(',').map(&:to_i)
  [pattern, groups]
end

def unfold(pattern, groups, times = 5)
  unfolded_pattern = ([pattern] * times).join('?')
  unfolded_groups = groups * times
  [unfolded_pattern, unfolded_groups]
end

def part1(lines)
  total = 0
  lines.each do |line|
    next if line.strip.empty?
    pattern, groups = parse_line(line)
    total += count_arrangements(pattern, groups)
  end
  total
end

def part2(lines)
  total = 0
  lines.each do |line|
    next if line.strip.empty?
    pattern, groups = parse_line(line)
    unfolded_pattern, unfolded_groups = unfold(pattern, groups)
    total += count_arrangements(unfolded_pattern, unfolded_groups)
  end
  total
end

def main
  lines = File.readlines(File.join(__dir__, '..', 'input.txt'))

  puts "Part 1: #{part1(lines)}"
  puts "Part 2: #{part2(lines)}"
end

main
