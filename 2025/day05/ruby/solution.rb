#!/usr/bin/env ruby

# Read input from file
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
input = File.read(input_path).strip

# Parse input into sections
ranges_section, ids_section = input.split("\n\n")

# Parse fresh ingredient ranges
ranges = ranges_section.split("\n").map do |line|
  start_val, end_val = line.split('-').map(&:to_i)
  { start: start_val, end: end_val }
end

# Parse available ingredient IDs
available_ids = ids_section.split("\n").map(&:to_i)

# Part 1: Count how many available ingredient IDs are fresh
def part1(ranges, available_ids)
  fresh_count = 0

  available_ids.each do |id|
    # Check if ID falls within any range
    is_fresh = ranges.any? { |range| id >= range[:start] && id <= range[:end] }
    fresh_count += 1 if is_fresh
  end

  fresh_count
end

# Part 2: Count total unique ingredient IDs covered by all ranges
def part2(ranges)
  # Sort ranges by start position
  sorted_ranges = ranges.sort_by { |r| r[:start] }

  # Merge overlapping ranges
  merged = []

  sorted_ranges.each do |range|
    if merged.empty?
      merged << range.dup
    else
      last = merged.last

      # Check if current range overlaps or is adjacent to the last merged range
      if range[:start] <= last[:end] + 1
        # Merge by extending the end if necessary
        last[:end] = [last[:end], range[:end]].max
      else
        # No overlap, add as new range
        merged << range.dup
      end
    end
  end

  # Sum up the size of each merged range
  total_count = 0
  merged.each do |range|
    count = range[:end] - range[:start] + 1
    total_count += count
  end

  total_count
end

puts "Part 1: #{part1(ranges, available_ids)}"
puts "Part 2: #{part2(ranges)}"
