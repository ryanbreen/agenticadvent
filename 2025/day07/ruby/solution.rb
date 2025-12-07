#!/usr/bin/env ruby

require 'set'

# Read input
input_text = File.read(File.join(__dir__, '..', 'input.txt')).strip
lines = input_text.split("\n")

def part1(lines)
  rows = lines.length
  cols = rows > 0 ? lines[0].length : 0

  # Find starting position S
  start_col = nil
  cols.times do |col|
    if lines[0][col] == 'S'
      start_col = col
      break
    end
  end

  return 0 if start_col.nil?

  # Track active beam columns at each row
  # Use a set to handle beam merging
  active_beams = Set.new([start_col])
  split_count = 0

  # Process row by row starting from row 1 (below S)
  (1...rows).each do |row|
    new_beams = Set.new

    active_beams.each do |col|
      if col >= 0 && col < cols
        cell = lines[row][col]
        if cell == '^'
          # Beam hits splitter - count it and emit left/right
          split_count += 1
          # Left beam goes to col-1, right beam goes to col+1
          new_beams.add(col - 1) if col - 1 >= 0
          new_beams.add(col + 1) if col + 1 < cols
        elsif cell == '.'
          # Beam continues straight down
          new_beams.add(col)
        else
          # If cell is something else (like S), beam continues
          new_beams.add(col)
        end
      end
    end

    active_beams = new_beams

    # If no more beams, stop
    break if active_beams.empty?
  end

  split_count
end

def part2(lines)
  rows = lines.length
  cols = rows > 0 ? lines[0].length : 0

  # Find starting position S
  start_col = nil
  cols.times do |col|
    if lines[0][col] == 'S'
      start_col = col
      break
    end
  end

  return 0 if start_col.nil?

  # Track number of timelines at each column position
  # Use a hash: col -> count of timelines at that position
  timelines = Hash.new(0)
  timelines[start_col] = 1

  # Process row by row starting from row 1 (below S)
  (1...rows).each do |row|
    new_timelines = Hash.new(0)

    timelines.each do |col, count|
      if col >= 0 && col < cols
        cell = lines[row][col]
        if cell == '^'
          # Each timeline splits into 2 (left and right)
          new_timelines[col - 1] += count if col - 1 >= 0
          new_timelines[col + 1] += count if col + 1 < cols
        elsif cell == '.'
          # Timelines continue straight down
          new_timelines[col] += count
        else
          # Other characters - timelines continue
          new_timelines[col] += count
        end
      end
    end

    timelines = new_timelines

    # If no more timelines, stop
    break if timelines.empty?
  end

  # Total number of timelines
  timelines.values.sum
end

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
