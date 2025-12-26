#!/usr/bin/env ruby
# Day 25: Code Chronicle - Lock and key matching

def parse_input(text)
  locks = []
  keys = []

  schematics = text.strip.split("\n\n")

  schematics.each do |schematic|
    lines = schematic.strip.split("\n")

    # Lock: top row is all #, bottom is all .
    # Key: top row is all ., bottom is all #
    if lines[0] == '#####'
      # It's a lock - count # from top (excluding top row)
      heights = []
      5.times do |col|
        height = 0
        (1..6).each do |row|  # rows 1-6
          if lines[row][col] == '#'
            height += 1
          else
            break
          end
        end
        heights << height
      end
      locks << heights
    else
      # It's a key - count # from bottom (excluding bottom row)
      heights = []
      5.times do |col|
        height = 0
        (5).downto(0) do |row|  # rows 5 down to 0
          if lines[row][col] == '#'
            height += 1
          else
            break
          end
        end
        heights << height
      end
      keys << heights
    end
  end

  [locks, keys]
end

def fits?(lock, key)
  # Check if a key fits a lock (no column exceeds 5)
  5.times do |i|
    return false if lock[i] + key[i] > 5
  end
  true
end

def part1(locks, keys)
  # Count unique lock/key pairs that fit together
  count = 0
  locks.each do |lock|
    keys.each do |key|
      count += 1 if fits?(lock, key)
    end
  end
  count
end

# Read input
input_file = File.join(File.dirname(__FILE__), '..', 'input.txt')
text = File.read(input_file)

locks, keys = parse_input(text)

answer1 = part1(locks, keys)
puts "Part 1: #{answer1}"

# Day 25 typically only has Part 1
puts "Part 2: Merry Christmas!"
