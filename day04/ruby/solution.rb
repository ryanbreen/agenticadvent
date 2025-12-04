#!/usr/bin/env ruby

# Read input file
input_text = File.read(File.join(__dir__, '..', 'input.txt')).strip
lines = input_text.split("\n")

# Define the 8 directions (N, NE, E, SE, S, SW, W, NW)
DIRECTIONS = [
  [-1, 0],  # N
  [-1, 1],  # NE
  [0, 1],   # E
  [1, 1],   # SE
  [1, 0],   # S
  [1, -1],  # SW
  [0, -1],  # W
  [-1, -1]  # NW
].freeze

# Count the number of adjacent rolls ('@') around position (row, col)
def count_adjacent_rolls(grid, row, col)
  rows = grid.length
  cols = grid[0].length
  count = 0

  DIRECTIONS.each do |dr, dc|
    new_row = row + dr
    new_col = col + dc

    # Check if neighbor is in bounds and is a paper roll
    if new_row >= 0 && new_row < rows &&
       new_col >= 0 && new_col < cols &&
       grid[new_row][new_col] == '@'
      count += 1
    end
  end

  count
end

def part1(lines)
  # Parse the grid
  grid = lines.map { |line| line.chars }
  rows = grid.length
  cols = grid[0].length

  accessible_count = 0

  # Check each cell
  (0...rows).each do |row|
    (0...cols).each do |col|
      # Only check cells with paper rolls
      next unless grid[row][col] == '@'

      adjacent_rolls = count_adjacent_rolls(grid, row, col)

      # Accessible if fewer than 4 adjacent rolls
      accessible_count += 1 if adjacent_rolls < 4
    end
  end

  accessible_count
end

def part2(lines)
  # Parse the grid (need to make a mutable copy)
  grid = lines.map { |line| line.chars }
  rows = grid.length
  cols = grid[0].length

  total_removed = 0

  loop do
    # Find all rolls with fewer than 4 adjacent rolls
    to_remove = []

    (0...rows).each do |row|
      (0...cols).each do |col|
        # Only check cells with paper rolls
        next unless grid[row][col] == '@'

        adjacent_rolls = count_adjacent_rolls(grid, row, col)

        # Mark for removal if fewer than 4 adjacent rolls
        to_remove << [row, col] if adjacent_rolls < 4
      end
    end

    # If no rolls can be removed, we're done
    break if to_remove.empty?

    # Remove all accessible rolls
    to_remove.each do |row, col|
      grid[row][col] = '.'
    end

    # Add to total count
    total_removed += to_remove.length
  end

  total_removed
end

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
