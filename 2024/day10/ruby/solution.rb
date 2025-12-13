#!/usr/bin/env ruby

require 'set'

# Read and parse input
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
input_text = File.read(input_path).strip

# Parse input into grid
lines = input_text.split("\n")
grid = lines.map { |line| line.chars.map(&:to_i) }
rows = grid.length
cols = grid[0].length

# Directions: up, down, left, right
DIRS = [[-1, 0], [1, 0], [0, -1], [0, 1]]

# Find all positions with height 0.
def find_trailheads(grid)
  trailheads = []
  grid.each_with_index do |row, r|
    row.each_with_index do |height, c|
      trailheads << [r, c] if height == 0
    end
  end
  trailheads
end

# BFS to find all 9s reachable from a trailhead.
def count_reachable_nines(grid, rows, cols, start_r, start_c)
  visited = Set.new
  visited.add([start_r, start_c])
  queue = [[start_r, start_c]]
  nines = Set.new

  until queue.empty?
    r, c = queue.shift
    current_height = grid[r][c]

    if current_height == 9
      nines.add([r, c])
      next
    end

    # Try all four directions
    DIRS.each do |dr, dc|
      nr, nc = r + dr, c + dc
      if nr >= 0 && nr < rows && nc >= 0 && nc < cols
        if !visited.include?([nr, nc])
          if grid[nr][nc] == current_height + 1
            visited.add([nr, nc])
            queue << [nr, nc]
          end
        end
      end
    end
  end

  nines.size
end

def part1(grid, rows, cols)
  trailheads = find_trailheads(grid)
  total_score = trailheads.sum { |r, c| count_reachable_nines(grid, rows, cols, r, c) }
  total_score
end

# DFS to count all distinct trails from a trailhead to any 9.
def count_distinct_trails(grid, rows, cols, start_r, start_c)
  dfs_helper(grid, rows, cols, start_r, start_c)
end

def dfs_helper(grid, rows, cols, r, c)
  current_height = grid[r][c]
  return 1 if current_height == 9

  total = 0
  DIRS.each do |dr, dc|
    nr, nc = r + dr, c + dc
    if nr >= 0 && nr < rows && nc >= 0 && nc < cols
      if grid[nr][nc] == current_height + 1
        total += dfs_helper(grid, rows, cols, nr, nc)
      end
    end
  end
  total
end

def part2(grid, rows, cols)
  trailheads = find_trailheads(grid)
  total_rating = trailheads.sum { |r, c| count_distinct_trails(grid, rows, cols, r, c) }
  total_rating
end

if __FILE__ == $0
  puts "Part 1: #{part1(grid, rows, cols)}"
  puts "Part 2: #{part2(grid, rows, cols)}"
end
