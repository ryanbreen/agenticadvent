#!/usr/bin/env ruby
# frozen_string_literal: true

require 'set'

# Four cardinal directions for neighbor checks
DIRECTIONS = [[0, 1], [0, -1], [1, 0], [-1, 0]].freeze

# Read and parse input
input_path = File.join(__dir__, '..', 'input.txt')
input_text = File.read(input_path).strip

# Parse grid
grid = input_text.split("\n").map(&:chars)

# Find all connected regions in the grid using BFS
def find_regions(grid)
  rows = grid.length
  cols = grid[0].length
  visited = Set.new
  regions = []

  rows.times do |r|
    cols.times do |c|
      next if visited.include?([r, c])

      # BFS to find all cells in this region
      plant = grid[r][c]
      region = Set.new
      queue = [[r, c]]

      until queue.empty?
        cr, cc = queue.shift
        next if visited.include?([cr, cc])
        next if cr < 0 || cr >= rows || cc < 0 || cc >= cols
        next if grid[cr][cc] != plant

        visited.add([cr, cc])
        region.add([cr, cc])

        DIRECTIONS.each do |dr, dc|
          nr, nc = cr + dr, cc + dc
          queue.push([nr, nc]) unless visited.include?([nr, nc])
        end
      end

      regions.push(region)
    end
  end

  regions
end

# Calculate perimeter of a region (edges not touching same region)
def calculate_perimeter(region)
  region.sum do |r, c|
    DIRECTIONS.count { |dr, dc| !region.include?([r + dr, c + dc]) }
  end
end

# Count number of sides (corners) in a region
def count_sides(region)
  region.sum do |r, c|
    # Check all 4 corners of this cell
    # Each corner is defined by checking two orthogonal neighbors and the diagonal
    # Convex: both orthogonal out
    # Concave: both orthogonal in, diagonal out

    up = region.include?([r - 1, c])
    down = region.include?([r + 1, c])
    left = region.include?([r, c - 1])
    right = region.include?([r, c + 1])
    up_left = region.include?([r - 1, c - 1])
    up_right = region.include?([r - 1, c + 1])
    down_left = region.include?([r + 1, c - 1])
    down_right = region.include?([r + 1, c + 1])

    corners = 0
    # Top-left corner
    corners += 1 if !up && !left  # convex
    corners += 1 if up && left && !up_left  # concave

    # Top-right corner
    corners += 1 if !up && !right  # convex
    corners += 1 if up && right && !up_right  # concave

    # Bottom-left corner
    corners += 1 if !down && !left  # convex
    corners += 1 if down && left && !down_left  # concave

    # Bottom-right corner
    corners += 1 if !down && !right  # convex
    corners += 1 if down && right && !down_right  # concave

    corners
  end
end

# Calculate total fencing cost: sum of area * perimeter for each region
def part1(grid)
  regions = find_regions(grid)
  regions.sum do |region|
    region.size * calculate_perimeter(region)
  end
end

# Calculate total fencing cost using sides instead of perimeter
def part2(grid)
  regions = find_regions(grid)
  regions.sum do |region|
    region.size * count_sides(region)
  end
end

# Run solutions
puts "Part 1: #{part1(grid)}"
puts "Part 2: #{part2(grid)}"
