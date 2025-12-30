#!/usr/bin/env ruby
# frozen_string_literal: true

require 'set'

def count_energized(grid, start_row, start_col, start_dir)
  # Directions: 0=right, 1=down, 2=left, 3=up
  rows = grid.length
  cols = grid[0].length
  dr = [0, 1, 0, -1]
  dc = [1, 0, -1, 0]

  visited = Set.new
  queue = [[start_row, start_col, start_dir]]

  until queue.empty?
    r, c, d = queue.shift

    next if r < 0 || r >= rows || c < 0 || c >= cols

    state = [r, c, d]
    next if visited.include?(state)

    visited.add(state)

    cell = grid[r][c]

    next_dirs = case cell
                when '.'
                  [d]
                when '/'
                  [[3, 2, 1, 0][d]]
                when '\\'
                  [[1, 0, 3, 2][d]]
                when '|'
                  [0, 2].include?(d) ? [1, 3] : [d]
                when '-'
                  [1, 3].include?(d) ? [0, 2] : [d]
                end

    next_dirs.each do |nd|
      queue << [r + dr[nd], c + dc[nd], nd]
    end
  end

  visited.map { |r, c, _d| [r, c] }.uniq.length
end

def part1(grid)
  count_energized(grid, 0, 0, 0)
end

def part2(grid)
  rows = grid.length
  cols = grid[0].length
  max_energized = 0

  # Top row, heading down
  cols.times do |c|
    max_energized = [max_energized, count_energized(grid, 0, c, 1)].max
  end

  # Bottom row, heading up
  cols.times do |c|
    max_energized = [max_energized, count_energized(grid, rows - 1, c, 3)].max
  end

  # Left column, heading right
  rows.times do |r|
    max_energized = [max_energized, count_energized(grid, r, 0, 0)].max
  end

  # Right column, heading left
  rows.times do |r|
    max_energized = [max_energized, count_energized(grid, r, cols - 1, 2)].max
  end

  max_energized
end

def main
  input_file = File.join(__dir__, '..', 'input.txt')
  grid = File.read(input_file).strip.split("\n")

  puts "Part 1: #{part1(grid)}"
  puts "Part 2: #{part2(grid)}"
end

main
