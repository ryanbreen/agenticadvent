#!/usr/bin/env ruby
# frozen_string_literal: true

require 'set'

def parse_grid(lines)
  galaxies = []
  lines.each_with_index do |line, r|
    line.chars.each_with_index do |ch, c|
      galaxies << [r, c] if ch == '#'
    end
  end
  galaxies
end

def find_empty_rows_and_cols(lines)
  rows = lines.length
  cols = lines.empty? ? 0 : lines[0].length

  empty_rows = Set.new
  empty_cols = Set.new

  # Find empty rows
  lines.each_with_index do |line, r|
    empty_rows.add(r) unless line.include?('#')
  end

  # Find empty columns
  (0...cols).each do |c|
    empty_cols.add(c) if (0...rows).all? { |r| lines[r][c] != '#' }
  end

  [empty_rows, empty_cols]
end

def calculate_distances(galaxies, empty_rows, empty_cols, expansion_factor: 2)
  total = 0

  galaxies.combination(2).each do |(r1, c1), (r2, c2)|
    # Calculate row distance with expansion
    min_r, max_r = [r1, r2].minmax
    row_dist = max_r - min_r
    (min_r...max_r).each do |r|
      row_dist += expansion_factor - 1 if empty_rows.include?(r)
    end

    # Calculate column distance with expansion
    min_c, max_c = [c1, c2].minmax
    col_dist = max_c - min_c
    (min_c...max_c).each do |c|
      col_dist += expansion_factor - 1 if empty_cols.include?(c)
    end

    total += row_dist + col_dist
  end

  total
end

def part1(lines)
  galaxies = parse_grid(lines)
  empty_rows, empty_cols = find_empty_rows_and_cols(lines)
  calculate_distances(galaxies, empty_rows, empty_cols, expansion_factor: 2)
end

def part2(lines)
  galaxies = parse_grid(lines)
  empty_rows, empty_cols = find_empty_rows_and_cols(lines)
  calculate_distances(galaxies, empty_rows, empty_cols, expansion_factor: 1_000_000)
end

def main
  input_file = ARGV[0] || '../input.txt'
  lines = File.readlines(input_file, chomp: true)

  # Remove any trailing empty lines
  lines.pop while lines.any? && lines.last.empty?

  puts "Part 1: #{part1(lines)}"
  puts "Part 2: #{part2(lines)}"
end

main if __FILE__ == $PROGRAM_NAME
