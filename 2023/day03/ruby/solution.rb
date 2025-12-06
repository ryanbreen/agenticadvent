#!/usr/bin/env ruby

# Read input
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
lines = File.read(input_path).strip.split("\n")

# Helper to check if a character is a symbol (not digit, not period)
def symbol?(char)
  char && char != '.' && !char.match?(/\d/)
end

# Helper to get adjacent positions (including diagonals)
def adjacent_positions(row, col_start, col_end)
  positions = []

  # Check row above
  ((col_start - 1)..(col_end + 1)).each do |c|
    positions << [row - 1, c]
  end

  # Check same row (left and right)
  positions << [row, col_start - 1]
  positions << [row, col_end + 1]

  # Check row below
  ((col_start - 1)..(col_end + 1)).each do |c|
    positions << [row + 1, c]
  end

  positions
end

# Helper to get character at position
def char_at(lines, row, col)
  return nil if row < 0 || row >= lines.length
  return nil if col < 0 || col >= lines[row].length
  lines[row][col]
end

def part1(lines)
  part_numbers = []

  lines.each_with_index do |line, row|
    # Find all numbers in this line
    col = 0
    while col < line.length
      if line[col].match?(/\d/)
        # Found start of a number
        num_start = col
        num_str = ""

        while col < line.length && line[col].match?(/\d/)
          num_str += line[col]
          col += 1
        end

        num_end = col - 1
        number = num_str.to_i

        # Check if any adjacent position has a symbol
        adjacent = adjacent_positions(row, num_start, num_end)
        has_adjacent_symbol = adjacent.any? do |r, c|
          ch = char_at(lines, r, c)
          symbol?(ch)
        end

        if has_adjacent_symbol
          part_numbers << number
        end
      else
        col += 1
      end
    end
  end

  part_numbers.sum
end

def part2(lines)
  # Find all numbers and track which * positions they're adjacent to
  gear_candidates = Hash.new { |h, k| h[k] = [] }

  lines.each_with_index do |line, row|
    # Find all numbers in this line
    col = 0
    while col < line.length
      if line[col].match?(/\d/)
        # Found start of a number
        num_start = col
        num_str = ""

        while col < line.length && line[col].match?(/\d/)
          num_str += line[col]
          col += 1
        end

        num_end = col - 1
        number = num_str.to_i

        # Find adjacent * symbols
        adjacent = adjacent_positions(row, num_start, num_end)
        adjacent.each do |r, c|
          ch = char_at(lines, r, c)
          if ch == '*'
            gear_candidates[[r, c]] << number
          end
        end
      else
        col += 1
      end
    end
  end

  # Find gears (exactly 2 adjacent numbers) and calculate gear ratios
  gear_ratios = []
  gear_candidates.each do |pos, numbers|
    if numbers.length == 2
      gear_ratios << (numbers[0] * numbers[1])
    end
  end

  gear_ratios.sum
end

# Run both parts
puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
