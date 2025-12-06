#!/usr/bin/env ruby

# Read input file
input_text = File.read(File.join(__dir__, '..', 'input.txt')).strip
lines = input_text.split("\n")

# Parse the worksheet into a list of [numbers, operator] pairs
def parse_problems(lines)
  return [] if lines.empty?

  # Find the operator row (last non-empty row with only +, *, and spaces)
  op_row_idx = lines.length - 1
  while op_row_idx >= 0 && (lines[op_row_idx].strip.empty? ||
        !lines[op_row_idx].chars.all? { |c| c == '+' || c == '*' || c == ' ' })
    op_row_idx -= 1
  end

  return [] if op_row_idx < 0

  op_row = lines[op_row_idx]
  number_rows = lines[0...op_row_idx]

  # Find max width
  max_width = lines.map(&:length).max

  # Pad all rows to the same width
  padded_number_rows = number_rows.map { |line| line.ljust(max_width) }
  padded_op_row = op_row.ljust(max_width)

  # Find problem boundaries by looking for columns that are all spaces
  problems = []
  col = 0

  while col < max_width
    # Skip separator columns (all spaces)
    while col < max_width && padded_number_rows.all? { |row| row[col] == ' ' } && padded_op_row[col] == ' '
      col += 1
    end

    break if col >= max_width

    # Find the end of this problem
    start_col = col
    while col < max_width
      # Check if this is a separator column
      is_separator = padded_number_rows.all? { |row| row[col] == ' ' } && padded_op_row[col] == ' '
      break if is_separator
      col += 1
    end

    end_col = col

    # Extract numbers and operator for this problem
    numbers = []
    padded_number_rows.each do |row|
      num_str = row[start_col...end_col].strip
      numbers << num_str.to_i unless num_str.empty?
    end

    op_str = padded_op_row[start_col...end_col].strip
    problems << [numbers, op_str] if !op_str.empty? && !numbers.empty?
  end

  problems
end

# Parse the worksheet for part 2 - reading right-to-left columns
def parse_problems_part2(lines)
  return [] if lines.empty?

  # Find the operator row (last non-empty row with only +, *, and spaces)
  op_row_idx = lines.length - 1
  while op_row_idx >= 0 && (lines[op_row_idx].strip.empty? ||
        !lines[op_row_idx].chars.all? { |c| c == '+' || c == '*' || c == ' ' })
    op_row_idx -= 1
  end

  return [] if op_row_idx < 0

  op_row = lines[op_row_idx]
  number_rows = lines[0...op_row_idx]

  # Find max width
  max_width = lines.map(&:length).max

  # Pad all rows to the same width
  padded_number_rows = number_rows.map { |line| line.ljust(max_width) }
  padded_op_row = op_row.ljust(max_width)

  # Find problem boundaries by looking for columns that are all spaces
  problems = []
  col = 0

  while col < max_width
    # Skip separator columns (all spaces)
    while col < max_width && padded_number_rows.all? { |row| row[col] == ' ' } && padded_op_row[col] == ' '
      col += 1
    end

    break if col >= max_width

    # Find the end of this problem
    start_col = col
    while col < max_width
      # Check if this is a separator column
      is_separator = padded_number_rows.all? { |row| row[col] == ' ' } && padded_op_row[col] == ' '
      break if is_separator
      col += 1
    end

    end_col = col

    # For Part 2: Read columns right-to-left, each column forms a number
    # reading top-to-bottom as most-to-least significant digit
    numbers = []
    (end_col - 1).downto(start_col) do |c|
      digits = []
      padded_number_rows.each do |row|
        ch = row[c]
        digits << ch if ch =~ /\d/
      end
      unless digits.empty?
        # Join digits to form number (top=most significant, bottom=least)
        num = digits.join.to_i
        numbers << num
      end
    end

    op_str = padded_op_row[start_col...end_col].strip
    problems << [numbers, op_str] if !op_str.empty? && !numbers.empty?
  end

  problems
end

# Solve a single problem given numbers and operator
def solve_problem(numbers, op)
  if op == '+'
    numbers.sum
  elsif op == '*'
    numbers.reduce(1, :*)
  else
    0
  end
end

# Part 1
def part1(lines)
  problems = parse_problems(lines)
  total = 0
  problems.each do |numbers, op|
    result = solve_problem(numbers, op)
    total += result
  end
  total
end

# Part 2
def part2(lines)
  problems = parse_problems_part2(lines)
  total = 0
  problems.each do |numbers, op|
    result = solve_problem(numbers, op)
    total += result
  end
  total
end

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
