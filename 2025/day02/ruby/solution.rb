#!/usr/bin/env ruby

# Read input file
input_text = File.read(File.join(__dir__, '..', 'input.txt')).strip

def is_invalid_id_part1(num)
  """Check if a number is invalid (a pattern repeated exactly twice)."""
  s = num.to_s
  length = s.length

  # Must have even length to be repeated twice
  return false if length % 2 != 0

  # Check if it starts with 0 (leading zeros not allowed)
  return false if s[0] == '0'

  # Split in half and check if both halves are identical
  mid = length / 2
  first_half = s[0...mid]
  second_half = s[mid..-1]

  first_half == second_half
end

def is_invalid_id_part2(num)
  """Check if a number is invalid (a pattern repeated at least twice)."""
  s = num.to_s
  length = s.length

  # Check if it starts with 0 (leading zeros not allowed)
  return false if s[0] == '0'

  # Try all possible pattern lengths from 1 to length//2
  # The pattern must be repeated at least twice, so max pattern length is length//2
  (1..length / 2).each do |pattern_length|
    # Check if the string length is divisible by pattern_length
    if length % pattern_length == 0
      pattern = s[0...pattern_length]
      # Check if repeating the pattern gives us the original string
      return true if pattern * (length / pattern_length) == s
    end
  end

  false
end

def part1(input_text)
  # Parse ranges from input
  ranges = []
  input_text.split(',').each do |part|
    part = part.strip
    if part.include?('-')
      parts = part.split('-')
      if parts.length == 2
        start_val = parts[0].to_i
        end_val = parts[1].to_i
        ranges << [start_val, end_val]
      end
    end
  end

  total = 0
  ranges.each do |start_val, end_val|
    (start_val..end_val).each do |num|
      total += num if is_invalid_id_part1(num)
    end
  end

  total
end

def part2(input_text)
  # Parse ranges from input
  ranges = []
  input_text.split(',').each do |part|
    part = part.strip
    if part.include?('-')
      parts = part.split('-')
      if parts.length == 2
        start_val = parts[0].to_i
        end_val = parts[1].to_i
        ranges << [start_val, end_val]
      end
    end
  end

  total = 0
  ranges.each do |start_val, end_val|
    (start_val..end_val).each do |num|
      total += num if is_invalid_id_part2(num)
    end
  end

  total
end

puts "Part 1: #{part1(input_text)}"
puts "Part 2: #{part2(input_text)}"
