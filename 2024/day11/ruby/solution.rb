#!/usr/bin/env ruby

# Read input from ../input.txt
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
input_text = File.read(input_path).strip

# Parse input - space-separated numbers
stones = input_text.split.map(&:to_i)

# Memoization cache for count_stones
$memo = {}

# Count how many stones result from a single stone after N blinks
def count_stones(value, blinks)
  # Base case: no more blinks
  return 1 if blinks == 0

  # Check memo cache
  key = [value, blinks]
  return $memo[key] if $memo.key?(key)

  result = if value == 0
    # Rule 1: 0 becomes 1
    count_stones(1, blinks - 1)
  else
    s = value.to_s
    if s.length.even?
      # Rule 2: Even number of digits -> split in half
      mid = s.length / 2
      left = s[0...mid].to_i
      right = s[mid..-1].to_i
      count_stones(left, blinks - 1) + count_stones(right, blinks - 1)
    else
      # Rule 3: Multiply by 2024
      count_stones(value * 2024, blinks - 1)
    end
  end

  $memo[key] = result
  result
end

def part1(stones)
  stones.sum { |stone| count_stones(stone, 25) }
end

def part2(stones)
  stones.sum { |stone| count_stones(stone, 75) }
end

puts "Part 1: #{part1(stones)}"
puts "Part 2: #{part2(stones)}"
