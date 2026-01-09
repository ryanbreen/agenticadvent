#!/usr/bin/env ruby
require 'json'

# Compare two values recursively.
# Returns: -1 if left < right (correct order)
#           1 if left > right (wrong order)
#           0 if equal (continue)
def compare(left, right)
  # Both integers
  if left.is_a?(Integer) && right.is_a?(Integer)
    return left <=> right
  end

  # Both arrays
  if left.is_a?(Array) && right.is_a?(Array)
    max_len = [left.length, right.length].min
    max_len.times do |i|
      result = compare(left[i], right[i])
      return result if result != 0
    end
    # Check lengths
    return left.length <=> right.length
  end

  # Mixed types - convert integer to array
  if left.is_a?(Integer)
    compare([left], right)
  else
    compare(left, [right])
  end
end

def part1(text)
  pairs = text.strip.split("\n\n")
  total = 0

  pairs.each_with_index do |pair, index|
    lines = pair.strip.split("\n")
    left = JSON.parse(lines[0])
    right = JSON.parse(lines[1])

    if compare(left, right) == -1
      total += index + 1  # 1-indexed
    end
  end

  total
end

def part2(text)
  lines = text.strip.split("\n").reject(&:empty?)
  packets = lines.map { |line| JSON.parse(line) }

  # Add divider packets
  divider1 = [[2]]
  divider2 = [[6]]
  packets << divider1
  packets << divider2

  # Sort using comparison function
  packets.sort! { |a, b| compare(a, b) }

  # Find positions of dividers (1-indexed)
  pos1 = packets.index(divider1) + 1
  pos2 = packets.index(divider2) + 1

  pos1 * pos2
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

main if __FILE__ == $PROGRAM_NAME
