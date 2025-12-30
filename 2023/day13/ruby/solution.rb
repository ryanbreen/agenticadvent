#!/usr/bin/env ruby

def parse_input(text)
  text.strip.split("\n\n").map { |block| block.split("\n") }
end

def count_differences(str1, str2)
  str1.chars.zip(str2.chars).count { |c1, c2| c1 != c2 }
end

def find_vertical_reflection(pattern, target_diff:)
  return 0 if pattern.empty?

  width = pattern.first.length

  (1...width).find do |col|
    total_diff = pattern.sum do |row|
      left = row[0...col].reverse
      right = row[col..]
      min_len = [left.length, right.length].min
      count_differences(left[0...min_len], right[0...min_len])
    end
    total_diff == target_diff
  end || 0
end

def find_horizontal_reflection(pattern, target_diff:)
  return 0 if pattern.empty?

  (1...pattern.length).find do |row|
    top = pattern[0...row].reverse
    bottom = pattern[row..]
    compare_len = [top.length, bottom.length].min

    total_diff = top.take(compare_len).zip(bottom.take(compare_len)).sum do |top_row, bottom_row|
      count_differences(top_row, bottom_row)
    end

    total_diff == target_diff
  end || 0
end

def summarize_pattern(pattern, target_diff:)
  vertical = find_vertical_reflection(pattern, target_diff: target_diff)
  return vertical if vertical > 0

  horizontal = find_horizontal_reflection(pattern, target_diff: target_diff)
  horizontal * 100
end

def part1(patterns)
  patterns.sum { |pattern| summarize_pattern(pattern, target_diff: 0) }
end

def part2(patterns)
  patterns.sum { |pattern| summarize_pattern(pattern, target_diff: 1) }
end

def main
  input_file = File.join(__dir__, '..', 'input.txt')
  text = File.read(input_file)
  patterns = parse_input(text)

  puts "Part 1: #{part1(patterns)}"
  puts "Part 2: #{part2(patterns)}"
end

main if __FILE__ == $PROGRAM_NAME
