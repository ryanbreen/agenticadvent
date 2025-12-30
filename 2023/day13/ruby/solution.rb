#!/usr/bin/env ruby

def parse_input(text)
  # Parse input into list of patterns (each pattern is an array of strings)
  text.strip.split("\n\n").map { |block| block.split("\n") }
end

def find_vertical_reflection(pattern)
  # Find vertical line of reflection. Returns columns to the left, or 0 if none.
  return 0 if pattern.empty?
  width = pattern[0].length

  (1...width).each do |col|
    # Check if reflection exists at this column
    is_reflection = true
    pattern.each do |row|
      # Compare left side with right side (mirrored)
      left = row[0...col].reverse
      right = row[col..]
      # Compare the overlapping parts
      min_len = [left.length, right.length].min
      if left[0...min_len] != right[0...min_len]
        is_reflection = false
        break
      end
    end
    return col if is_reflection
  end
  0
end

def find_horizontal_reflection(pattern)
  # Find horizontal line of reflection. Returns rows above, or 0 if none.
  return 0 if pattern.empty?
  height = pattern.length

  (1...height).each do |row|
    # Check if reflection exists at this row
    is_reflection = true
    # Compare top with bottom (mirrored)
    top = pattern[0...row].reverse
    bottom = pattern[row..]
    min_len = [top.length, bottom.length].min
    (0...min_len).each do |i|
      if top[i] != bottom[i]
        is_reflection = false
        break
      end
    end
    return row if is_reflection
  end
  0
end

def summarize_pattern(pattern)
  # Get the summary value for a pattern
  v = find_vertical_reflection(pattern)
  return v if v > 0
  h = find_horizontal_reflection(pattern)
  h * 100
end

def part1(patterns)
  # Calculate the sum of all pattern summaries
  patterns.sum { |p| summarize_pattern(p) }
end

def count_differences(s1, s2)
  # Count character differences between two strings
  s1.chars.zip(s2.chars).count { |c1, c2| c1 != c2 }
end

def find_vertical_reflection_with_smudge(pattern)
  # Find vertical line with exactly one smudge fix needed
  return 0 if pattern.empty?
  width = pattern[0].length

  (1...width).each do |col|
    total_diff = 0
    pattern.each do |row|
      left = row[0...col].reverse
      right = row[col..]
      min_len = [left.length, right.length].min
      total_diff += count_differences(left[0...min_len], right[0...min_len])
      break if total_diff > 1
    end
    return col if total_diff == 1
  end
  0
end

def find_horizontal_reflection_with_smudge(pattern)
  # Find horizontal line with exactly one smudge fix needed
  return 0 if pattern.empty?
  height = pattern.length

  (1...height).each do |row|
    total_diff = 0
    top = pattern[0...row].reverse
    bottom = pattern[row..]
    min_len = [top.length, bottom.length].min
    (0...min_len).each do |i|
      total_diff += count_differences(top[i], bottom[i])
      break if total_diff > 1
    end
    return row if total_diff == 1
  end
  0
end

def summarize_pattern_with_smudge(pattern)
  # Get the summary value for a pattern with smudge fix
  v = find_vertical_reflection_with_smudge(pattern)
  return v if v > 0
  h = find_horizontal_reflection_with_smudge(pattern)
  h * 100
end

def part2(patterns)
  # Calculate the sum with smudge fixes
  patterns.sum { |p| summarize_pattern_with_smudge(p) }
end

def main
  input_file = File.join(File.dirname(__FILE__), '..', 'input.txt')
  text = File.read(input_file)
  patterns = parse_input(text)

  puts "Part 1: #{part1(patterns)}"
  puts "Part 2: #{part2(patterns)}"
end

main if __FILE__ == $0
