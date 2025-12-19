#!/usr/bin/env ruby

input_path = File.join(File.dirname(__FILE__), "..", "input.txt")
input_text = File.read(input_path).strip

# Parse input
parts = input_text.split("\n\n")
patterns = parts[0].split(",").map(&:strip)
designs = parts[1].strip.split("\n")

def can_form(design, patterns)
  memo = {}

  dp = lambda do |pos|
    return true if pos == design.length
    return memo[pos] if memo.key?(pos)

    result = patterns.any? do |pattern|
      plen = pattern.length
      if design[pos, plen] == pattern
        dp.call(pos + plen)
      else
        false
      end
    end

    memo[pos] = result
  end

  dp.call(0)
end

def count_ways(design, patterns)
  memo = {}

  dp = lambda do |pos|
    return 1 if pos == design.length
    return memo[pos] if memo.key?(pos)

    total = 0
    patterns.each do |pattern|
      plen = pattern.length
      if design[pos, plen] == pattern
        total += dp.call(pos + plen)
      end
    end

    memo[pos] = total
  end

  dp.call(0)
end

def part1(designs, patterns)
  designs.count { |d| can_form(d, patterns) }
end

def part2(designs, patterns)
  designs.sum { |d| count_ways(d, patterns) }
end

puts "Part 1: #{part1(designs, patterns)}"
puts "Part 2: #{part2(designs, patterns)}"
