#!/usr/bin/env ruby

def parse_input(filename)
  content = File.read(filename).strip

  content.split("\n\n").map do |group|
    group.split("\n").map(&:to_i).sum
  end
end

def part1(elves)
  elves.max
end

def part2(elves)
  elves.sort.reverse.take(3).sum
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  elves = parse_input(input_file)

  puts "Part 1: #{part1(elves)}"
  puts "Part 2: #{part2(elves)}"
end

main
