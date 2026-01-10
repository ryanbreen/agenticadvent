#!/usr/bin/env ruby
# frozen_string_literal: true

def parse_input
  input_path = File.join(__dir__, '..', 'input.txt')
  File.readlines(input_path).map do |line|
    parts = line.strip.split
    [parts[0], parts[1].to_i]
  end
end

def part1(commands)
  horizontal = 0
  depth = 0

  commands.each do |cmd, val|
    case cmd
    when 'forward'
      horizontal += val
    when 'down'
      depth += val
    when 'up'
      depth -= val
    end
  end

  horizontal * depth
end

def part2(commands)
  horizontal = 0
  depth = 0
  aim = 0

  commands.each do |cmd, val|
    case cmd
    when 'forward'
      horizontal += val
      depth += aim * val
    when 'down'
      aim += val
    when 'up'
      aim -= val
    end
  end

  horizontal * depth
end

commands = parse_input
puts "Part 1: #{part1(commands)}"
puts "Part 2: #{part2(commands)}"
