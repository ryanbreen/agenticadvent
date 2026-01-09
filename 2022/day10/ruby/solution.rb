#!/usr/bin/env ruby
# frozen_string_literal: true

require 'set'

def simulate_cpu(instructions)
  return enum_for(:simulate_cpu, instructions) unless block_given?

  x = 1
  cycle = 0

  instructions.each do |line|
    if line == 'noop'
      cycle += 1
      yield cycle, x
    else
      v = line.split[1].to_i
      cycle += 1
      yield cycle, x
      cycle += 1
      yield cycle, x
      x += v
    end
  end
end

def part1(instructions)
  target_cycles = [20, 60, 100, 140, 180, 220].to_set
  total = 0

  simulate_cpu(instructions) do |cycle, x|
    total += cycle * x if target_cycles.include?(cycle)
  end

  total
end

def part2(instructions)
  screen = []
  row = []

  simulate_cpu(instructions) do |cycle, x|
    pos = (cycle - 1) % 40
    row << ((pos - x).abs <= 1 ? '#' : '.')

    if cycle % 40 == 0
      screen << row.join
      row = []
    end
  end

  screen.join("\n")
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  instructions = File.read(input_file).strip.split("\n")

  puts "Part 1: #{part1(instructions)}"
  puts 'Part 2:'
  puts part2(instructions)
end

main if __FILE__ == $PROGRAM_NAME
