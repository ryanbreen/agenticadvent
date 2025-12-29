#!/usr/bin/env ruby
# frozen_string_literal: true

def parse_input(text)
  lines = text.strip.split("\n")
  instructions = lines[0]

  network = {}
  lines[2..].each do |line|
    next if line.strip.empty?

    # Parse: AAA = (BBB, CCC)
    node, rest = line.split(' = ')
    left, right = rest[1..-2].split(', ')
    network[node] = [left, right]
  end

  [instructions, network]
end

DIR_INDEX = { 'L' => 0, 'R' => 1 }.freeze

def navigate_until(start, instructions, network, &goal_reached)
  current = start
  instructions.chars.cycle.with_index do |dir, steps|
    return steps if goal_reached.call(current)

    current = network[current][DIR_INDEX[dir]]
  end
end

def part1(instructions, network)
  navigate_until('AAA', instructions, network) { |node| node == 'ZZZ' }
end

def part2(instructions, network)
  # Find all starting nodes (ending in A)
  start_nodes = network.keys.select { |node| node.end_with?('A') }

  # For each starting node, find the cycle length to reach a Z node
  cycle_lengths = start_nodes.map do |node|
    navigate_until(node, instructions, network) { |n| n.end_with?('Z') }
  end

  # Find LCM of all cycle lengths
  cycle_lengths.reduce(1, :lcm)
end

def main
  input_path = File.join(__dir__, '..', 'input.txt')
  text = File.read(input_path)

  instructions, network = parse_input(text)

  puts "Part 1: #{part1(instructions, network)}"
  puts "Part 2: #{part2(instructions, network)}"
end

main
