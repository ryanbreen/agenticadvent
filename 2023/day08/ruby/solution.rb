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

def part1(instructions, network)
  current = 'AAA'
  steps = 0
  instruction_len = instructions.length

  while current != 'ZZZ'
    instruction = instructions[steps % instruction_len]
    current = instruction == 'L' ? network[current][0] : network[current][1]
    steps += 1
  end

  steps
end

def part2(instructions, network)
  # Find all starting nodes (ending in A)
  current_nodes = network.keys.select { |node| node.end_with?('A') }

  instruction_len = instructions.length

  # For each starting node, find the cycle length to reach a Z node
  cycle_lengths = current_nodes.map do |node|
    current = node
    steps = 0
    until current.end_with?('Z')
      instruction = instructions[steps % instruction_len]
      current = instruction == 'L' ? network[current][0] : network[current][1]
      steps += 1
    end
    steps
  end

  # Find LCM of all cycle lengths
  cycle_lengths.reduce(1) { |acc, length| acc.lcm(length) }
end

def main
  input_path = File.join(__dir__, '..', 'input.txt')
  text = File.read(input_path)

  instructions, network = parse_input(text)

  puts "Part 1: #{part1(instructions, network)}"
  puts "Part 2: #{part2(instructions, network)}"
end

main
