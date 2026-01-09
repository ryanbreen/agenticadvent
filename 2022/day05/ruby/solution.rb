#!/usr/bin/env ruby

def parse_input(filename)
  content = File.read(filename)
  parts = content.split("\n\n")
  stack_lines = parts[0].split("\n")
  move_lines = parts[1].strip.split("\n")

  # Find number of stacks from the last line (the numbers)
  num_stacks = stack_lines[-1].split.length

  # Parse stacks (bottom-up, excluding the number line)
  stacks = Array.new(num_stacks) { [] }
  stack_lines[0...-1].each do |line|
    num_stacks.times do |i|
      pos = 1 + i * 4  # Position of crate letter
      if pos < line.length && line[pos] != ' '
        stacks[i] << line[pos]
      end
    end
  end

  # Reverse so bottom is at index 0
  stacks.each(&:reverse!)

  # Parse moves
  moves = []
  move_lines.each do |line|
    if line =~ /move (\d+) from (\d+) to (\d+)/
      count = $1.to_i
      from_stack = $2.to_i - 1  # 0-indexed
      to_stack = $3.to_i - 1
      moves << [count, from_stack, to_stack]
    end
  end

  [stacks, moves]
end

def part1(stacks, moves)
  stacks = stacks.map(&:dup)
  moves.each do |count, from_stack, to_stack|
    count.times do
      crate = stacks[from_stack].pop
      stacks[to_stack] << crate
    end
  end
  stacks.map { |stack| stack.last }.join
end

def part2(stacks, moves)
  stacks = stacks.map(&:dup)
  moves.each do |count, from_stack, to_stack|
    # Move multiple crates at once (preserve order)
    crates = stacks[from_stack].pop(count)
    stacks[to_stack].concat(crates)
  end
  stacks.map { |stack| stack.last }.join
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  stacks, moves = parse_input(input_file)

  puts "Part 1: #{part1(stacks, moves)}"
  puts "Part 2: #{part2(stacks, moves)}"
end

main
