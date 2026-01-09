#!/usr/bin/env ruby

require 'set'

DIRECTIONS = {
  'U' => [0, 1],
  'D' => [0, -1],
  'L' => [-1, 0],
  'R' => [1, 0]
}.freeze

def sign(x)
  x <=> 0
end

def move_tail(head, tail)
  dx = head[0] - tail[0]
  dy = head[1] - tail[1]

  # If adjacent or overlapping, don't move
  return tail if dx.abs <= 1 && dy.abs <= 1

  # Move toward head
  [tail[0] + sign(dx), tail[1] + sign(dy)]
end

def simulate_rope(moves, rope_length)
  knots = Array.new(rope_length) { [0, 0] }
  visited = Set.new([knots[-1].dup])

  moves.each do |line|
    direction, count = line.split
    count = count.to_i
    dx, dy = DIRECTIONS[direction]

    count.times do
      # Move head
      knots[0] = [knots[0][0] + dx, knots[0][1] + dy]

      # Move each subsequent knot
      (1...rope_length).each do |i|
        knots[i] = move_tail(knots[i - 1], knots[i])
      end

      visited.add(knots[-1].dup)
    end
  end

  visited.size
end

def part1(moves)
  simulate_rope(moves, 2)
end

def part2(moves)
  simulate_rope(moves, 10)
end

def main
  script_dir = File.dirname(File.absolute_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  moves = File.read(input_file).strip.split("\n")

  puts "Part 1: #{part1(moves)}"
  puts "Part 2: #{part2(moves)}"
end

main if __FILE__ == $PROGRAM_NAME
