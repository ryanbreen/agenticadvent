#!/usr/bin/env ruby

require 'set'

def parse_input(text)
  elves = Set.new
  text.strip.split("\n").each_with_index do |line, r|
    line.chars.each_with_index do |ch, c|
      elves.add([r, c]) if ch == '#'
    end
  end
  elves
end

def simulate_round(elves, directions)
  # Direction checks: direction => [[check positions], [move delta]]
  dir_checks = {
    'N' => [[[-1, -1], [-1, 0], [-1, 1]], [-1, 0]],
    'S' => [[[1, -1], [1, 0], [1, 1]], [1, 0]],
    'W' => [[[-1, -1], [0, -1], [1, -1]], [0, -1]],
    'E' => [[[-1, 1], [0, 1], [1, 1]], [0, 1]],
  }

  # All 8 neighbors
  all_neighbors = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]

  # Phase 1: Each elf proposes a move
  proposals = {}  # elf -> proposed position
  proposal_counts = Hash.new(0)  # position -> count

  elves.each do |elf|
    r, c = elf

    # Check if any neighbors
    has_neighbor = all_neighbors.any? { |dr, dc| elves.include?([r + dr, c + dc]) }

    next unless has_neighbor

    # Try each direction
    directions.each do |d|
      checks, delta = dir_checks[d]
      dr, dc = delta
      if checks.all? { |cr, cc| !elves.include?([r + cr, c + cc]) }
        new_pos = [r + dr, c + dc]
        proposals[elf] = new_pos
        proposal_counts[new_pos] += 1
        break
      end
    end
  end

  # Phase 2: Execute moves (only if unique proposal)
  new_elves = Set.new
  moved = false

  elves.each do |elf|
    if proposals.key?(elf)
      new_pos = proposals[elf]
      if proposal_counts[new_pos] == 1
        new_elves.add(new_pos)
        moved = true
      else
        new_elves.add(elf)
      end
    else
      new_elves.add(elf)
    end
  end

  [new_elves, moved]
end

def bounding_rect_empty(elves)
  min_r = elves.map { |r, _| r }.min
  max_r = elves.map { |r, _| r }.max
  min_c = elves.map { |_, c| c }.min
  max_c = elves.map { |_, c| c }.max

  area = (max_r - min_r + 1) * (max_c - min_c + 1)
  area - elves.size
end

def part1(text)
  elves = parse_input(text)
  directions = ['N', 'S', 'W', 'E']

  10.times do
    elves, _ = simulate_round(elves, directions)
    directions = directions[1..] + [directions[0]]
  end

  bounding_rect_empty(elves)
end

def part2(text)
  elves = parse_input(text)
  directions = ['N', 'S', 'W', 'E']

  round_num = 0
  loop do
    round_num += 1
    elves, moved = simulate_round(elves, directions)
    return round_num unless moved
    directions = directions[1..] + [directions[0]]
  end
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

main if __FILE__ == $PROGRAM_NAME
