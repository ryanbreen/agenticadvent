#!/usr/bin/env ruby
# Day 13: Claw Contraption

require 'pathname'

# Read input file
input_path = Pathname.new(__FILE__).parent.parent / "input.txt"
input_text = File.read(input_path).strip

def parse_machines(text)
  """Parse claw machine configurations."""
  machines = []
  blocks = text.split("\n\n")

  blocks.each do |block|
    lines = block.strip.split("\n")

    # Button A: X+ax, Y+ay
    if lines[0] =~ /Button A: X\+(\d+), Y\+(\d+)/
      ax, ay = $1.to_i, $2.to_i
    end

    # Button B: X+bx, Y+by
    if lines[1] =~ /Button B: X\+(\d+), Y\+(\d+)/
      bx, by = $1.to_i, $2.to_i
    end

    # Prize: X=px, Y=py
    if lines[2] =~ /Prize: X=(\d+), Y=(\d+)/
      px, py = $1.to_i, $2.to_i
    end

    machines << [ax, ay, bx, by, px, py]
  end

  machines
end

def solve_machine(ax, ay, bx, by, px, py, max_presses = nil)
  """
  Solve for button presses using Cramer's rule.

  System of equations:
    a*ax + b*bx = px
    a*ay + b*by = py

  Solution:
    det = ax*by - ay*bx
    a = (px*by - py*bx) / det
    b = (ax*py - ay*px) / det

  Returns token cost (3*a + b) or nil if no valid solution.
  """
  det = ax * by - ay * bx

  # No unique solution
  return nil if det == 0

  # Calculate using integer arithmetic
  a_num = px * by - py * bx
  b_num = ax * py - ay * px

  # Check if solutions are integers
  return nil if a_num % det != 0 || b_num % det != 0

  a = a_num / det
  b = b_num / det

  # Check non-negative
  return nil if a < 0 || b < 0

  # Check max presses constraint (Part 1)
  if max_presses != nil && (a > max_presses || b > max_presses)
    return nil
  end

  3 * a + b
end

def part1(input_text)
  """Part 1: Max 100 presses per button."""
  machines = parse_machines(input_text)
  total = 0

  machines.each do |machine|
    cost = solve_machine(*machine, 100)
    total += cost if cost != nil
  end

  total
end

def part2(input_text)
  """Part 2: Prize coordinates shifted by 10^13, no press limit."""
  machines = parse_machines(input_text)
  offset = 10_000_000_000_000
  total = 0

  machines.each do |ax, ay, bx, by, px, py|
    # Shift prize coordinates
    cost = solve_machine(ax, ay, bx, by, px + offset, py + offset, nil)
    total += cost if cost != nil
  end

  total
end

if __FILE__ == $0
  puts "Part 1: #{part1(input_text)}"
  puts "Part 2: #{part2(input_text)}"
end
