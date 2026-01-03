#!/usr/bin/env ruby
# Day 22: Sand Slabs - 3D falling bricks simulation

require 'set'

def parse_input(filename)
  bricks = []
  File.read(filename).strip.split("\n").each do |line|
    left, right = line.split('~')
    x1, y1, z1 = left.split(',').map(&:to_i)
    x2, y2, z2 = right.split(',').map(&:to_i)
    # Ensure z1 <= z2 for consistent processing
    if z1 > z2
      x1, y1, z1, x2, y2, z2 = x2, y2, z2, x1, y1, z1
    end
    bricks << [x1, y1, z1, x2, y2, z2]
  end
  bricks
end

def settle_bricks(bricks)
  # Sort by minimum z coordinate, keeping track of original indices
  sorted_indices = (0...bricks.size).sort_by { |i| [bricks[i][2], bricks[i][5]].min }

  # Track occupied cells: (x, y, z) -> brick index
  occupied = {}
  settled = Array.new(bricks.size)

  # supports[i] = set of brick indices that brick i supports (bricks above)
  # supporters[i] = set of brick indices that support brick i (bricks below)
  supports = Hash.new { |h, k| h[k] = Set.new }
  supporters = Hash.new { |h, k| h[k] = Set.new }

  sorted_indices.each do |orig_idx|
    brick = bricks[orig_idx]
    x1, y1, z1, x2, y2, z2 = brick

    # Find the maximum drop for this brick
    drop = z1 - 1  # Maximum possible drop (to z=1)

    # Get xy footprint of this brick and check how far it can fall
    ([x1, x2].min..[x1, x2].max).each do |x|
      ([y1, y2].min..[y1, y2].max).each do |y|
        # Check each z level below the brick
        (z1 - 1).downto(1) do |z|
          if occupied.key?([x, y, z])
            drop = [drop, z1 - z - 1].min
            break
          end
        end
      end
    end

    # Drop the brick
    new_z1 = z1 - drop
    new_z2 = z2 - drop
    new_brick = [x1, y1, new_z1, x2, y2, new_z2]
    settled[orig_idx] = new_brick

    # Mark cells as occupied and find supporters
    ([x1, x2].min..[x1, x2].max).each do |x|
      ([y1, y2].min..[y1, y2].max).each do |y|
        # Check if there's a brick directly below
        below_key = [x, y, new_z1 - 1]
        if occupied.key?(below_key)
          supporter_idx = occupied[below_key]
          supporters[orig_idx].add(supporter_idx)
          supports[supporter_idx].add(orig_idx)
        end

        # Mark all cells of this brick as occupied
        (new_z1..new_z2).each do |z|
          occupied[[x, y, z]] = orig_idx
        end
      end
    end
  end

  [settled, supports, supporters]
end

def part1(bricks)
  _settled, supports, supporters = settle_bricks(bricks)

  safe_count = 0
  bricks.size.times do |i|
    # Brick i can be safely removed if every brick it supports
    # has at least one other supporter
    can_remove = true
    supports[i].each do |supported|
      if supporters[supported].size == 1
        can_remove = false
        break
      end
    end
    safe_count += 1 if can_remove
  end

  safe_count
end

def part2(bricks)
  _settled, supports, supporters = settle_bricks(bricks)

  total_falls = 0

  bricks.size.times do |i|
    # Simulate removing brick i and count chain reaction using BFS
    falling = Set.new([i])
    queue = [i]

    until queue.empty?
      brick = queue.shift

      # Check all bricks that this brick supports
      supports[brick].each do |supported|
        next if falling.include?(supported)

        # This brick falls if all its supporters have fallen
        if supporters[supported].subset?(falling)
          falling.add(supported)
          queue << supported
        end
      end
    end

    # Don't count the initial brick we removed
    total_falls += falling.size - 1
  end

  total_falls
end

def main
  input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
  bricks = parse_input(input_path)
  puts "Part 1: #{part1(bricks)}"
  puts "Part 2: #{part2(bricks)}"
end

main if __FILE__ == $PROGRAM_NAME
