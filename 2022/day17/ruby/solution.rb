#!/usr/bin/env ruby

# Rock shapes as list of [dx, dy] offsets from bottom-left
ROCKS = [
  [[0, 0], [1, 0], [2, 0], [3, 0]],           # Horizontal line
  [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]],   # Plus
  [[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]],   # L shape
  [[0, 0], [0, 1], [0, 2], [0, 3]],           # Vertical line
  [[0, 0], [1, 0], [0, 1], [1, 1]]            # Square
]

WIDTH = 7

def simulate(jets, num_rocks)
  occupied = Set.new
  height = 0
  jet_idx = 0

  # For cycle detection
  states = {}
  heights = []

  num_rocks.times do |rock_num|
    rock_type = rock_num % 5
    rock = ROCKS[rock_type]

    # Starting position: left edge at x=2, bottom at y=height+3
    x = 2
    y = height + 3

    loop do
      # Jet push
      jet = jets[jet_idx]
      jet_idx = (jet_idx + 1) % jets.length

      dx = jet == '>' ? 1 : -1

      # Check if can move horizontally
      can_move = rock.all? do |rx, ry|
        nx = x + rx + dx
        ny = y + ry
        nx >= 0 && nx < WIDTH && !occupied.include?([nx, ny])
      end

      x += dx if can_move

      # Fall down
      can_fall = rock.all? do |rx, ry|
        nx = x + rx
        ny = y + ry - 1
        ny >= 0 && !occupied.include?([nx, ny])
      end

      if can_fall
        y -= 1
      else
        # Rock stops
        rock.each do |rx, ry|
          occupied.add([x + rx, y + ry])
          height = [height, y + ry + 1].max
        end
        break
      end
    end

    heights << height

    # Cycle detection for Part 2
    if num_rocks > 10000
      # Create state key from surface profile
      profile_depth = 30
      profile = []

      WIDTH.times do |col|
        found = false
        profile_depth.times do |row|
          if occupied.include?([col, height - 1 - row])
            profile << [col, row]
            found = true
            break
          end
        end
        profile << [col, profile_depth] unless found
      end

      state = [rock_type, jet_idx, profile]

      if states.key?(state)
        # Found cycle
        cycle_start = states[state]
        cycle_len = rock_num - cycle_start
        cycle_height = height - heights[cycle_start]

        # Calculate final height
        remaining = num_rocks - rock_num - 1
        full_cycles = remaining / cycle_len
        leftover = remaining % cycle_len

        final_height = height + full_cycles * cycle_height
        if leftover > 0
          final_height += heights[cycle_start + leftover] - heights[cycle_start]
        end

        return final_height
      end

      states[state] = rock_num
    end
  end

  height
end

def part1(text)
  jets = text.strip
  simulate(jets, 2022)
end

def part2(text)
  jets = text.strip
  simulate(jets, 1_000_000_000_000)
end

def main
  script_dir = File.dirname(File.absolute_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

require 'set'
main
