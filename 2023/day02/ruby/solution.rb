#!/usr/bin/env ruby

# Read input from ../input.txt
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
input = File.read(input_path).strip

lines = input.split("\n")

def part1(lines)
  # Limits for Part 1: 12 red, 13 green, 14 blue
  max_red = 12
  max_green = 13
  max_blue = 14

  sum_of_ids = 0

  lines.each do |line|
    # Parse game ID
    game_part, draws_part = line.split(': ')
    game_id = game_part.match(/Game (\d+)/)[1].to_i

    # Parse all draws (separated by semicolons)
    draws = draws_part.split('; ')

    # Check if game is possible
    possible = true

    draws.each do |draw|
      # Parse individual cube counts (separated by commas)
      cubes = draw.split(', ')

      cubes.each do |cube|
        count, color = cube.split(' ')
        count = count.to_i

        # Check if this draw exceeds limits
        if color == 'red' && count > max_red
          possible = false
          break
        elsif color == 'green' && count > max_green
          possible = false
          break
        elsif color == 'blue' && count > max_blue
          possible = false
          break
        end
      end

      break unless possible
    end

    sum_of_ids += game_id if possible
  end

  sum_of_ids
end

def part2(lines)
  total_power = 0

  lines.each do |line|
    # Parse game ID and draws
    game_part, draws_part = line.split(': ')

    # Parse all draws (separated by semicolons)
    draws = draws_part.split('; ')

    # Track minimum cubes needed for each color
    min_red = 0
    min_green = 0
    min_blue = 0

    draws.each do |draw|
      # Parse individual cube counts (separated by commas)
      cubes = draw.split(', ')

      cubes.each do |cube|
        count, color = cube.split(' ')
        count = count.to_i

        # Track the maximum seen for each color (minimum needed)
        if color == 'red'
          min_red = [min_red, count].max
        elsif color == 'green'
          min_green = [min_green, count].max
        elsif color == 'blue'
          min_blue = [min_blue, count].max
        end
      end
    end

    # Calculate power (product of minimum cubes)
    power = min_red * min_green * min_blue
    total_power += power
  end

  total_power
end

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
