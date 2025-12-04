#!/usr/bin/env ruby

# Read input file
input_text = File.read(File.join(__dir__, '..', 'input.txt')).strip
lines = input_text.split("\n")

def part1(lines)
  position = 50  # Starting position
  zero_count = 0

  lines.each do |line|
    next if line.empty?  # Skip empty lines

    direction = line[0]
    distance = line[1..].to_i

    if direction == 'L'
      position = (position - distance) % 100
    else  # direction == 'R'
      position = (position + distance) % 100
    end

    zero_count += 1 if position == 0
  end

  zero_count
end

def part2(lines)
  position = 50  # Starting position
  zero_count = 0

  lines.each do |line|
    next if line.empty?  # Skip empty lines

    direction = line[0]
    distance = line[1..].to_i

    if direction == 'L'
      # Moving left (toward lower numbers)
      # We hit 0 after exactly 'position' steps, then every 100 steps after that
      if position > 0 && distance >= position
        zero_count += 1 + (distance - position) / 100
      elsif position == 0 && distance >= 100
        # Starting from 0, we hit it again after 100 steps, then every 100 steps
        zero_count += distance / 100
      end
    else  # direction == 'R'
      # Moving right (toward higher numbers)
      # We hit 0 after (100 - position) steps, then every 100 steps after that
      if position > 0
        steps_to_zero = 100 - position
        if distance >= steps_to_zero
          zero_count += 1 + (distance - steps_to_zero) / 100
        end
      else  # position == 0
        # Starting from 0, we hit it again after 100 steps, then every 100 steps
        zero_count += distance / 100 if distance >= 100
      end
    end

    # Update position
    if direction == 'L'
      position = (position - distance) % 100
    else
      position = (position + distance) % 100
    end
  end

  zero_count
end

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
