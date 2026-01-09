#!/usr/bin/env ruby
# frozen_string_literal: true

def parse_input(text)
  text.strip.split("\n").map(&:to_i)
end

def mix(numbers, times = 1)
  n = numbers.length
  # Store [original_index, value] pairs
  indexed = numbers.each_with_index.map { |val, idx| [idx, val] }

  times.times do
    n.times do |orig_idx|
      # Find current position of this element
      curr_pos = indexed.index { |idx, _| idx == orig_idx }

      # Remove from current position
      elem = indexed.delete_at(curr_pos)

      # Calculate new position (modulo n-1 because we removed the element)
      new_pos = (curr_pos + elem[1]) % (n - 1)

      # Insert at new position
      indexed.insert(new_pos, elem)
    end
  end

  indexed.map { |_, val| val }
end

def grove_coordinates(mixed)
  n = mixed.length
  zero_idx = mixed.index(0)
  [1000, 2000, 3000].sum { |offset| mixed[(zero_idx + offset) % n] }
end

def part1(text)
  numbers = parse_input(text)
  mixed = mix(numbers, 1)
  grove_coordinates(mixed)
end

def part2(text)
  numbers = parse_input(text)
  decryption_key = 811_589_153
  numbers = numbers.map { |n| n * decryption_key }
  mixed = mix(numbers, 10)
  grove_coordinates(mixed)
end

def main
  script_dir = File.dirname(File.absolute_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

main if __FILE__ == $PROGRAM_NAME
