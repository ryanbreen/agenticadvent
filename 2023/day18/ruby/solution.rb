#!/usr/bin/env ruby
# Day 18: Lavaduct Lagoon - Polygon area with Shoelace formula and Pick's theorem

def parse_input(filename)
  File.read(filename).strip.split("\n").map do |line|
    parts = line.split
    direction = parts[0]
    distance = parts[1].to_i
    color = parts[2][2..-2]  # Remove (# and )
    [direction, distance, color]
  end
end

def calculate_area(vertices, perimeter)
  # Shoelace formula for polygon area
  n = vertices.length
  area = 0

  n.times do |i|
    j = (i + 1) % n
    area += vertices[i][0] * vertices[j][1]
    area -= vertices[j][0] * vertices[i][1]
  end
  area = area.abs / 2

  # Total points = interior + boundary
  # From Pick's theorem: interior = area - boundary/2 + 1
  # Total = interior + boundary = area + boundary/2 + 1
  area + perimeter / 2 + 1
end

def part1(instructions)
  direction_map = {
    'R' => [0, 1],
    'D' => [1, 0],
    'L' => [0, -1],
    'U' => [-1, 0]
  }

  vertices = [[0, 0]]
  perimeter = 0
  r, c = 0, 0

  instructions.each do |direction, distance, _|
    dr, dc = direction_map[direction]
    r += dr * distance
    c += dc * distance
    vertices << [r, c]
    perimeter += distance
  end

  calculate_area(vertices, perimeter)
end

def part2(instructions)
  # Last digit of hex: 0=R, 1=D, 2=L, 3=U
  # First 5 digits: distance in hex
  direction_map = {
    '0' => [0, 1],   # R
    '1' => [1, 0],   # D
    '2' => [0, -1],  # L
    '3' => [-1, 0]   # U
  }

  vertices = [[0, 0]]
  perimeter = 0
  r, c = 0, 0

  instructions.each do |_, _, color|
    distance = color[0, 5].to_i(16)
    direction = color[5]
    dr, dc = direction_map[direction]
    r += dr * distance
    c += dc * distance
    vertices << [r, c]
    perimeter += distance
  end

  calculate_area(vertices, perimeter)
end

def main
  instructions = parse_input(File.join(__dir__, '..', 'input.txt'))
  puts "Part 1: #{part1(instructions)}"
  puts "Part 2: #{part2(instructions)}"
end

main if __FILE__ == $PROGRAM_NAME
