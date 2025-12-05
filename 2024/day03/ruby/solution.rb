#!/usr/bin/env ruby

def part1(data)
  # Find all valid mul(X,Y) instructions and sum their products
  pattern = /mul\((\d{1,3}),(\d{1,3})\)/
  matches = data.scan(pattern)
  matches.sum { |x, y| x.to_i * y.to_i }
end

def part2(data)
  # Like part1, but do() enables and don't() disables mul instructions
  mul_pattern = /mul\((\d{1,3}),(\d{1,3})\)/
  do_pattern = /do\(\)/
  dont_pattern = /don't\(\)/

  total = 0
  enabled = true

  # Build a list of all events with positions
  events = []

  # Find all mul instructions with their positions
  data.to_enum(:scan, mul_pattern).each do
    match = Regexp.last_match
    events << [match.begin(0), 'mul', match[1].to_i, match[2].to_i]
  end

  # Find all do() instructions with their positions
  data.to_enum(:scan, do_pattern).each do
    pos = Regexp.last_match.begin(0)
    events << [pos, 'do', 0, 0]
  end

  # Find all don't() instructions with their positions
  data.to_enum(:scan, dont_pattern).each do
    pos = Regexp.last_match.begin(0)
    events << [pos, 'dont', 0, 0]
  end

  # Sort by position and process
  events.sort_by! { |e| e[0] }

  events.each do |pos, event_type, x, y|
    case event_type
    when 'do'
      enabled = true
    when 'dont'
      enabled = false
    when 'mul'
      total += x * y if enabled
    end
  end

  total
end

def main
  input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
  data = File.read(input_path)

  puts "Part 1: #{part1(data)}"
  puts "Part 2: #{part2(data)}"
end

main if __FILE__ == $PROGRAM_NAME
