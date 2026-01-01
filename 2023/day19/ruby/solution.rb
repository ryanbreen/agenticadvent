#!/usr/bin/env ruby
# frozen_string_literal: true

# Day 19: Aplenty - Workflow processing and range analysis

def parse_input(filename)
  text = File.read(filename).strip
  workflow_section, parts_section = text.split("\n\n")

  # Parse workflows
  workflows = {}
  workflow_section.each_line do |line|
    line = line.strip
    name, rules_str = line.split('{')
    rules_str = rules_str.chomp('}')
    rules = rules_str.split(',').map do |rule|
      if rule.include?(':')
        condition, destination = rule.split(':')
        match = condition.match(/([xmas])([<>])(\d+)/)
        attr, op, value = match[1], match[2], match[3].to_i
        { attr: attr, op: op, value: value, destination: destination }
      else
        { attr: nil, op: nil, value: nil, destination: rule }
      end
    end
    workflows[name] = rules
  end

  # Parse parts
  parts = parts_section.each_line.map do |line|
    part = {}
    line.scan(/([xmas])=(\d+)/).each do |attr, value|
      part[attr] = value.to_i
    end
    part
  end

  [workflows, parts]
end

def process_part(workflows, part)
  current = 'in'

  until %w[A R].include?(current)
    workflows[current].each do |rule|
      if rule[:attr].nil?
        current = rule[:destination]
        break
      elsif rule[:op] == '<' && part[rule[:attr]] < rule[:value]
        current = rule[:destination]
        break
      elsif rule[:op] == '>' && part[rule[:attr]] > rule[:value]
        current = rule[:destination]
        break
      end
    end
  end

  current == 'A'
end

def part1(workflows, parts)
  parts.sum do |part|
    if process_part(workflows, part)
      part['x'] + part['m'] + part['a'] + part['s']
    else
      0
    end
  end
end

def count_accepted(workflows, workflow, ranges)
  return 0 if workflow == 'R'

  if workflow == 'A'
    return ranges.values.map { |lo, hi| [0, hi - lo + 1].max }.reduce(1, :*)
  end

  total = 0
  ranges = ranges.dup

  workflows[workflow].each do |rule|
    if rule[:attr].nil?
      total += count_accepted(workflows, rule[:destination], ranges)
    else
      attr = rule[:attr]
      lo, hi = ranges[attr]
      op = rule[:op]
      value = rule[:value]
      destination = rule[:destination]

      if op == '<'
        # Split: [lo, value-1] goes to destination, [value, hi] continues
        if lo < value
          new_ranges = ranges.dup
          new_ranges[attr] = [lo, [hi, value - 1].min]
          total += count_accepted(workflows, destination, new_ranges)
        end
        # Remaining range continues to next rule
        if hi >= value
          ranges[attr] = [[lo, value].max, hi]
        else
          break
        end
      else # op == '>'
        # Split: [value+1, hi] goes to destination, [lo, value] continues
        if hi > value
          new_ranges = ranges.dup
          new_ranges[attr] = [[lo, value + 1].max, hi]
          total += count_accepted(workflows, destination, new_ranges)
        end
        # Remaining range continues to next rule
        if lo <= value
          ranges[attr] = [lo, [hi, value].min]
        else
          break
        end
      end
    end
  end

  total
end

def part2(workflows)
  initial_ranges = {
    'x' => [1, 4000],
    'm' => [1, 4000],
    'a' => [1, 4000],
    's' => [1, 4000]
  }
  count_accepted(workflows, 'in', initial_ranges)
end

def main
  input_file = File.join(__dir__, '..', 'input.txt')
  workflows, parts = parse_input(input_file)
  puts "Part 1: #{part1(workflows, parts)}"
  puts "Part 2: #{part2(workflows)}"
end

main if __FILE__ == $PROGRAM_NAME
