#!/usr/bin/env ruby

require 'pathname'
require 'set'

# Read input file
input_path = Pathname.new(__FILE__).parent.parent / "input.txt"
input_text = input_path.read.strip

# Parse input - split into rules and updates sections
sections = input_text.split("\n\n")
rules_section = sections[0].split("\n")
updates_section = sections[1].split("\n")

# Parse rules: X|Y means X must come before Y
# Store as: rules[X] = set of pages that must come AFTER X
rules = Hash.new { |h, k| h[k] = Set.new }
rules_section.each do |rule|
  before, after = rule.split("|").map(&:to_i)
  rules[before].add(after)
end

# Parse updates
updates = updates_section.map { |line| line.split(",").map(&:to_i) }

def is_valid_order(update, rules)
  # Check if an update is in valid order according to rules.
  page_positions = {}
  update.each_with_index { |page, i| page_positions[page] = i }

  update.each_with_index do |page, i|
    # Check all pages that must come after this page
    if rules[page]
      rules[page].each do |must_be_after|
        if page_positions.key?(must_be_after)
          return false if page_positions[must_be_after] < i
        end
      end
    end
  end

  true
end

def fix_order(update, rules)
  # Reorder an update to satisfy all rules using a custom comparator.
  update.sort do |a, b|
    # If a must come before b, return -1
    if rules[a].include?(b)
      -1
    # If b must come before a, return 1
    elsif rules[b].include?(a)
      1
    else
      0
    end
  end
end

def part1(updates, rules)
  total = 0
  updates.each do |update|
    if is_valid_order(update, rules)
      middle_idx = update.length / 2
      total += update[middle_idx]
    end
  end
  total
end

def part2(updates, rules)
  total = 0
  updates.each do |update|
    unless is_valid_order(update, rules)
      fixed = fix_order(update, rules)
      middle_idx = fixed.length / 2
      total += fixed[middle_idx]
    end
  end
  total
end

# Main execution
puts "Part 1: #{part1(updates, rules)}"
puts "Part 2: #{part2(updates, rules)}"
