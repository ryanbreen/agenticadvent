#!/usr/bin/env ruby

require 'set'

# Read input
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
lines = File.read(input_path).strip.split("\n")

# Parse cards into arrays of [winning_set, have_set]
def parse_cards(lines)
  lines.map do |line|
    _, numbers = line.split(':')
    winning_part, have_part = numbers.split('|')
    winning = winning_part.split.map(&:to_i).to_set
    have = have_part.split.map(&:to_i).to_set
    [winning, have]
  end
end

def part1(cards)
  cards.sum do |winning, have|
    matches = (winning & have).size
    matches > 0 ? 2 ** (matches - 1) : 0
  end
end

def part2(cards)
  matches = cards.map { |winning, have| (winning & have).size }
  copies = [1] * cards.size

  matches.each_with_index do |m, i|
    (1..m).each do |offset|
      j = i + offset
      copies[j] += copies[i] if j < cards.size
    end
  end

  copies.sum
end

# Parse cards once and run both parts
cards = parse_cards(lines)
puts "Part 1: #{part1(cards)}"
puts "Part 2: #{part2(cards)}"
