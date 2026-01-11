#!/usr/bin/env ruby
# frozen_string_literal: true

def parse_input
  input_path = File.join(__dir__, '..', 'input.txt')
  content = File.read(input_path).strip

  sections = content.split("\n\n")
  numbers = sections[0].split(',').map(&:to_i)

  boards = sections[1..].map do |section|
    section.strip.split("\n").map do |line|
      line.split.map(&:to_i)
    end
  end

  [numbers, boards]
end

def check_winner(marked)
  # Check rows
  return true if (0...5).any? { |row| (0...5).all? { |col| marked[row][col] } }

  # Check columns
  return true if (0...5).any? { |col| (0...5).all? { |row| marked[row][col] } }

  false
end

def calculate_score(board, marked, last_number)
  unmarked_sum = 0
  (0...5).each do |row|
    (0...5).each do |col|
      unmarked_sum += board[row][col] unless marked[row][col]
    end
  end
  unmarked_sum * last_number
end

def mark_number(board, marked, number)
  (0...5).each do |row|
    (0...5).each do |col|
      marked[row][col] = true if board[row][col] == number
    end
  end
end

def part1(numbers, boards)
  marked = boards.map { Array.new(5) { Array.new(5, false) } }

  numbers.each do |number|
    boards.each_with_index do |board, i|
      mark_number(board, marked[i], number)
      return calculate_score(board, marked[i], number) if check_winner(marked[i])
    end
  end

  nil
end

def part2(numbers, boards)
  marked = boards.map { Array.new(5) { Array.new(5, false) } }
  won = Array.new(boards.length, false)
  last_score = nil

  numbers.each do |number|
    boards.each_with_index do |board, i|
      next if won[i]

      mark_number(board, marked[i], number)
      if check_winner(marked[i])
        won[i] = true
        last_score = calculate_score(board, marked[i], number)
      end
    end
  end

  last_score
end

numbers, boards = parse_input
puts "Part 1: #{part1(numbers, boards)}"
puts "Part 2: #{part2(numbers, boards)}"
