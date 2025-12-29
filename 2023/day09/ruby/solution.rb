# frozen_string_literal: true

input_path = File.join(__dir__, '..', 'input.txt')
input_text = File.read(input_path).strip

def parse_input(text)
  text.split("\n").map { |line| line.split.map(&:to_i) }
end

def get_differences(seq)
  seq.each_cons(2).map { |a, b| b - a }
end

def extrapolate_next(seq)
  sequences = [seq.dup]
  current = seq.dup

  until current.all?(&:zero?)
    current = get_differences(current)
    sequences << current
  end

  (sequences.length - 2).downto(0) do |i|
    sequences[i] << sequences[i].last + sequences[i + 1].last
  end

  sequences[0].last
end

def extrapolate_prev(seq)
  sequences = [seq.dup]
  current = seq.dup

  until current.all?(&:zero?)
    current = get_differences(current)
    sequences << current
  end

  (sequences.length - 2).downto(0) do |i|
    sequences[i].unshift(sequences[i].first - sequences[i + 1].first)
  end

  sequences[0].first
end

histories = parse_input(input_text)

def part1(histories)
  histories.sum { |h| extrapolate_next(h) }
end

def part2(histories)
  histories.sum { |h| extrapolate_prev(h) }
end

puts "Part 1: #{part1(histories)}"
puts "Part 2: #{part2(histories)}"
