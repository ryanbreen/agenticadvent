#!/usr/bin/env ruby

def parse_input(text)
  equations = []
  text.strip.split("\n").each do |line|
    target, nums = line.split(': ')
    equations << [target.to_i, nums.split.map(&:to_i)]
  end
  equations
end

def evaluate(nums, ops)
  """Evaluate left-to-right with given operators."""
  result = nums[0]
  ops.each_with_index do |op, i|
    case op
    when '+'
      result += nums[i + 1]
    when '*'
      result *= nums[i + 1]
    when '||'
      result = (result.to_s + nums[i + 1].to_s).to_i
    end
  end
  result
end

def can_make_target(target, nums, operators)
  """Check if any combination of operators can produce target."""
  n_ops = nums.length - 1
  operators.repeated_permutation(n_ops).each do |ops|
    return true if evaluate(nums, ops) == target
  end
  false
end

def part1(equations)
  operators = ['+', '*']
  total = 0
  equations.each do |target, nums|
    total += target if can_make_target(target, nums, operators)
  end
  total
end

def part2(equations)
  operators = ['+', '*', '||']
  total = 0
  equations.each do |target, nums|
    total += target if can_make_target(target, nums, operators)
  end
  total
end

if __FILE__ == $0
  input_file = File.join(File.dirname(__FILE__), '..', 'input.txt')
  text = File.read(input_file)
  equations = parse_input(text)

  puts "Part 1: #{part1(equations)}"
  puts "Part 2: #{part2(equations)}"
end
