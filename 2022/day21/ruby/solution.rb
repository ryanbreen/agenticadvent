#!/usr/bin/env ruby

def parse_input(text)
  monkeys = {}
  text.strip.split("\n").each do |line|
    name, job = line.split(': ')
    parts = job.split
    if parts.length == 1
      monkeys[name] = parts[0].to_i
    else
      monkeys[name] = [parts[0], parts[1], parts[2]]
    end
  end
  monkeys
end

def evaluate(monkeys, name, memo = {})
  return memo[name] if memo.key?(name)

  job = monkeys[name]
  if job.is_a?(Integer)
    return job
  end

  left, op, right = job
  left_val = evaluate(monkeys, left, memo)
  right_val = evaluate(monkeys, right, memo)

  result = case op
           when '+' then left_val + right_val
           when '-' then left_val - right_val
           when '*' then left_val * right_val
           when '/' then left_val / right_val
           end

  memo[name] = result
  result
end

def part1(text)
  monkeys = parse_input(text)
  evaluate(monkeys, 'root')
end

def contains_humn(monkeys, name, memo = {})
  return memo[name] if memo.key?(name)
  return true if name == 'humn'

  job = monkeys[name]
  if job.is_a?(Integer)
    memo[name] = false
    return false
  end

  left, _, right = job
  result = contains_humn(monkeys, left, memo) || contains_humn(monkeys, right, memo)
  memo[name] = result
  result
end

def solve_for_humn(monkeys, name, target)
  return target if name == 'humn'

  job = monkeys[name]
  return nil if job.is_a?(Integer)

  left, op, right = job

  left_has_humn = contains_humn(monkeys, left)

  if left_has_humn
    right_val = evaluate(monkeys, right)
    new_target = case op
                 when '+' then target - right_val
                 when '-' then target + right_val
                 when '*' then target / right_val
                 when '/' then target * right_val
                 end
    solve_for_humn(monkeys, left, new_target)
  else
    left_val = evaluate(monkeys, left)
    new_target = case op
                 when '+' then target - left_val
                 when '-' then left_val - target
                 when '*' then target / left_val
                 when '/' then left_val / target
                 end
    solve_for_humn(monkeys, right, new_target)
  end
end

def part2(text)
  monkeys = parse_input(text)

  left, _, right = monkeys['root']

  left_has_humn = contains_humn(monkeys, left)

  if left_has_humn
    target = evaluate(monkeys, right)
    solve_for_humn(monkeys, left, target)
  else
    target = evaluate(monkeys, left)
    solve_for_humn(monkeys, right, target)
  end
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

main
