#!/usr/bin/env python3
import os

def parse_input(text):
    """Parse monkey definitions into a dictionary."""
    monkeys = {}
    for line in text.strip().split('\n'):
        name, job = line.split(': ')
        parts = job.split()
        if len(parts) == 1:
            monkeys[name] = int(parts[0])
        else:
            monkeys[name] = (parts[0], parts[1], parts[2])
    return monkeys

def evaluate(monkeys, name, memo=None):
    """Recursively evaluate a monkey's value."""
    if memo is None:
        memo = {}
    if name in memo:
        return memo[name]
    
    job = monkeys[name]
    if isinstance(job, int):
        return job
    
    left, op, right = job
    left_val = evaluate(monkeys, left, memo)
    right_val = evaluate(monkeys, right, memo)
    
    if op == '+':
        result = left_val + right_val
    elif op == '-':
        result = left_val - right_val
    elif op == '*':
        result = left_val * right_val
    elif op == '/':
        result = left_val // right_val
    
    memo[name] = result
    return result

def part1(text):
    """Evaluate the root monkey."""
    monkeys = parse_input(text)
    return evaluate(monkeys, 'root')

def contains_humn(monkeys, name, memo=None):
    """Check if evaluation tree contains 'humn'."""
    if memo is None:
        memo = {}
    if name in memo:
        return memo[name]
    if name == 'humn':
        return True
    
    job = monkeys[name]
    if isinstance(job, int):
        memo[name] = False
        return False
    
    left, _, right = job
    result = contains_humn(monkeys, left, memo) or contains_humn(monkeys, right, memo)
    memo[name] = result
    return result

def solve_for_humn(monkeys, name, target):
    """
    Given that 'name' should equal 'target', find what 'humn' should be.
    """
    if name == 'humn':
        return target
    
    job = monkeys[name]
    if isinstance(job, int):
        return None  # Can't solve if it's just a number
    
    left, op, right = job
    
    # Find which side contains humn
    left_has_humn = contains_humn(monkeys, left)
    
    if left_has_humn:
        # Evaluate right side to get its value
        right_val = evaluate(monkeys, right)
        # Solve for left
        if op == '+':
            # left + right = target => left = target - right
            new_target = target - right_val
        elif op == '-':
            # left - right = target => left = target + right
            new_target = target + right_val
        elif op == '*':
            # left * right = target => left = target / right
            new_target = target // right_val
        elif op == '/':
            # left / right = target => left = target * right
            new_target = target * right_val
        return solve_for_humn(monkeys, left, new_target)
    else:
        # Evaluate left side to get its value
        left_val = evaluate(monkeys, left)
        # Solve for right
        if op == '+':
            # left + right = target => right = target - left
            new_target = target - left_val
        elif op == '-':
            # left - right = target => right = left - target
            new_target = left_val - target
        elif op == '*':
            # left * right = target => right = target / left
            new_target = target // left_val
        elif op == '/':
            # left / right = target => right = left / target
            new_target = left_val // target
        return solve_for_humn(monkeys, right, new_target)

def part2(text):
    """Find what humn needs to yell for root's two values to be equal."""
    monkeys = parse_input(text)
    
    # root checks equality between its two children
    left, _, right = monkeys['root']
    
    # Find which side contains humn
    left_has_humn = contains_humn(monkeys, left)
    
    if left_has_humn:
        # Right side gives us the target value
        target = evaluate(monkeys, right)
        return solve_for_humn(monkeys, left, target)
    else:
        # Left side gives us the target value
        target = evaluate(monkeys, left)
        return solve_for_humn(monkeys, right, target)

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')
    
    with open(input_file) as f:
        text = f.read()
    
    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
