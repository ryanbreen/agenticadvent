#!/usr/bin/env python3
import sys
from pathlib import Path
from itertools import product

def parse_input(text):
    equations = []
    for line in text.strip().split('\n'):
        target, nums = line.split(': ')
        equations.append((int(target), list(map(int, nums.split()))))
    return equations

def evaluate(nums, ops):
    """Evaluate left-to-right with given operators."""
    result = nums[0]
    for i, op in enumerate(ops):
        if op == '+':
            result += nums[i + 1]
        elif op == '*':
            result *= nums[i + 1]
        elif op == '||':
            result = int(str(result) + str(nums[i + 1]))
    return result

def can_make_target(target, nums, operators):
    """Check if any combination of operators can produce target."""
    n_ops = len(nums) - 1
    for ops in product(operators, repeat=n_ops):
        if evaluate(nums, ops) == target:
            return True
    return False

def part1(equations):
    operators = ['+', '*']
    total = 0
    for target, nums in equations:
        if can_make_target(target, nums, operators):
            total += target
    return total

def part2(equations):
    operators = ['+', '*', '||']
    total = 0
    for target, nums in equations:
        if can_make_target(target, nums, operators):
            total += target
    return total

if __name__ == '__main__':
    input_file = Path(__file__).parent.parent / 'input.txt'
    text = input_file.read_text()
    equations = parse_input(text)

    print('Part 1:', part1(equations))
    print('Part 2:', part2(equations))
