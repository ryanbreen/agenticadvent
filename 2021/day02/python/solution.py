#!/usr/bin/env python3
import os

def parse_input():
    input_path = os.path.join(os.path.dirname(__file__), '..', 'input.txt')
    with open(input_path) as f:
        commands = []
        for line in f:
            line = line.strip()
            if line:
                parts = line.split()
                commands.append((parts[0], int(parts[1])))
        return commands

def part1(commands):
    horizontal = 0
    depth = 0
    for cmd, val in commands:
        if cmd == 'forward':
            horizontal += val
        elif cmd == 'down':
            depth += val
        elif cmd == 'up':
            depth -= val
    return horizontal * depth

def part2(commands):
    horizontal = 0
    depth = 0
    aim = 0
    for cmd, val in commands:
        if cmd == 'forward':
            horizontal += val
            depth += aim * val
        elif cmd == 'down':
            aim += val
        elif cmd == 'up':
            aim -= val
    return horizontal * depth

if __name__ == '__main__':
    commands = parse_input()
    print(f"Part 1: {part1(commands)}")
    print(f"Part 2: {part2(commands)}")
