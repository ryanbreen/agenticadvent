#!/usr/bin/env python3
import os
import re
import copy

def parse_input(filename):
    with open(filename) as f:
        content = f.read()

    parts = content.split('\n\n')
    stack_lines = parts[0].split('\n')
    move_lines = parts[1].strip().split('\n')

    # Find number of stacks from the last line (the numbers)
    num_stacks = len(stack_lines[-1].split())

    # Parse stacks (bottom-up, excluding the number line)
    stacks = [[] for _ in range(num_stacks)]
    for line in stack_lines[:-1]:
        for i in range(num_stacks):
            pos = 1 + i * 4  # Position of crate letter
            if pos < len(line) and line[pos] != ' ':
                stacks[i].append(line[pos])

    # Reverse so bottom is at index 0
    for stack in stacks:
        stack.reverse()

    # Parse moves
    moves = []
    for line in move_lines:
        match = re.match(r'move (\d+) from (\d+) to (\d+)', line)
        if match:
            count = int(match.group(1))
            from_stack = int(match.group(2)) - 1  # 0-indexed
            to_stack = int(match.group(3)) - 1
            moves.append((count, from_stack, to_stack))

    return stacks, moves

def part1(stacks, moves):
    stacks = copy.deepcopy(stacks)
    for count, from_stack, to_stack in moves:
        for _ in range(count):
            crate = stacks[from_stack].pop()
            stacks[to_stack].append(crate)
    return ''.join(stack[-1] for stack in stacks if stack)

def part2(stacks, moves):
    stacks = copy.deepcopy(stacks)
    for count, from_stack, to_stack in moves:
        # Move multiple crates at once (preserve order)
        crates = stacks[from_stack][-count:]
        stacks[from_stack] = stacks[from_stack][:-count]
        stacks[to_stack].extend(crates)
    return ''.join(stack[-1] for stack in stacks if stack)

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    stacks, moves = parse_input(input_file)

    print('Part 1:', part1(stacks, moves))
    print('Part 2:', part2(stacks, moves))

if __name__ == '__main__':
    main()
