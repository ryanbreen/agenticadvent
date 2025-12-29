#!/usr/bin/env python3
import sys
from pathlib import Path

def parse_input(text):
    """Parse the input into instructions and network."""
    lines = text.strip().split('\n')
    instructions = lines[0]

    network = {}
    for line in lines[2:]:
        if not line.strip():
            continue
        # Parse: AAA = (BBB, CCC)
        node, rest = line.split(' = ')
        left, right = rest[1:-1].split(', ')
        network[node] = (left, right)

    return instructions, network


def part1(instructions, network):
    """Navigate from AAA to ZZZ following L/R instructions."""
    current = 'AAA'
    steps = 0
    instruction_len = len(instructions)

    while current != 'ZZZ':
        instruction = instructions[steps % instruction_len]
        if instruction == 'L':
            current = network[current][0]
        else:
            current = network[current][1]
        steps += 1

    return steps


def part2(instructions, network):
    """Navigate all nodes ending in A simultaneously to nodes ending in Z."""
    # Find all starting nodes (ending in A)
    current_nodes = [node for node in network if node.endswith('A')]

    # For each starting node, find the cycle length to reach a Z node
    # This works because of the structure of the input - each starting node
    # reaches exactly one Z node in a regular cycle
    from math import gcd

    def lcm(a, b):
        return a * b // gcd(a, b)

    cycle_lengths = []
    instruction_len = len(instructions)

    for node in current_nodes:
        current = node
        steps = 0
        while not current.endswith('Z'):
            instruction = instructions[steps % instruction_len]
            if instruction == 'L':
                current = network[current][0]
            else:
                current = network[current][1]
            steps += 1
        cycle_lengths.append(steps)

    # Find LCM of all cycle lengths
    result = cycle_lengths[0]
    for length in cycle_lengths[1:]:
        result = lcm(result, length)

    return result


def main():
    input_path = Path(__file__).parent.parent / 'input.txt'
    text = input_path.read_text()

    instructions, network = parse_input(text)

    print("Part 1:", part1(instructions, network))
    print("Part 2:", part2(instructions, network))


if __name__ == '__main__':
    main()
