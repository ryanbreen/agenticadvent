#!/usr/bin/env python3
"""Advent of Code 2023 Day 8: Haunted Wasteland"""
import itertools
import math
from pathlib import Path
from typing import Callable


def parse_input(text: str) -> tuple[str, dict[str, tuple[str, str]]]:
    """Parse the input into instructions and network."""
    lines = text.strip().split('\n')
    instructions = lines[0]

    network: dict[str, tuple[str, str]] = {}
    for line in lines[2:]:
        if not line.strip():
            continue
        # Parse: AAA = (BBB, CCC)
        node, rest = line.split(' = ')
        left, right = rest[1:-1].split(', ')
        network[node] = (left, right)

    return instructions, network


def navigate(
    start: str,
    instructions: str,
    network: dict[str, tuple[str, str]],
    is_end: Callable[[str], bool]
) -> int:
    """Navigate from start node until is_end condition is met, returning step count."""
    current = start
    for steps, instruction in enumerate(itertools.cycle(instructions)):
        if is_end(current):
            return steps
        current = network[current][0 if instruction == 'L' else 1]
    return 0  # unreachable, satisfies type checker


def part1(instructions: str, network: dict[str, tuple[str, str]]) -> int:
    """Navigate from AAA to ZZZ following L/R instructions."""
    return navigate('AAA', instructions, network, lambda n: n == 'ZZZ')


def part2(instructions: str, network: dict[str, tuple[str, str]]) -> int:
    """Navigate all nodes ending in A simultaneously to nodes ending in Z."""
    # Find all starting nodes (ending in A)
    starting_nodes = [node for node in network if node.endswith('A')]

    # For each starting node, find the cycle length to reach a Z node
    # This works because of the structure of the input - each starting node
    # reaches exactly one Z node in a regular cycle
    cycle_lengths = [
        navigate(node, instructions, network, lambda n: n.endswith('Z'))
        for node in starting_nodes
    ]

    # Find LCM of all cycle lengths
    return math.lcm(*cycle_lengths)


def main() -> None:
    input_path = Path(__file__).parent.parent / 'input.txt'
    text = input_path.read_text()

    instructions, network = parse_input(text)

    print("Part 1:", part1(instructions, network))
    print("Part 2:", part2(instructions, network))


if __name__ == '__main__':
    main()
