#!/usr/bin/env python3
import os
import json
from functools import cmp_to_key

def compare(left, right):
    """
    Compare two values recursively.
    Returns: -1 if left < right (correct order)
              1 if left > right (wrong order)
              0 if equal (continue)
    """
    # Both integers
    if isinstance(left, int) and isinstance(right, int):
        if left < right:
            return -1
        elif left > right:
            return 1
        return 0

    # Both lists
    if isinstance(left, list) and isinstance(right, list):
        for i in range(min(len(left), len(right))):
            result = compare(left[i], right[i])
            if result != 0:
                return result
        # Check lengths
        if len(left) < len(right):
            return -1
        elif len(left) > len(right):
            return 1
        return 0

    # Mixed types - convert integer to list
    if isinstance(left, int):
        return compare([left], right)
    else:
        return compare(left, [right])

def part1(text):
    """Sum indices of pairs in correct order."""
    pairs = text.strip().split('\n\n')
    total = 0

    for i, pair in enumerate(pairs, 1):
        lines = pair.strip().split('\n')
        left = json.loads(lines[0])
        right = json.loads(lines[1])

        if compare(left, right) == -1:
            total += i

    return total

def part2(text):
    """Sort all packets with divider packets, find decoder key."""
    lines = [line for line in text.strip().split('\n') if line]
    packets = [json.loads(line) for line in lines]

    # Add divider packets
    divider1 = [[2]]
    divider2 = [[6]]
    packets.append(divider1)
    packets.append(divider2)

    # Sort using comparison function
    packets.sort(key=cmp_to_key(compare))

    # Find positions of dividers (1-indexed)
    pos1 = packets.index(divider1) + 1
    pos2 = packets.index(divider2) + 1

    return pos1 * pos2

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        text = f.read()

    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
