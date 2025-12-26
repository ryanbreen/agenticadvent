#!/usr/bin/env python3
"""Day 25: Code Chronicle - Lock and key matching"""

import sys
from pathlib import Path


def parse_input(text):
    """Parse schematics into locks and keys."""
    locks = []
    keys = []

    schematics = text.strip().split('\n\n')

    for schematic in schematics:
        lines = schematic.strip().split('\n')

        # Lock: top row is all #, bottom is all .
        # Key: top row is all ., bottom is all #
        if lines[0] == '#####':
            # It's a lock - count # from top (excluding top row)
            heights = []
            for col in range(5):
                height = 0
                for row in range(1, 7):  # rows 1-6
                    if lines[row][col] == '#':
                        height += 1
                    else:
                        break
                heights.append(height)
            locks.append(heights)
        else:
            # It's a key - count # from bottom (excluding bottom row)
            heights = []
            for col in range(5):
                height = 0
                for row in range(5, -1, -1):  # rows 5 down to 0
                    if lines[row][col] == '#':
                        height += 1
                    else:
                        break
                heights.append(height)
            keys.append(heights)

    return locks, keys


def fits(lock, key):
    """Check if a key fits a lock (no column exceeds 5)."""
    for i in range(5):
        if lock[i] + key[i] > 5:
            return False
    return True


def part1(locks, keys):
    """Count unique lock/key pairs that fit together."""
    count = 0
    for lock in locks:
        for key in keys:
            if fits(lock, key):
                count += 1
    return count


def main():
    input_file = Path(__file__).parent.parent / "input.txt"
    text = input_file.read_text()

    locks, keys = parse_input(text)

    answer1 = part1(locks, keys)
    print(f"Part 1: {answer1}")

    # Day 25 typically only has Part 1
    print("Part 2: Merry Christmas! 🎄")


if __name__ == "__main__":
    main()
