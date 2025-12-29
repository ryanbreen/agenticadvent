#!/usr/bin/env python3
import sys
from itertools import combinations

def parse_grid(lines):
    """Parse the grid and return galaxy positions."""
    galaxies = []
    for r, line in enumerate(lines):
        for c, ch in enumerate(line):
            if ch == '#':
                galaxies.append((r, c))
    return galaxies

def find_empty_rows_and_cols(lines):
    """Find rows and columns that contain no galaxies."""
    rows = len(lines)
    cols = len(lines[0]) if lines else 0

    empty_rows = set()
    empty_cols = set()

    # Find empty rows
    for r, line in enumerate(lines):
        if '#' not in line:
            empty_rows.add(r)

    # Find empty columns
    for c in range(cols):
        if all(lines[r][c] != '#' for r in range(rows)):
            empty_cols.add(c)

    return empty_rows, empty_cols

def calculate_distances(galaxies, empty_rows, empty_cols, expansion_factor=2):
    """
    Calculate sum of Manhattan distances between all pairs of galaxies.
    Empty rows/columns are expanded by the expansion factor.
    """
    total = 0

    for (r1, c1), (r2, c2) in combinations(galaxies, 2):
        # Calculate row distance with expansion
        min_r, max_r = min(r1, r2), max(r1, r2)
        row_dist = max_r - min_r
        for r in range(min_r, max_r):
            if r in empty_rows:
                row_dist += expansion_factor - 1

        # Calculate column distance with expansion
        min_c, max_c = min(c1, c2), max(c1, c2)
        col_dist = max_c - min_c
        for c in range(min_c, max_c):
            if c in empty_cols:
                col_dist += expansion_factor - 1

        total += row_dist + col_dist

    return total

def part1(lines):
    """Solve Part 1 - expansion factor of 2."""
    galaxies = parse_grid(lines)
    empty_rows, empty_cols = find_empty_rows_and_cols(lines)
    return calculate_distances(galaxies, empty_rows, empty_cols, expansion_factor=2)

def part2(lines):
    """Solve Part 2 - expansion factor of 1,000,000."""
    galaxies = parse_grid(lines)
    empty_rows, empty_cols = find_empty_rows_and_cols(lines)
    return calculate_distances(galaxies, empty_rows, empty_cols, expansion_factor=1000000)

def main():
    input_file = sys.argv[1] if len(sys.argv) > 1 else '../input.txt'
    with open(input_file) as f:
        lines = [line.rstrip('\n') for line in f]

    # Remove any trailing empty lines
    while lines and not lines[-1]:
        lines.pop()

    print("Part 1:", part1(lines))
    print("Part 2:", part2(lines))

if __name__ == '__main__':
    main()
