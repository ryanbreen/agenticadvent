#!/usr/bin/env python3
import sys
from pathlib import Path

def parse_input(text):
    """Parse input into list of patterns (each pattern is a list of strings)."""
    blocks = text.strip().split('\n\n')
    return [block.split('\n') for block in blocks]

def find_vertical_reflection(pattern):
    """Find vertical line of reflection. Returns columns to the left, or 0 if none."""
    if not pattern:
        return 0
    width = len(pattern[0])

    for col in range(1, width):
        # Check if reflection exists at this column
        is_reflection = True
        for row in pattern:
            # Compare left side with right side (mirrored)
            left = row[:col][::-1]  # reverse left side
            right = row[col:]
            # Compare the overlapping parts
            min_len = min(len(left), len(right))
            if left[:min_len] != right[:min_len]:
                is_reflection = False
                break
        if is_reflection:
            return col
    return 0

def find_horizontal_reflection(pattern):
    """Find horizontal line of reflection. Returns rows above, or 0 if none."""
    if not pattern:
        return 0
    height = len(pattern)

    for row in range(1, height):
        # Check if reflection exists at this row
        is_reflection = True
        # Compare top with bottom (mirrored)
        top = pattern[:row][::-1]  # reverse top side
        bottom = pattern[row:]
        min_len = min(len(top), len(bottom))
        for i in range(min_len):
            if top[i] != bottom[i]:
                is_reflection = False
                break
        if is_reflection:
            return row
    return 0

def summarize_pattern(pattern):
    """Get the summary value for a pattern."""
    v = find_vertical_reflection(pattern)
    if v > 0:
        return v
    h = find_horizontal_reflection(pattern)
    return h * 100

def part1(patterns):
    """Calculate the sum of all pattern summaries."""
    return sum(summarize_pattern(p) for p in patterns)

def count_differences(s1, s2):
    """Count character differences between two strings."""
    return sum(c1 != c2 for c1, c2 in zip(s1, s2))

def find_vertical_reflection_with_smudge(pattern):
    """Find vertical line with exactly one smudge fix needed."""
    if not pattern:
        return 0
    width = len(pattern[0])

    for col in range(1, width):
        total_diff = 0
        for row in pattern:
            left = row[:col][::-1]
            right = row[col:]
            min_len = min(len(left), len(right))
            total_diff += count_differences(left[:min_len], right[:min_len])
            if total_diff > 1:
                break
        if total_diff == 1:
            return col
    return 0

def find_horizontal_reflection_with_smudge(pattern):
    """Find horizontal line with exactly one smudge fix needed."""
    if not pattern:
        return 0
    height = len(pattern)

    for row in range(1, height):
        total_diff = 0
        top = pattern[:row][::-1]
        bottom = pattern[row:]
        min_len = min(len(top), len(bottom))
        for i in range(min_len):
            total_diff += count_differences(top[i], bottom[i])
            if total_diff > 1:
                break
        if total_diff == 1:
            return row
    return 0

def summarize_pattern_with_smudge(pattern):
    """Get the summary value for a pattern with smudge fix."""
    v = find_vertical_reflection_with_smudge(pattern)
    if v > 0:
        return v
    h = find_horizontal_reflection_with_smudge(pattern)
    return h * 100

def part2(patterns):
    """Calculate the sum with smudge fixes."""
    return sum(summarize_pattern_with_smudge(p) for p in patterns)

def main():
    input_file = Path(__file__).parent.parent / 'input.txt'
    text = input_file.read_text()
    patterns = parse_input(text)

    print(f"Part 1: {part1(patterns)}")
    print(f"Part 2: {part2(patterns)}")

if __name__ == '__main__':
    main()
