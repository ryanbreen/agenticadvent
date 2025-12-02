#!/usr/bin/env python3
"""
Advent of Code 2025 Day 2 Part 1
Find invalid product IDs (numbers that are a sequence repeated twice)
"""

def is_invalid_id(n):
    """Check if a number is invalid (some digit sequence repeated twice)"""
    s = str(n)
    length = len(s)

    # Can't be repeated if odd length
    if length % 2 != 0:
        return False

    # Check if first half equals second half
    mid = length // 2
    return s[:mid] == s[mid:]

def solve(input_text):
    """Solve the puzzle"""
    # Parse the ranges
    ranges_str = input_text.strip()
    ranges = []

    for range_str in ranges_str.split(','):
        range_str = range_str.strip()
        if not range_str:
            continue
        start, end = map(int, range_str.split('-'))
        ranges.append((start, end))

    # Find all invalid IDs
    total = 0
    invalid_count = 0

    for start, end in ranges:
        for num in range(start, end + 1):
            if is_invalid_id(num):
                total += num
                invalid_count += 1

    return total, invalid_count

def main():
    # Read input
    with open('/Users/wrb/fun/code/advent2025/day02/input.txt', 'r') as f:
        input_text = f.read()

    # Solve
    answer, count = solve(input_text)

    print(f"Found {count} invalid IDs")
    print(f"Part 1 Answer: {answer}")

if __name__ == '__main__':
    main()
