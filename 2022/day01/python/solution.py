#!/usr/bin/env python3
import os

def parse_input(filename):
    """Parse input into list of calorie totals per elf."""
    with open(filename) as f:
        content = f.read().strip()

    elves = []
    for group in content.split('\n\n'):
        total = sum(int(line) for line in group.split('\n') if line)
        elves.append(total)

    return elves

def part1(elves):
    """Find the Elf carrying the most Calories."""
    return max(elves)

def part2(elves):
    """Find total calories carried by top three Elves."""
    sorted_elves = sorted(elves, reverse=True)
    return sum(sorted_elves[:3])

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    elves = parse_input(input_file)

    print('Part 1:', part1(elves))
    print('Part 2:', part2(elves))

if __name__ == '__main__':
    main()
