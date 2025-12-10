#!/usr/bin/env python3

from solution import parse_line_part2, solve_machine_part2
from pathlib import Path

input_file = Path(__file__).parent.parent / 'input.txt'
lines = input_file.read_text().strip().split('\n')

print("Testing first 5 machines:")
for i, line in enumerate(lines[:5], 1):
    n_counters, joltage, buttons = parse_line_part2(line)
    result = solve_machine_part2(n_counters, joltage, buttons)
    print(f"Machine {i}: {result}")

print("\nTotal for first 5:")
print(sum(solve_machine_part2(*parse_line_part2(line)) for line in lines[:5]))
