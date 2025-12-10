#!/usr/bin/env python3

from solution import parse_line_part2, solve_machine_part2
from pathlib import Path

input_file = Path(__file__).parent.parent / 'input.txt'
lines = input_file.read_text().strip().split('\n')

print("Computing all machines:")
results = []
for i, line in enumerate(lines, 1):
    n_counters, joltage, buttons = parse_line_part2(line)
    result = solve_machine_part2(n_counters, joltage, buttons)
    results.append(result)
    if i == 183:
        print(f"Machine {i}: {result} (joltage={joltage}, buttons={buttons})")

print(f"\nTotal: {sum(results)}")
