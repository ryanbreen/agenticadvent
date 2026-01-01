#!/usr/bin/env python3
"""Day 19: Aplenty - Workflow processing and range analysis."""

from pathlib import Path
import re


def parse_input(filename: str) -> tuple[dict, list[dict]]:
    """Parse workflows and parts from input."""
    text = Path(filename).read_text().strip()
    workflow_section, parts_section = text.split('\n\n')

    # Parse workflows
    workflows = {}
    for line in workflow_section.split('\n'):
        name, rules_str = line.split('{')
        rules_str = rules_str.rstrip('}')
        rules = []
        for rule in rules_str.split(','):
            if ':' in rule:
                condition, destination = rule.split(':')
                match = re.match(r'([xmas])([<>])(\d+)', condition)
                attr, op, value = match.groups()
                rules.append((attr, op, int(value), destination))
            else:
                rules.append((None, None, None, rule))  # Default rule
        workflows[name] = rules

    # Parse parts
    parts = []
    for line in parts_section.split('\n'):
        part = {}
        for match in re.finditer(r'([xmas])=(\d+)', line):
            part[match.group(1)] = int(match.group(2))
        parts.append(part)

    return workflows, parts


def process_part(workflows: dict, part: dict) -> bool:
    """Process a part through the workflows, return True if accepted."""
    current = 'in'

    while current not in ('A', 'R'):
        for attr, op, value, destination in workflows[current]:
            if attr is None:  # Default rule
                current = destination
                break
            elif op == '<' and part[attr] < value:
                current = destination
                break
            elif op == '>' and part[attr] > value:
                current = destination
                break

    return current == 'A'


def part1(workflows: dict, parts: list[dict]) -> int:
    """Part 1: Sum ratings of accepted parts."""
    total = 0
    for part in parts:
        if process_part(workflows, part):
            total += part['x'] + part['m'] + part['a'] + part['s']
    return total


def count_accepted(workflows: dict, workflow: str, ranges: dict[str, tuple[int, int]]) -> int:
    """
    Count combinations of xmas values that lead to acceptance.
    Uses range splitting to process all possible paths through workflows.

    ranges: dict mapping 'x', 'm', 'a', 's' to (min, max) inclusive ranges
    """
    if workflow == 'R':
        return 0
    if workflow == 'A':
        # Count all combinations in current ranges
        result = 1
        for lo, hi in ranges.values():
            result *= max(0, hi - lo + 1)
        return result

    total = 0
    ranges = dict(ranges)  # Make a copy

    for attr, op, value, destination in workflows[workflow]:
        if attr is None:  # Default rule
            total += count_accepted(workflows, destination, ranges)
        else:
            lo, hi = ranges[attr]

            if op == '<':
                # Split: [lo, value-1] goes to destination, [value, hi] continues
                if lo < value:
                    # Part that matches the condition
                    new_ranges = dict(ranges)
                    new_ranges[attr] = (lo, min(hi, value - 1))
                    total += count_accepted(workflows, destination, new_ranges)
                # Remaining part continues to next rule
                if hi >= value:
                    ranges[attr] = (max(lo, value), hi)
                else:
                    break  # No remaining range
            else:  # op == '>'
                # Split: [value+1, hi] goes to destination, [lo, value] continues
                if hi > value:
                    # Part that matches the condition
                    new_ranges = dict(ranges)
                    new_ranges[attr] = (max(lo, value + 1), hi)
                    total += count_accepted(workflows, destination, new_ranges)
                # Remaining part continues to next rule
                if lo <= value:
                    ranges[attr] = (lo, min(hi, value))
                else:
                    break  # No remaining range

    return total


def part2(workflows: dict) -> int:
    """Part 2: Count all possible accepted combinations (1-4000 for each rating)."""
    initial_ranges = {
        'x': (1, 4000),
        'm': (1, 4000),
        'a': (1, 4000),
        's': (1, 4000)
    }
    return count_accepted(workflows, 'in', initial_ranges)


def main():
    workflows, parts = parse_input(Path(__file__).parent.parent / "input.txt")
    print(f"Part 1: {part1(workflows, parts)}")
    print(f"Part 2: {part2(workflows)}")


if __name__ == "__main__":
    main()
