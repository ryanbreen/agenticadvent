#!/usr/bin/env python3
import re
from pathlib import Path

def part1(data: str) -> int:
    """Find all valid mul(X,Y) instructions and sum their products."""
    pattern = r'mul\((\d{1,3}),(\d{1,3})\)'
    matches = re.findall(pattern, data)
    return sum(int(x) * int(y) for x, y in matches)

def part2(data: str) -> int:
    """Like part1, but do() enables and don't() disables mul instructions."""
    # Find all mul instructions and do/don't toggles with their positions
    mul_pattern = r'mul\((\d{1,3}),(\d{1,3})\)'
    do_pattern = r'do\(\)'
    dont_pattern = r"don't\(\)"

    total = 0
    enabled = True

    # Build a list of all events with positions
    events = []
    for m in re.finditer(mul_pattern, data):
        events.append((m.start(), 'mul', int(m.group(1)), int(m.group(2))))
    for m in re.finditer(do_pattern, data):
        events.append((m.start(), 'do', 0, 0))
    for m in re.finditer(dont_pattern, data):
        events.append((m.start(), 'dont', 0, 0))

    # Sort by position and process
    events.sort(key=lambda x: x[0])

    for pos, event_type, x, y in events:
        if event_type == 'do':
            enabled = True
        elif event_type == 'dont':
            enabled = False
        elif event_type == 'mul' and enabled:
            total += x * y

    return total

def main():
    input_path = Path(__file__).parent.parent / 'input.txt'
    data = input_path.read_text()

    print('Part 1:', part1(data))
    print('Part 2:', part2(data))

if __name__ == '__main__':
    main()
