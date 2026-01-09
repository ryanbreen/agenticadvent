#!/usr/bin/env python3
import os

def simulate_cpu(instructions):
    """Simulate CPU and yield (cycle, X) for each cycle."""
    x = 1
    cycle = 0

    for line in instructions:
        if line == 'noop':
            cycle += 1
            yield cycle, x
        else:  # addx V
            v = int(line.split()[1])
            cycle += 1
            yield cycle, x
            cycle += 1
            yield cycle, x
            x += v

def part1(instructions):
    """Sum signal strengths at cycles 20, 60, 100, 140, 180, 220."""
    target_cycles = {20, 60, 100, 140, 180, 220}
    total = 0

    for cycle, x in simulate_cpu(instructions):
        if cycle in target_cycles:
            total += cycle * x

    return total

def part2(instructions):
    """Render CRT display. Sprite is 3 pixels wide centered at X."""
    screen = []
    row = []

    for cycle, x in simulate_cpu(instructions):
        pos = (cycle - 1) % 40  # CRT position in current row
        if abs(pos - x) <= 1:
            row.append('#')
        else:
            row.append('.')

        if cycle % 40 == 0:
            screen.append(''.join(row))
            row = []

    return '\n'.join(screen)

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        instructions = f.read().strip().split('\n')

    print('Part 1:', part1(instructions))
    print('Part 2:')
    print(part2(instructions))

if __name__ == '__main__':
    main()
