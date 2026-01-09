#!/usr/bin/env python3
import os

DIRECTIONS = {
    'U': (0, 1),
    'D': (0, -1),
    'L': (-1, 0),
    'R': (1, 0),
}

def sign(x):
    return 0 if x == 0 else (1 if x > 0 else -1)

def move_tail(head, tail):
    """Move tail toward head if not adjacent."""
    dx = head[0] - tail[0]
    dy = head[1] - tail[1]

    # If adjacent or overlapping, don't move
    if abs(dx) <= 1 and abs(dy) <= 1:
        return tail

    # Move toward head
    return (tail[0] + sign(dx), tail[1] + sign(dy))

def simulate_rope(moves, rope_length):
    """Simulate rope with given length and return positions visited by tail."""
    knots = [(0, 0)] * rope_length
    visited = {knots[-1]}

    for line in moves:
        direction, count = line.split()
        count = int(count)
        dx, dy = DIRECTIONS[direction]

        for _ in range(count):
            # Move head
            knots[0] = (knots[0][0] + dx, knots[0][1] + dy)

            # Move each subsequent knot
            for i in range(1, rope_length):
                knots[i] = move_tail(knots[i-1], knots[i])

            visited.add(knots[-1])

    return len(visited)

def part1(moves):
    return simulate_rope(moves, 2)

def part2(moves):
    return simulate_rope(moves, 10)

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        moves = f.read().strip().split('\n')

    print('Part 1:', part1(moves))
    print('Part 2:', part2(moves))

if __name__ == '__main__':
    main()
