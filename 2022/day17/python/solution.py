#!/usr/bin/env python3
import os

def parse_input(text):
    """Parse jet pattern."""
    return text.strip()

# Rock shapes as list of (dx, dy) offsets from bottom-left
ROCKS = [
    [(0, 0), (1, 0), (2, 0), (3, 0)],           # Horizontal line
    [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],   # Plus
    [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],   # L shape
    [(0, 0), (0, 1), (0, 2), (0, 3)],           # Vertical line
    [(0, 0), (1, 0), (0, 1), (1, 1)]            # Square
]

WIDTH = 7

def simulate(jets, num_rocks):
    """Simulate falling rocks and return final height."""
    occupied = set()
    height = 0
    jet_idx = 0

    # For cycle detection
    states = {}
    heights = []

    for rock_num in range(num_rocks):
        rock_type = rock_num % 5
        rock = ROCKS[rock_type]

        # Starting position: left edge at x=2, bottom at y=height+3
        x, y = 2, height + 3

        while True:
            # Jet push
            jet = jets[jet_idx]
            jet_idx = (jet_idx + 1) % len(jets)

            dx = 1 if jet == '>' else -1

            # Check if can move horizontally
            can_move = True
            for rx, ry in rock:
                nx = x + rx + dx
                ny = y + ry
                if nx < 0 or nx >= WIDTH or (nx, ny) in occupied:
                    can_move = False
                    break

            if can_move:
                x += dx

            # Fall down
            can_fall = True
            for rx, ry in rock:
                nx = x + rx
                ny = y + ry - 1
                if ny < 0 or (nx, ny) in occupied:
                    can_fall = False
                    break

            if can_fall:
                y -= 1
            else:
                # Rock stops
                for rx, ry in rock:
                    occupied.add((x + rx, y + ry))
                    height = max(height, y + ry + 1)
                break

        heights.append(height)

        # Cycle detection for Part 2
        if num_rocks > 10000:
            # Create state key from surface profile
            # Use top 30 rows relative positions
            profile_depth = 30
            profile = []
            for col in range(WIDTH):
                for row in range(profile_depth):
                    if (col, height - 1 - row) in occupied:
                        profile.append((col, row))
                        break
                else:
                    profile.append((col, profile_depth))

            state = (rock_type, jet_idx, tuple(profile))

            if state in states:
                # Found cycle
                cycle_start = states[state]
                cycle_len = rock_num - cycle_start
                cycle_height = height - heights[cycle_start]

                # Calculate final height
                remaining = num_rocks - rock_num - 1
                full_cycles = remaining // cycle_len
                leftover = remaining % cycle_len

                final_height = height + full_cycles * cycle_height
                if leftover > 0:
                    final_height += heights[cycle_start + leftover] - heights[cycle_start]

                return final_height

            states[state] = rock_num

    return height

def part1(text):
    """Simulate 2022 rocks."""
    jets = parse_input(text)
    return simulate(jets, 2022)

def part2(text):
    """Simulate 1000000000000 rocks with cycle detection."""
    jets = parse_input(text)
    return simulate(jets, 1000000000000)

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        text = f.read()

    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
