#!/usr/bin/env python3
import sys
from collections import deque

def parse_grid(input_text):
    grid = []
    start = end = None
    for r, line in enumerate(input_text.strip().split('\n')):
        row = list(line)
        grid.append(row)
        for c, ch in enumerate(row):
            if ch == 'S':
                start = (r, c)
            elif ch == 'E':
                end = (r, c)
    return grid, start, end

def trace_path(grid, start, end):
    """Trace the single path from start to end, returning distance from start for each cell."""
    rows, cols = len(grid), len(grid[0])
    dist = {start: 0}
    queue = deque([start])

    while queue:
        r, c = queue.popleft()
        if (r, c) == end:
            break
        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols and grid[nr][nc] != '#':
                if (nr, nc) not in dist:
                    dist[(nr, nc)] = dist[(r, c)] + 1
                    queue.append((nr, nc))

    return dist

def count_cheats(dist, max_cheat_time, min_savings):
    """Count cheats that save at least min_savings picoseconds."""
    count = 0
    track_positions = list(dist.keys())

    for r1, c1 in track_positions:
        d1 = dist[(r1, c1)]
        for r2, c2 in track_positions:
            # Manhattan distance is the cheat cost
            cheat_cost = abs(r2 - r1) + abs(c2 - c1)
            if cheat_cost <= max_cheat_time:
                d2 = dist[(r2, c2)]
                savings = d2 - d1 - cheat_cost
                if savings >= min_savings:
                    count += 1

    return count

def part1(grid, start, end):
    dist = trace_path(grid, start, end)
    return count_cheats(dist, 2, 100)

def part2(grid, start, end):
    dist = trace_path(grid, start, end)
    return count_cheats(dist, 20, 100)

def main():
    with open('../input.txt') as f:
        input_text = f.read()

    grid, start, end = parse_grid(input_text)

    print('Part 1:', part1(grid, start, end))
    print('Part 2:', part2(grid, start, end))

if __name__ == '__main__':
    main()
