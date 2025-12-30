#!/usr/bin/env python3

from pathlib import Path
from collections import deque


def parse_input(text: str) -> list[str]:
    return text.strip().split('\n')


def count_energized(grid: list[str], start_row: int, start_col: int, start_dir: int) -> int:
    """Count energized tiles starting from given position and direction.
    Directions: 0=right, 1=down, 2=left, 3=up
    """
    rows, cols = len(grid), len(grid[0])
    dr = [0, 1, 0, -1]
    dc = [1, 0, -1, 0]

    visited = set()
    queue = deque([(start_row, start_col, start_dir)])

    while queue:
        r, c, d = queue.popleft()

        if r < 0 or r >= rows or c < 0 or c >= cols:
            continue

        state = (r, c, d)
        if state in visited:
            continue
        visited.add(state)

        cell = grid[r][c]

        if cell == '.':
            next_dirs = [d]
        elif cell == '/':
            next_dirs = [[3, 2, 1, 0][d]]
        elif cell == '\\':
            next_dirs = [[1, 0, 3, 2][d]]
        elif cell == '|':
            next_dirs = [1, 3] if d in (0, 2) else [d]
        elif cell == '-':
            next_dirs = [0, 2] if d in (1, 3) else [d]

        for nd in next_dirs:
            queue.append((r + dr[nd], c + dc[nd], nd))

    return len(set((r, c) for r, c, d in visited))


def part1(grid: list[str]) -> int:
    return count_energized(grid, 0, 0, 0)


def part2(grid: list[str]) -> int:
    """Find maximum energized tiles from any edge starting position."""
    rows, cols = len(grid), len(grid[0])
    max_energized = 0

    # Top row, heading down
    for c in range(cols):
        max_energized = max(max_energized, count_energized(grid, 0, c, 1))

    # Bottom row, heading up
    for c in range(cols):
        max_energized = max(max_energized, count_energized(grid, rows - 1, c, 3))

    # Left column, heading right
    for r in range(rows):
        max_energized = max(max_energized, count_energized(grid, r, 0, 0))

    # Right column, heading left
    for r in range(rows):
        max_energized = max(max_energized, count_energized(grid, r, cols - 1, 2))

    return max_energized


def main():
    input_file = Path(__file__).parent / "../input.txt"
    text = input_file.read_text()
    grid = parse_input(text)

    print(f"Part 1: {part1(grid)}")
    print(f"Part 2: {part2(grid)}")


if __name__ == "__main__":
    main()
