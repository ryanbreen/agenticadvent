from pathlib import Path
from collections import deque

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input into grid
lines = input_text.split("\n")
grid = [[int(c) for c in line] for line in lines]
rows = len(grid)
cols = len(grid[0])

# Directions: up, down, left, right
DIRS = [(-1, 0), (1, 0), (0, -1), (0, 1)]


def find_trailheads():
    """Find all positions with height 0."""
    trailheads = []
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == 0:
                trailheads.append((r, c))
    return trailheads


def count_reachable_nines(start_r, start_c):
    """BFS to find all 9s reachable from a trailhead."""
    visited = set()
    visited.add((start_r, start_c))
    queue = deque([(start_r, start_c)])
    nines = set()

    while queue:
        r, c = queue.popleft()
        current_height = grid[r][c]

        if current_height == 9:
            nines.add((r, c))
            continue

        # Try all four directions
        for dr, dc in DIRS:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols:
                if (nr, nc) not in visited:
                    if grid[nr][nc] == current_height + 1:
                        visited.add((nr, nc))
                        queue.append((nr, nc))

    return len(nines)


def part1():
    trailheads = find_trailheads()
    total_score = sum(count_reachable_nines(r, c) for r, c in trailheads)
    return total_score


def count_distinct_trails(start_r, start_c):
    """DFS to count all distinct trails from a trailhead to any 9."""
    def dfs(r, c):
        current_height = grid[r][c]
        if current_height == 9:
            return 1

        total = 0
        for dr, dc in DIRS:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols:
                if grid[nr][nc] == current_height + 1:
                    total += dfs(nr, nc)
        return total

    return dfs(start_r, start_c)


def part2():
    trailheads = find_trailheads()
    total_rating = sum(count_distinct_trails(r, c) for r, c in trailheads)
    return total_rating


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
