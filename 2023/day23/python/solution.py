#!/usr/bin/env python3
"""Day 23: A Long Walk - Longest path through hiking trails."""

from pathlib import Path
from collections import defaultdict


def parse_input(filename: str) -> list[str]:
    """Parse the grid from input."""
    return Path(filename).read_text().strip().split('\n')


def find_junctions(grid: list[str]) -> set[tuple[int, int]]:
    """Find all junction points (start, end, and intersections)."""
    rows, cols = len(grid), len(grid[0])
    junctions = set()

    # Start and end points
    start = (0, grid[0].index('.'))
    end = (rows - 1, grid[rows - 1].index('.'))
    junctions.add(start)
    junctions.add(end)

    # Find intersections (cells with 3+ walkable neighbors)
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == '#':
                continue
            neighbors = 0
            for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                nr, nc = r + dr, c + dc
                if 0 <= nr < rows and 0 <= nc < cols and grid[nr][nc] != '#':
                    neighbors += 1
            if neighbors >= 3:
                junctions.add((r, c))

    return junctions


def build_graph(grid: list[str], junctions: set[tuple[int, int]], respect_slopes: bool) -> dict:
    """Build a graph of junctions with edge weights (distances)."""
    rows, cols = len(grid), len(grid[0])

    # Direction mappings for slopes
    slope_dirs = {
        '^': (-1, 0),
        'v': (1, 0),
        '<': (0, -1),
        '>': (0, 1)
    }

    graph = defaultdict(dict)

    for start_junction in junctions:
        # BFS from each junction to find reachable junctions
        stack = [(start_junction, 0)]
        visited = {start_junction}

        while stack:
            (r, c), dist = stack.pop()

            if dist > 0 and (r, c) in junctions:
                # Found another junction
                graph[start_junction][(r, c)] = dist
                continue

            # Explore neighbors
            for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                nr, nc = r + dr, c + dc
                if not (0 <= nr < rows and 0 <= nc < cols):
                    continue
                if grid[nr][nc] == '#':
                    continue
                if (nr, nc) in visited:
                    continue

                # Check slope constraints for Part 1
                if respect_slopes:
                    cell = grid[r][c]
                    if cell in slope_dirs:
                        req_dr, req_dc = slope_dirs[cell]
                        if (dr, dc) != (req_dr, req_dc):
                            continue

                visited.add((nr, nc))
                stack.append(((nr, nc), dist + 1))

    return graph


def longest_path_dfs(graph: dict, start: tuple[int, int], end: tuple[int, int]) -> int:
    """Find longest path using DFS with backtracking."""
    visited = set()

    def dfs(node: tuple[int, int]) -> int:
        if node == end:
            return 0

        visited.add(node)
        max_dist = float('-inf')

        for neighbor, dist in graph[node].items():
            if neighbor not in visited:
                result = dfs(neighbor)
                if result != float('-inf'):
                    max_dist = max(max_dist, dist + result)

        visited.remove(node)
        return max_dist

    return dfs(start)


def solve(grid: list[str], respect_slopes: bool) -> int:
    """Solve for either part."""
    rows = len(grid)
    start = (0, grid[0].index('.'))
    end = (rows - 1, grid[rows - 1].index('.'))

    junctions = find_junctions(grid)
    graph = build_graph(grid, junctions, respect_slopes)

    return longest_path_dfs(graph, start, end)


def part1(grid: list[str]) -> int:
    """Part 1: Respect slope directions."""
    return solve(grid, respect_slopes=True)


def part2(grid: list[str]) -> int:
    """Part 2: Ignore slopes (treat as regular paths)."""
    return solve(grid, respect_slopes=False)


def main():
    input_path = Path(__file__).parent.parent / "input.txt"
    grid = parse_input(input_path)
    print(f"Part 1: {part1(grid)}")
    print(f"Part 2: {part2(grid)}")


if __name__ == "__main__":
    main()
