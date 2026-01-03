#!/usr/bin/env python3
"""Day 22: Sand Slabs - 3D falling bricks simulation."""

from pathlib import Path
from collections import defaultdict, deque


def parse_input(filename: str) -> list[tuple[int, ...]]:
    """Parse brick coordinates from input."""
    bricks = []
    for line in Path(filename).read_text().strip().split('\n'):
        left, right = line.split('~')
        x1, y1, z1 = map(int, left.split(','))
        x2, y2, z2 = map(int, right.split(','))
        # Ensure z1 <= z2 for consistent processing
        if z1 > z2:
            x1, y1, z1, x2, y2, z2 = x2, y2, z2, x1, y1, z1
        bricks.append((x1, y1, z1, x2, y2, z2))
    return bricks


def get_brick_cells(brick: tuple[int, ...]) -> set[tuple[int, int, int]]:
    """Get all cells occupied by a brick."""
    x1, y1, z1, x2, y2, z2 = brick
    cells = set()
    for x in range(min(x1, x2), max(x1, x2) + 1):
        for y in range(min(y1, y2), max(y1, y2) + 1):
            for z in range(min(z1, z2), max(z1, z2) + 1):
                cells.add((x, y, z))
    return cells


def settle_bricks(bricks: list[tuple[int, ...]]) -> tuple[list[tuple[int, ...]], dict[int, set[int]], dict[int, set[int]]]:
    """
    Simulate bricks falling and settling.
    Returns settled bricks and support relationships.
    """
    # Sort by minimum z coordinate
    sorted_bricks = sorted(enumerate(bricks), key=lambda x: min(x[1][2], x[1][5]))

    # Track occupied cells: (x, y, z) -> brick index
    occupied = {}
    settled = [None] * len(bricks)

    # supports[i] = set of brick indices that brick i supports (bricks above)
    # supporters[i] = set of brick indices that support brick i (bricks below)
    supports = defaultdict(set)
    supporters = defaultdict(set)

    for orig_idx, brick in sorted_bricks:
        x1, y1, z1, x2, y2, z2 = brick

        # Find the lowest z where this brick can rest
        drop = z1 - 1  # Maximum drop (to z=1)

        # Get xy footprint of this brick
        for x in range(min(x1, x2), max(x1, x2) + 1):
            for y in range(min(y1, y2), max(y1, y2) + 1):
                # Check each z level below the brick
                for z in range(z1 - 1, 0, -1):
                    if (x, y, z) in occupied:
                        drop = min(drop, z1 - z - 1)
                        break

        # Drop the brick
        new_z1 = z1 - drop
        new_z2 = z2 - drop
        new_brick = (x1, y1, new_z1, x2, y2, new_z2)
        settled[orig_idx] = new_brick

        # Mark cells as occupied and find supporters
        for x in range(min(x1, x2), max(x1, x2) + 1):
            for y in range(min(y1, y2), max(y1, y2) + 1):
                # Check if there's a brick directly below
                if (x, y, new_z1 - 1) in occupied:
                    supporter_idx = occupied[(x, y, new_z1 - 1)]
                    supporters[orig_idx].add(supporter_idx)
                    supports[supporter_idx].add(orig_idx)

                # Mark all cells of this brick as occupied
                for z in range(new_z1, new_z2 + 1):
                    occupied[(x, y, z)] = orig_idx

    return settled, supports, supporters


def part1(bricks: list[tuple[int, ...]]) -> int:
    """Count bricks that can be safely disintegrated."""
    settled, supports, supporters = settle_bricks(bricks)

    safe_count = 0
    for i in range(len(bricks)):
        # Brick i can be safely removed if every brick it supports
        # has at least one other supporter
        can_remove = True
        for supported in supports[i]:
            if len(supporters[supported]) == 1:
                can_remove = False
                break
        if can_remove:
            safe_count += 1

    return safe_count


def part2(bricks: list[tuple[int, ...]]) -> int:
    """Count total bricks that would fall for each disintegration."""
    settled, supports, supporters = settle_bricks(bricks)

    total_falls = 0

    for i in range(len(bricks)):
        # Simulate removing brick i and count chain reaction
        # BFS to find all bricks that would fall
        falling = {i}
        queue = deque([i])

        while queue:
            brick = queue.popleft()

            # Check all bricks that this brick supports
            for supported in supports[brick]:
                if supported in falling:
                    continue

                # This brick falls if all its supporters have fallen
                if supporters[supported].issubset(falling):
                    falling.add(supported)
                    queue.append(supported)

        # Don't count the initial brick we removed
        total_falls += len(falling) - 1

    return total_falls


def main():
    input_path = Path(__file__).parent.parent / "input.txt"
    bricks = parse_input(input_path)
    print(f"Part 1: {part1(bricks)}")
    print(f"Part 2: {part2(bricks)}")


if __name__ == "__main__":
    main()
