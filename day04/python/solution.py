from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def part1():
    """Count rolls of paper that can be accessed by a forklift.

    A roll can be accessed if it has fewer than 4 adjacent rolls
    in the 8 surrounding positions (including diagonals).
    """
    grid = lines
    rows = len(grid)
    cols = len(grid[0]) if rows > 0 else 0

    # Directions for 8 neighbors (including diagonals)
    directions = [
        (-1, -1), (-1, 0), (-1, 1),
        (0, -1),           (0, 1),
        (1, -1),  (1, 0),  (1, 1)
    ]

    accessible_count = 0

    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == '@':
                # Count adjacent rolls
                adjacent_rolls = 0
                for dr, dc in directions:
                    nr, nc = r + dr, c + dc
                    # Check bounds
                    if 0 <= nr < rows and 0 <= nc < cols:
                        if grid[nr][nc] == '@':
                            adjacent_rolls += 1

                # Accessible if fewer than 4 adjacent rolls
                if adjacent_rolls < 4:
                    accessible_count += 1

    return accessible_count


def part2():
    """Count total rolls removed by iteratively removing accessible rolls.

    A roll can be removed if it has fewer than 4 adjacent rolls.
    After removal, check again for newly accessible rolls.
    Repeat until no more rolls can be removed.
    """
    # Create a mutable copy of the grid
    grid = [list(line) for line in lines]
    rows = len(grid)
    cols = len(grid[0]) if rows > 0 else 0

    # Directions for 8 neighbors (including diagonals)
    directions = [
        (-1, -1), (-1, 0), (-1, 1),
        (0, -1),           (0, 1),
        (1, -1),  (1, 0),  (1, 1)
    ]

    total_removed = 0

    while True:
        # Find all rolls that can be removed in this iteration
        removable = []

        for r in range(rows):
            for c in range(cols):
                if grid[r][c] == '@':
                    # Count adjacent rolls
                    adjacent_rolls = 0
                    for dr, dc in directions:
                        nr, nc = r + dr, c + dc
                        # Check bounds
                        if 0 <= nr < rows and 0 <= nc < cols:
                            if grid[nr][nc] == '@':
                                adjacent_rolls += 1

                    # Can be removed if fewer than 4 adjacent rolls
                    if adjacent_rolls < 4:
                        removable.append((r, c))

        # If no rolls can be removed, we're done
        if not removable:
            break

        # Remove all accessible rolls
        for r, c in removable:
            grid[r][c] = '.'

        total_removed += len(removable)

    return total_removed


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
