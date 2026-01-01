#!/usr/bin/env python3
"""Day 18: Lavaduct Lagoon - Polygon area with Shoelace formula and Pick's theorem."""

from pathlib import Path


def parse_input(filename: str) -> list[tuple[str, int, str]]:
    """Parse dig plan instructions."""
    text = Path(filename).read_text().strip()
    instructions = []
    for line in text.split('\n'):
        parts = line.split()
        direction = parts[0]
        distance = int(parts[1])
        color = parts[2][2:-1]  # Remove (# and )
        instructions.append((direction, distance, color))
    return instructions


def calculate_area(vertices: list[tuple[int, int]], perimeter: int) -> int:
    """
    Calculate total area using Shoelace formula and Pick's theorem.

    Shoelace gives us twice the signed area of the polygon.
    Pick's theorem: A = i + b/2 - 1, where i = interior points, b = boundary points
    We want: Total = i + b = A + b/2 + 1
    """
    # Shoelace formula for polygon area
    n = len(vertices)
    area = 0
    for i in range(n):
        j = (i + 1) % n
        area += vertices[i][0] * vertices[j][1]
        area -= vertices[j][0] * vertices[i][1]
    area = abs(area) // 2

    # Total points = interior + boundary
    # From Pick's theorem: interior = area - boundary/2 + 1
    # Total = interior + boundary = area + boundary/2 + 1
    return area + perimeter // 2 + 1


def part1(instructions: list[tuple[str, int, str]]) -> int:
    """Part 1: Follow the dig plan directions."""
    direction_map = {
        'R': (0, 1),
        'D': (1, 0),
        'L': (0, -1),
        'U': (-1, 0)
    }

    vertices = [(0, 0)]
    perimeter = 0
    r, c = 0, 0

    for direction, distance, _ in instructions:
        dr, dc = direction_map[direction]
        r += dr * distance
        c += dc * distance
        vertices.append((r, c))
        perimeter += distance

    return calculate_area(vertices, perimeter)


def part2(instructions: list[tuple[str, int, str]]) -> int:
    """Part 2: Decode instructions from hex color codes."""
    # Last digit of hex: 0=R, 1=D, 2=L, 3=U
    # First 5 digits: distance in hex
    direction_map = {
        '0': (0, 1),   # R
        '1': (1, 0),   # D
        '2': (0, -1),  # L
        '3': (-1, 0)   # U
    }

    vertices = [(0, 0)]
    perimeter = 0
    r, c = 0, 0

    for _, _, color in instructions:
        distance = int(color[:5], 16)
        direction = color[5]
        dr, dc = direction_map[direction]
        r += dr * distance
        c += dc * distance
        vertices.append((r, c))
        perimeter += distance

    return calculate_area(vertices, perimeter)


def main():
    instructions = parse_input(Path(__file__).parent.parent / "input.txt")
    print(f"Part 1: {part1(instructions)}")
    print(f"Part 2: {part2(instructions)}")


if __name__ == "__main__":
    main()
