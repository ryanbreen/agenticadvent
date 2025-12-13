from pathlib import Path
import re

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()


def parse_machines(text: str) -> list[tuple]:
    """Parse claw machine configurations."""
    machines = []
    blocks = text.split("\n\n")

    for block in blocks:
        lines = block.strip().split("\n")
        # Button A: X+ax, Y+ay
        a_match = re.search(r"Button A: X\+(\d+), Y\+(\d+)", lines[0])
        ax, ay = int(a_match.group(1)), int(a_match.group(2))
        # Button B: X+bx, Y+by
        b_match = re.search(r"Button B: X\+(\d+), Y\+(\d+)", lines[1])
        bx, by = int(b_match.group(1)), int(b_match.group(2))
        # Prize: X=px, Y=py
        p_match = re.search(r"Prize: X=(\d+), Y=(\d+)", lines[2])
        px, py = int(p_match.group(1)), int(p_match.group(2))

        machines.append((ax, ay, bx, by, px, py))

    return machines


def solve_machine(ax: int, ay: int, bx: int, by: int, px: int, py: int, max_presses: int = None) -> int | None:
    """
    Solve for button presses using Cramer's rule.

    System of equations:
      a*ax + b*bx = px
      a*ay + b*by = py

    Solution:
      det = ax*by - ay*bx
      a = (px*by - py*bx) / det
      b = (ax*py - ay*px) / det

    Returns token cost (3*a + b) or None if no valid solution.
    """
    det = ax * by - ay * bx

    if det == 0:
        return None  # No unique solution

    # Calculate using integer arithmetic
    a_num = px * by - py * bx
    b_num = ax * py - ay * px

    # Check if solutions are integers
    if a_num % det != 0 or b_num % det != 0:
        return None

    a = a_num // det
    b = b_num // det

    # Check non-negative
    if a < 0 or b < 0:
        return None

    # Check max presses constraint (Part 1)
    if max_presses is not None and (a > max_presses or b > max_presses):
        return None

    return 3 * a + b


def part1() -> int:
    """Part 1: Max 100 presses per button."""
    machines = parse_machines(input_text)
    total = 0

    for machine in machines:
        cost = solve_machine(*machine, max_presses=100)
        if cost is not None:
            total += cost

    return total


def part2() -> int:
    """Part 2: Prize coordinates shifted by 10^13, no press limit."""
    machines = parse_machines(input_text)
    offset = 10_000_000_000_000
    total = 0

    for ax, ay, bx, by, px, py in machines:
        # Shift prize coordinates
        cost = solve_machine(ax, ay, bx, by, px + offset, py + offset, max_presses=None)
        if cost is not None:
            total += cost

    return total


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
