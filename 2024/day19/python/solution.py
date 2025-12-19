from pathlib import Path
from functools import lru_cache

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
parts = input_text.split("\n\n")
patterns = tuple(p.strip() for p in parts[0].split(","))
designs = parts[1].strip().split("\n")


def can_form(design: str) -> bool:
    """Check if design can be formed by concatenating patterns."""
    @lru_cache(maxsize=None)
    def dp(pos: int) -> bool:
        if pos == len(design):
            return True
        for pattern in patterns:
            plen = len(pattern)
            if design[pos:pos + plen] == pattern:
                if dp(pos + plen):
                    return True
        return False
    return dp(0)


def part1():
    return sum(1 for d in designs if can_form(d))


def count_ways(design: str) -> int:
    """Count number of ways to form design from patterns."""
    @lru_cache(maxsize=None)
    def dp(pos: int) -> int:
        if pos == len(design):
            return 1
        total = 0
        for pattern in patterns:
            plen = len(pattern)
            if design[pos:pos + plen] == pattern:
                total += dp(pos + plen)
        return total
    return dp(0)


def part2():
    return sum(count_ways(d) for d in designs)


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
