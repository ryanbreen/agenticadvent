from pathlib import Path
from functools import lru_cache

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input - space-separated numbers
stones = list(map(int, input_text.split()))


@lru_cache(maxsize=None)
def count_stones(value: int, blinks: int) -> int:
    """Count how many stones result from a single stone after N blinks."""
    if blinks == 0:
        return 1

    # Rule 1: 0 becomes 1
    if value == 0:
        return count_stones(1, blinks - 1)

    # Rule 2: Even number of digits -> split
    s = str(value)
    if len(s) % 2 == 0:
        mid = len(s) // 2
        left = int(s[:mid])
        right = int(s[mid:])
        return count_stones(left, blinks - 1) + count_stones(right, blinks - 1)

    # Rule 3: Multiply by 2024
    return count_stones(value * 2024, blinks - 1)


def part1():
    return sum(count_stones(stone, 25) for stone in stones)


def part2():
    return sum(count_stones(stone, 75) for stone in stones)


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
