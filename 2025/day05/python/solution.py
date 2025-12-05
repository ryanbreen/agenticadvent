from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def part1():
    # Find the blank line separator
    blank_idx = lines.index("")

    # Parse ranges from the first section
    ranges = []
    for line in lines[:blank_idx]:
        start, end = map(int, line.split("-"))
        ranges.append((start, end))

    # Parse ingredient IDs from the second section
    ingredient_ids = [int(line) for line in lines[blank_idx + 1:] if line]

    # Count how many ingredient IDs fall within any range
    fresh_count = 0
    for ingredient_id in ingredient_ids:
        for start, end in ranges:
            if start <= ingredient_id <= end:
                fresh_count += 1
                break  # Found a match, no need to check other ranges

    return fresh_count


def part2():
    # Find the blank line separator
    blank_idx = lines.index("")

    # Parse ranges from the first section
    ranges = []
    for line in lines[:blank_idx]:
        start, end = map(int, line.split("-"))
        ranges.append((start, end))

    # Sort ranges by start position
    ranges.sort(key=lambda x: x[0])

    # Merge overlapping ranges
    merged = []
    for start, end in ranges:
        if merged and start <= merged[-1][1] + 1:
            # Overlapping or adjacent - merge with the last range
            merged[-1] = (merged[-1][0], max(merged[-1][1], end))
        else:
            # No overlap - add as new range
            merged.append((start, end))

    # Count total unique IDs covered by merged ranges
    total_count = 0
    for start, end in merged:
        total_count += (end - start + 1)

    return total_count


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
