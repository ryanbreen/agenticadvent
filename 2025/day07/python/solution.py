from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()
lines = input_text.split("\n")


def part1():
    rows = len(lines)
    cols = len(lines[0]) if rows > 0 else 0

    # Find starting position S
    start_col = None
    for col in range(cols):
        if lines[0][col] == 'S':
            start_col = col
            break

    if start_col is None:
        return 0

    # Track active beam columns at each row
    # Use a set to handle beam merging
    active_beams = {start_col}
    split_count = 0

    # Process row by row starting from row 1 (below S)
    for row in range(1, rows):
        new_beams = set()

        for col in active_beams:
            if 0 <= col < cols:
                cell = lines[row][col]
                if cell == '^':
                    # Beam hits splitter - count it and emit left/right
                    split_count += 1
                    # Left beam goes to col-1, right beam goes to col+1
                    if col - 1 >= 0:
                        new_beams.add(col - 1)
                    if col + 1 < cols:
                        new_beams.add(col + 1)
                elif cell == '.':
                    # Beam continues straight down
                    new_beams.add(col)
                # If cell is something else (like S), beam continues
                else:
                    new_beams.add(col)

        active_beams = new_beams

        # If no more beams, stop
        if not active_beams:
            break

    return split_count


def part2():
    rows = len(lines)
    cols = len(lines[0]) if rows > 0 else 0

    # Find starting position S
    start_col = None
    for col in range(cols):
        if lines[0][col] == 'S':
            start_col = col
            break

    if start_col is None:
        return 0

    # Track number of timelines at each column position
    # Use a dict: col -> count of timelines at that position
    from collections import defaultdict
    timelines = defaultdict(int)
    timelines[start_col] = 1

    # Process row by row starting from row 1 (below S)
    for row in range(1, rows):
        new_timelines = defaultdict(int)

        for col, count in timelines.items():
            if 0 <= col < cols:
                cell = lines[row][col]
                if cell == '^':
                    # Each timeline splits into 2 (left and right)
                    if col - 1 >= 0:
                        new_timelines[col - 1] += count
                    if col + 1 < cols:
                        new_timelines[col + 1] += count
                elif cell == '.':
                    # Timelines continue straight down
                    new_timelines[col] += count
                else:
                    # Other characters - timelines continue
                    new_timelines[col] += count

        timelines = new_timelines

        # If no more timelines, stop
        if not timelines:
            break

    # Total number of timelines
    return sum(timelines.values())


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
