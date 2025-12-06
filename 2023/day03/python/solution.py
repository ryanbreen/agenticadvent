from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def part1():
    total = 0
    rows = len(lines)
    cols = len(lines[0]) if rows > 0 else 0

    # Helper function to check if a position has a symbol
    def is_symbol(r, c):
        if 0 <= r < rows and 0 <= c < cols:
            char = lines[r][c]
            return char != '.' and not char.isdigit()
        return False

    # Helper function to check if a number is adjacent to any symbol
    def is_adjacent_to_symbol(row, start_col, end_col):
        # Check all positions around the number
        for r in range(row - 1, row + 2):
            for c in range(start_col - 1, end_col + 2):
                if is_symbol(r, c):
                    return True
        return False

    # Find all numbers in the grid
    for row in range(rows):
        col = 0
        while col < cols:
            if lines[row][col].isdigit():
                # Found start of a number
                start_col = col
                num_str = ""
                while col < cols and lines[row][col].isdigit():
                    num_str += lines[row][col]
                    col += 1
                end_col = col - 1

                # Check if this number is adjacent to a symbol
                if is_adjacent_to_symbol(row, start_col, end_col):
                    total += int(num_str)
            else:
                col += 1

    return total


def part2():
    rows = len(lines)
    cols = len(lines[0]) if rows > 0 else 0

    # Find all numbers and their positions
    numbers = []  # List of (number, row, start_col, end_col)
    for row in range(rows):
        col = 0
        while col < cols:
            if lines[row][col].isdigit():
                start_col = col
                num_str = ""
                while col < cols and lines[row][col].isdigit():
                    num_str += lines[row][col]
                    col += 1
                end_col = col - 1
                numbers.append((int(num_str), row, start_col, end_col))
            else:
                col += 1

    # Helper function to check if a number is adjacent to a position
    def is_adjacent(num_row, num_start, num_end, gear_row, gear_col):
        for r in range(num_row - 1, num_row + 2):
            for c in range(num_start - 1, num_end + 2):
                if r == gear_row and c == gear_col:
                    return True
        return False

    # Find all gears (*)
    total = 0
    for row in range(rows):
        for col in range(cols):
            if lines[row][col] == '*':
                # Find all numbers adjacent to this *
                adjacent_numbers = []
                for num, num_row, num_start, num_end in numbers:
                    if is_adjacent(num_row, num_start, num_end, row, col):
                        adjacent_numbers.append(num)

                # If exactly 2 numbers are adjacent, it's a gear
                if len(adjacent_numbers) == 2:
                    gear_ratio = adjacent_numbers[0] * adjacent_numbers[1]
                    total += gear_ratio

    return total


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
