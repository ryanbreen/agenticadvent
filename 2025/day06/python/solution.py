from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def parse_problems(lines):
    """Parse the worksheet into a list of (numbers, operator) tuples."""
    if not lines:
        return []

    # Find the operator row (last non-empty row with only +, *, and spaces)
    op_row_idx = len(lines) - 1
    while op_row_idx >= 0 and (not lines[op_row_idx].strip() or
          not all(c in '+* ' for c in lines[op_row_idx])):
        op_row_idx -= 1

    if op_row_idx < 0:
        return []

    op_row = lines[op_row_idx]
    number_rows = lines[:op_row_idx]

    # Find max width
    max_width = max(len(line) for line in lines)

    # Pad all rows to the same width
    padded_number_rows = [line.ljust(max_width) for line in number_rows]
    padded_op_row = op_row.ljust(max_width)

    # Find problem boundaries by looking for columns that are all spaces
    problems = []
    col = 0

    while col < max_width:
        # Skip separator columns (all spaces)
        while col < max_width and all(row[col] == ' ' for row in padded_number_rows) and padded_op_row[col] == ' ':
            col += 1

        if col >= max_width:
            break

        # Find the end of this problem
        start_col = col
        while col < max_width:
            # Check if this is a separator column
            is_separator = all(row[col] == ' ' for row in padded_number_rows) and padded_op_row[col] == ' '
            if is_separator:
                break
            col += 1

        end_col = col

        # Extract numbers and operator for this problem
        numbers = []
        for row in padded_number_rows:
            num_str = row[start_col:end_col].strip()
            if num_str:
                numbers.append(int(num_str))

        op_str = padded_op_row[start_col:end_col].strip()
        if op_str and numbers:
            problems.append((numbers, op_str))

    return problems


def solve_problem(numbers, op):
    """Solve a single problem given numbers and operator."""
    if op == '+':
        return sum(numbers)
    elif op == '*':
        result = 1
        for n in numbers:
            result *= n
        return result
    return 0


def part1():
    problems = parse_problems(lines)
    total = 0
    for numbers, op in problems:
        result = solve_problem(numbers, op)
        total += result
    return total


def parse_problems_part2(lines):
    """Parse the worksheet for part 2 - reading right-to-left columns."""
    if not lines:
        return []

    # Find the operator row (last non-empty row with only +, *, and spaces)
    op_row_idx = len(lines) - 1
    while op_row_idx >= 0 and (not lines[op_row_idx].strip() or
          not all(c in '+* ' for c in lines[op_row_idx])):
        op_row_idx -= 1

    if op_row_idx < 0:
        return []

    op_row = lines[op_row_idx]
    number_rows = lines[:op_row_idx]

    # Find max width
    max_width = max(len(line) for line in lines)

    # Pad all rows to the same width
    padded_number_rows = [line.ljust(max_width) for line in number_rows]
    padded_op_row = op_row.ljust(max_width)

    # Find problem boundaries by looking for columns that are all spaces
    problems = []
    col = 0

    while col < max_width:
        # Skip separator columns (all spaces)
        while col < max_width and all(row[col] == ' ' for row in padded_number_rows) and padded_op_row[col] == ' ':
            col += 1

        if col >= max_width:
            break

        # Find the end of this problem
        start_col = col
        while col < max_width:
            # Check if this is a separator column
            is_separator = all(row[col] == ' ' for row in padded_number_rows) and padded_op_row[col] == ' '
            if is_separator:
                break
            col += 1

        end_col = col

        # For Part 2: Read columns right-to-left, each column forms a number
        # reading top-to-bottom as most-to-least significant digit
        numbers = []
        for c in range(end_col - 1, start_col - 1, -1):  # Right to left
            digits = []
            for row in padded_number_rows:
                ch = row[c]
                if ch.isdigit():
                    digits.append(ch)
            if digits:
                # Join digits to form number (top=most significant, bottom=least)
                num = int(''.join(digits))
                numbers.append(num)

        op_str = padded_op_row[start_col:end_col].strip()
        if op_str and numbers:
            problems.append((numbers, op_str))

    return problems


def part2():
    problems = parse_problems_part2(lines)
    total = 0
    for numbers, op in problems:
        result = solve_problem(numbers, op)
        total += result
    return total


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
