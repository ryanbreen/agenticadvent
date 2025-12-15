from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()


def parse_input(text):
    parts = text.split('\n\n')
    grid_lines = parts[0].split('\n')
    grid = [list(line) for line in grid_lines]
    moves = ''.join(parts[1].split())
    return grid, moves


def find_robot(grid):
    for r, row in enumerate(grid):
        for c, cell in enumerate(row):
            if cell == '@':
                return r, c
    return None


def move_robot(grid, robot_pos, direction):
    dr, dc = {'<': (0, -1), '>': (0, 1), '^': (-1, 0), 'v': (1, 0)}[direction]
    r, c = robot_pos
    nr, nc = r + dr, c + dc

    if grid[nr][nc] == '#':
        return robot_pos

    if grid[nr][nc] == '.':
        grid[r][c] = '.'
        grid[nr][nc] = '@'
        return (nr, nc)

    if grid[nr][nc] == 'O':
        check_r, check_c = nr, nc
        while grid[check_r][check_c] == 'O':
            check_r += dr
            check_c += dc

        if grid[check_r][check_c] == '#':
            return robot_pos

        grid[check_r][check_c] = 'O'
        grid[r][c] = '.'
        grid[nr][nc] = '@'
        return (nr, nc)

    return robot_pos


def calculate_gps(grid, box_char='O'):
    total = 0
    for r, row in enumerate(grid):
        for c, cell in enumerate(row):
            if cell == box_char:
                total += 100 * r + c
    return total


def part1():
    grid, moves = parse_input(input_text)
    robot_pos = find_robot(grid)

    for move in moves:
        robot_pos = move_robot(grid, robot_pos, move)

    return calculate_gps(grid)


def scale_grid(grid):
    """Scale the grid 2x wide for Part 2."""
    new_grid = []
    for row in grid:
        new_row = []
        for cell in row:
            if cell == '#':
                new_row.extend(['#', '#'])
            elif cell == 'O':
                new_row.extend(['[', ']'])
            elif cell == '.':
                new_row.extend(['.', '.'])
            elif cell == '@':
                new_row.extend(['@', '.'])
        new_grid.append(new_row)
    return new_grid


def can_move_box_vertical(grid, box_left_c, r, dr):
    """Check if a wide box at (r, box_left_c) can move in direction dr."""
    nr = r + dr
    left_c, right_c = box_left_c, box_left_c + 1

    # Check both cells the box would move into
    left_target = grid[nr][left_c]
    right_target = grid[nr][right_c]

    # If either hits a wall, can't move
    if left_target == '#' or right_target == '#':
        return False

    # Check for boxes in the way
    boxes_to_check = set()

    if left_target == '[':
        boxes_to_check.add((nr, left_c))
    elif left_target == ']':
        boxes_to_check.add((nr, left_c - 1))

    if right_target == '[':
        boxes_to_check.add((nr, right_c))
    elif right_target == ']':
        boxes_to_check.add((nr, right_c - 1))

    # Recursively check if those boxes can move
    for box_r, box_c in boxes_to_check:
        if not can_move_box_vertical(grid, box_c, box_r, dr):
            return False

    return True


def collect_boxes_vertical(grid, box_left_c, r, dr, collected):
    """Collect all boxes that need to move when pushing box at (r, box_left_c)."""
    collected.add((r, box_left_c))
    nr = r + dr
    left_c, right_c = box_left_c, box_left_c + 1

    left_target = grid[nr][left_c]
    right_target = grid[nr][right_c]

    boxes_to_check = set()

    if left_target == '[':
        boxes_to_check.add((nr, left_c))
    elif left_target == ']':
        boxes_to_check.add((nr, left_c - 1))

    if right_target == '[':
        boxes_to_check.add((nr, right_c))
    elif right_target == ']':
        boxes_to_check.add((nr, right_c - 1))

    for box_r, box_c in boxes_to_check:
        if (box_r, box_c) not in collected:
            collect_boxes_vertical(grid, box_c, box_r, dr, collected)


def move_robot_wide(grid, robot_pos, direction):
    dr, dc = {'<': (0, -1), '>': (0, 1), '^': (-1, 0), 'v': (1, 0)}[direction]
    r, c = robot_pos
    nr, nc = r + dr, c + dc

    target = grid[nr][nc]

    if target == '#':
        return robot_pos

    if target == '.':
        grid[r][c] = '.'
        grid[nr][nc] = '@'
        return (nr, nc)

    # Handle box pushing
    if target in '[]':
        if dc != 0:  # Horizontal movement
            # Find end of box chain
            check_c = nc
            while grid[r][check_c] in '[]':
                check_c += dc

            if grid[r][check_c] == '#':
                return robot_pos

            # Shift all boxes
            if dc > 0:  # Moving right
                for col in range(check_c, nc, -1):
                    grid[r][col] = grid[r][col - 1]
            else:  # Moving left
                for col in range(check_c, nc):
                    grid[r][col] = grid[r][col + 1]

            grid[r][c] = '.'
            grid[nr][nc] = '@'
            return (nr, nc)

        else:  # Vertical movement
            # Find the left edge of the box
            if target == '[':
                box_left_c = nc
            else:  # ']'
                box_left_c = nc - 1

            # Check if we can move this box (and all boxes it pushes)
            if not can_move_box_vertical(grid, box_left_c, nr, dr):
                return robot_pos

            # Collect all boxes that need to move
            boxes_to_move = set()
            collect_boxes_vertical(grid, box_left_c, nr, dr, boxes_to_move)

            # Sort boxes by row so we move them in the right order
            sorted_boxes = sorted(boxes_to_move, key=lambda b: -b[0] if dr > 0 else b[0])

            # Move all boxes
            for box_r, box_c in sorted_boxes:
                grid[box_r][box_c] = '.'
                grid[box_r][box_c + 1] = '.'
                grid[box_r + dr][box_c] = '['
                grid[box_r + dr][box_c + 1] = ']'

            # Move robot
            grid[r][c] = '.'
            grid[nr][nc] = '@'
            return (nr, nc)

    return robot_pos


def part2():
    grid, moves = parse_input(input_text)
    grid = scale_grid(grid)
    robot_pos = find_robot(grid)

    for move in moves:
        robot_pos = move_robot_wide(grid, robot_pos, move)

    return calculate_gps(grid, '[')


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
