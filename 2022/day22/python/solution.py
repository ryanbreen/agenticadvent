#!/usr/bin/env python3
import os
import re

def parse_input(text):
    """Parse the map and path instructions."""
    parts = text.split('\n\n')
    grid_lines = parts[0].split('\n')
    path = parts[1].strip()
    
    # Find dimensions
    height = len(grid_lines)
    width = max(len(line) for line in grid_lines)
    
    # Create grid (pad lines to consistent width)
    grid = []
    for line in grid_lines:
        padded = line.ljust(width)
        grid.append(padded)
    
    # Parse path into moves and turns
    instructions = []
    i = 0
    while i < len(path):
        if path[i].isdigit():
            j = i
            while j < len(path) and path[j].isdigit():
                j += 1
            instructions.append(int(path[i:j]))
            i = j
        else:
            instructions.append(path[i])
            i += 1
    
    return grid, instructions

def part1(text):
    """Navigate the map with 2D flat wrapping."""
    grid, instructions = parse_input(text)
    height = len(grid)
    width = len(grid[0])
    
    # Directions: 0=right, 1=down, 2=left, 3=up
    DR = [0, 1, 0, -1]
    DC = [1, 0, -1, 0]
    
    # Find starting position (leftmost open tile on top row)
    row, col = 0, grid[0].index('.')
    facing = 0  # Start facing right
    
    for instr in instructions:
        if isinstance(instr, int):
            # Move forward
            for _ in range(instr):
                dr, dc = DR[facing], DC[facing]
                nr, nc = row + dr, col + dc
                
                # Wrap around if needed
                # Wrap horizontally
                if facing == 0:  # Right
                    if nc >= width or grid[nr][nc] == ' ':
                        # Find leftmost non-space on this row
                        nc = 0
                        while grid[nr][nc] == ' ':
                            nc += 1
                elif facing == 2:  # Left
                    if nc < 0 or grid[nr][nc] == ' ':
                        # Find rightmost non-space on this row
                        nc = width - 1
                        while grid[nr][nc] == ' ':
                            nc -= 1
                elif facing == 1:  # Down
                    if nr >= height or grid[nr][nc] == ' ':
                        # Find topmost non-space on this column
                        nr = 0
                        while grid[nr][nc] == ' ':
                            nr += 1
                elif facing == 3:  # Up
                    if nr < 0 or grid[nr][nc] == ' ':
                        # Find bottommost non-space on this column
                        nr = height - 1
                        while grid[nr][nc] == ' ':
                            nr -= 1
                
                # Check if we hit a wall
                if grid[nr][nc] == '#':
                    break  # Stop moving
                
                # Move to new position
                row, col = nr, nc
        else:
            # Turn
            if instr == 'R':
                facing = (facing + 1) % 4
            else:
                facing = (facing - 1) % 4
    
    # Calculate password: 1000*row + 4*col + facing (1-indexed)
    return 1000 * (row + 1) + 4 * (col + 1) + facing

def get_cube_face_and_local(row, col, face_size):
    """Determine which face and local coordinates based on the specific cube layout."""
    # For the actual input, the cube layout appears to be:
    #   12
    #   3
    #  45
    #  6
    # With face_size = 50
    
    face_row = row // face_size
    face_col = col // face_size
    local_r = row % face_size
    local_c = col % face_size
    
    # Map face_row, face_col to face number
    if face_row == 0 and face_col == 1:
        return 1, local_r, local_c
    elif face_row == 0 and face_col == 2:
        return 2, local_r, local_c
    elif face_row == 1 and face_col == 1:
        return 3, local_r, local_c
    elif face_row == 2 and face_col == 0:
        return 4, local_r, local_c
    elif face_row == 2 and face_col == 1:
        return 5, local_r, local_c
    elif face_row == 3 and face_col == 0:
        return 6, local_r, local_c
    else:
        return -1, local_r, local_c

def wrap_cube(row, col, facing, face_size):
    """
    Handle cube wrapping for the actual input layout:
       12
       3
      45
      6
    
    Each face is face_size x face_size (50 for the actual input).
    Returns (new_row, new_col, new_facing).
    """
    S = face_size
    face, lr, lc = get_cube_face_and_local(row, col, S)
    
    # Based on the direction and which edge we're leaving
    if face == 1:
        if facing == 3:  # Up: goes to face 6, from left, facing right
            return 3*S + lc, 0, 0  # Face 6 left edge
        elif facing == 2:  # Left: goes to face 4, from left, facing right (inverted)
            return 3*S - 1 - lr, 0, 0
    elif face == 2:
        if facing == 0:  # Right: goes to face 5, from right, facing left (inverted)
            return 3*S - 1 - lr, 2*S - 1, 2
        elif facing == 1:  # Down: goes to face 3, from right, facing left
            return S + lc, 2*S - 1, 2
        elif facing == 3:  # Up: goes to face 6, from bottom, facing up
            return 4*S - 1, lc, 3
    elif face == 3:
        if facing == 0:  # Right: goes to face 2, from bottom, facing up
            return S - 1, 2*S + lr, 3
        elif facing == 2:  # Left: goes to face 4, from top, facing down
            return 2*S, lr, 1
    elif face == 4:
        if facing == 3:  # Up: goes to face 3, from left, facing right
            return S + lc, S, 0
        elif facing == 2:  # Left: goes to face 1, from left, facing right (inverted)
            return S - 1 - lr, S, 0
    elif face == 5:
        if facing == 0:  # Right: goes to face 2, from right, facing left (inverted)
            return S - 1 - lr, 3*S - 1, 2
        elif facing == 1:  # Down: goes to face 6, from right, facing left
            return 3*S + lc, S - 1, 2
    elif face == 6:
        if facing == 0:  # Right: goes to face 5, from bottom, facing up
            return 3*S - 1, S + lr, 3
        elif facing == 1:  # Down: goes to face 2, from top, facing down
            return 0, 2*S + lc, 1
        elif facing == 2:  # Left: goes to face 1, from top, facing down
            return 0, S + lr, 1
    
    # Shouldn't reach here
    return row, col, facing

def part2(text):
    """Navigate the map with cube wrapping."""
    grid, instructions = parse_input(text)
    height = len(grid)
    width = len(grid[0])
    
    # Determine face size
    face_size = 50 if height > 50 else 4  # 50 for actual input, 4 for example
    
    # Directions: 0=right, 1=down, 2=left, 3=up
    DR = [0, 1, 0, -1]
    DC = [1, 0, -1, 0]
    
    # Find starting position
    row, col = 0, grid[0].index('.')
    facing = 0
    
    for instr in instructions:
        if isinstance(instr, int):
            for _ in range(instr):
                dr, dc = DR[facing], DC[facing]
                nr, nc = row + dr, col + dc
                nf = facing
                
                # Check if we need to wrap (off grid or hitting a space)
                need_wrap = False
                if nr < 0 or nr >= height or nc < 0 or nc >= width:
                    need_wrap = True
                elif grid[nr][nc] == ' ':
                    need_wrap = True
                
                if need_wrap:
                    nr, nc, nf = wrap_cube(row, col, facing, face_size)
                
                # Check for wall
                if grid[nr][nc] == '#':
                    break
                
                row, col, facing = nr, nc, nf
        else:
            if instr == 'R':
                facing = (facing + 1) % 4
            else:
                facing = (facing - 1) % 4
    
    return 1000 * (row + 1) + 4 * (col + 1) + facing

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')
    
    with open(input_file) as f:
        text = f.read()
    
    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
