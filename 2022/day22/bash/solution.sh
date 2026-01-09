#!/bin/bash

# Day 22: Monkey Map
# Navigate a 2D map with wrapping (Part 1: flat, Part 2: cube)

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Use AWK for the heavy computation
awk '
BEGIN {
    height = 0
    width = 0
    path_line = ""
    reading_grid = 1
}

{
    if (reading_grid) {
        if ($0 == "") {
            reading_grid = 0
        } else {
            grid[height] = $0
            if (length($0) > width) width = length($0)
            height++
        }
    } else {
        path_line = $0
    }
}

END {
    # Pad all grid lines to consistent width
    for (r = 0; r < height; r++) {
        while (length(grid[r]) < width) {
            grid[r] = grid[r] " "
        }
    }

    # Parse path into instructions
    num_instructions = 0
    i = 1
    while (i <= length(path_line)) {
        c = substr(path_line, i, 1)
        if (c ~ /[0-9]/) {
            num = ""
            while (i <= length(path_line) && substr(path_line, i, 1) ~ /[0-9]/) {
                num = num substr(path_line, i, 1)
                i++
            }
            instr_type[num_instructions] = "N"
            instr_val[num_instructions] = int(num)
            num_instructions++
        } else {
            instr_type[num_instructions] = "T"
            instr_val[num_instructions] = c
            num_instructions++
            i++
        }
    }

    # Direction deltas: 0=right, 1=down, 2=left, 3=up
    DR[0] = 0; DC[0] = 1
    DR[1] = 1; DC[1] = 0
    DR[2] = 0; DC[2] = -1
    DR[3] = -1; DC[3] = 0

    # Find starting position (leftmost open tile on top row)
    start_col = 0
    for (c = 0; c < width; c++) {
        if (substr(grid[0], c+1, 1) == ".") {
            start_col = c
            break
        }
    }

    # ============= PART 1: Flat wrapping =============
    row = 0
    col = start_col
    facing = 0

    for (i = 0; i < num_instructions; i++) {
        if (instr_type[i] == "N") {
            steps = instr_val[i]
            for (s = 0; s < steps; s++) {
                dr = DR[facing]
                dc = DC[facing]
                nr = row + dr
                nc = col + dc

                # Wrap around if needed
                if (facing == 0) {  # Right
                    if (nc >= width || substr(grid[nr], nc+1, 1) == " ") {
                        nc = 0
                        while (substr(grid[nr], nc+1, 1) == " ") nc++
                    }
                } else if (facing == 2) {  # Left
                    if (nc < 0 || substr(grid[nr], nc+1, 1) == " ") {
                        nc = width - 1
                        while (substr(grid[nr], nc+1, 1) == " ") nc--
                    }
                } else if (facing == 1) {  # Down
                    if (nr >= height || substr(grid[nr], nc+1, 1) == " ") {
                        nr = 0
                        while (substr(grid[nr], nc+1, 1) == " ") nr++
                    }
                } else if (facing == 3) {  # Up
                    if (nr < 0 || substr(grid[nr], nc+1, 1) == " ") {
                        nr = height - 1
                        while (substr(grid[nr], nc+1, 1) == " ") nr--
                    }
                }

                # Check for wall
                if (substr(grid[nr], nc+1, 1) == "#") {
                    break
                }

                row = nr
                col = nc
            }
        } else {
            # Turn
            if (instr_val[i] == "R") {
                facing = (facing + 1) % 4
            } else {
                facing = (facing + 3) % 4
            }
        }
    }

    part1 = 1000 * (row + 1) + 4 * (col + 1) + facing

    # ============= PART 2: Cube wrapping =============
    # Determine face size
    face_size = (height > 50) ? 50 : 4
    S = face_size

    row = 0
    col = start_col
    facing = 0

    for (i = 0; i < num_instructions; i++) {
        if (instr_type[i] == "N") {
            steps = instr_val[i]
            for (st = 0; st < steps; st++) {
                dr = DR[facing]
                dc = DC[facing]
                nr = row + dr
                nc = col + dc
                nf = facing

                # Check if we need to wrap
                need_wrap = 0
                if (nr < 0 || nr >= height || nc < 0 || nc >= width) {
                    need_wrap = 1
                } else if (substr(grid[nr], nc+1, 1) == " ") {
                    need_wrap = 1
                }

                if (need_wrap) {
                    # Determine which face we are on
                    face_row = int(row / S)
                    face_col = int(col / S)
                    lr = row % S
                    lc = col % S

                    # Map face_row, face_col to face number
                    # Layout:
                    #   12
                    #   3
                    #  45
                    #  6
                    face = -1
                    if (face_row == 0 && face_col == 1) face = 1
                    else if (face_row == 0 && face_col == 2) face = 2
                    else if (face_row == 1 && face_col == 1) face = 3
                    else if (face_row == 2 && face_col == 0) face = 4
                    else if (face_row == 2 && face_col == 1) face = 5
                    else if (face_row == 3 && face_col == 0) face = 6

                    # Cube wrapping rules
                    if (face == 1) {
                        if (facing == 3) {  # Up: goes to face 6, from left, facing right
                            nr = 3*S + lc
                            nc = 0
                            nf = 0
                        } else if (facing == 2) {  # Left: goes to face 4, from left, facing right (inverted)
                            nr = 3*S - 1 - lr
                            nc = 0
                            nf = 0
                        }
                    } else if (face == 2) {
                        if (facing == 0) {  # Right: goes to face 5, from right, facing left (inverted)
                            nr = 3*S - 1 - lr
                            nc = 2*S - 1
                            nf = 2
                        } else if (facing == 1) {  # Down: goes to face 3, from right, facing left
                            nr = S + lc
                            nc = 2*S - 1
                            nf = 2
                        } else if (facing == 3) {  # Up: goes to face 6, from bottom, facing up
                            nr = 4*S - 1
                            nc = lc
                            nf = 3
                        }
                    } else if (face == 3) {
                        if (facing == 0) {  # Right: goes to face 2, from bottom, facing up
                            nr = S - 1
                            nc = 2*S + lr
                            nf = 3
                        } else if (facing == 2) {  # Left: goes to face 4, from top, facing down
                            nr = 2*S
                            nc = lr
                            nf = 1
                        }
                    } else if (face == 4) {
                        if (facing == 3) {  # Up: goes to face 3, from left, facing right
                            nr = S + lc
                            nc = S
                            nf = 0
                        } else if (facing == 2) {  # Left: goes to face 1, from left, facing right (inverted)
                            nr = S - 1 - lr
                            nc = S
                            nf = 0
                        }
                    } else if (face == 5) {
                        if (facing == 0) {  # Right: goes to face 2, from right, facing left (inverted)
                            nr = S - 1 - lr
                            nc = 3*S - 1
                            nf = 2
                        } else if (facing == 1) {  # Down: goes to face 6, from right, facing left
                            nr = 3*S + lc
                            nc = S - 1
                            nf = 2
                        }
                    } else if (face == 6) {
                        if (facing == 0) {  # Right: goes to face 5, from bottom, facing up
                            nr = 3*S - 1
                            nc = S + lr
                            nf = 3
                        } else if (facing == 1) {  # Down: goes to face 2, from top, facing down
                            nr = 0
                            nc = 2*S + lc
                            nf = 1
                        } else if (facing == 2) {  # Left: goes to face 1, from top, facing down
                            nr = 0
                            nc = S + lr
                            nf = 1
                        }
                    }
                }

                # Check for wall
                if (substr(grid[nr], nc+1, 1) == "#") {
                    break
                }

                row = nr
                col = nc
                facing = nf
            }
        } else {
            # Turn
            if (instr_val[i] == "R") {
                facing = (facing + 1) % 4
            } else {
                facing = (facing + 3) % 4
            }
        }
    }

    part2 = 1000 * (row + 1) + 4 * (col + 1) + facing

    print "Part 1:", part1
    print "Part 2:", part2
}
' "$INPUT_FILE"
