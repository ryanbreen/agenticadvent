// Day 22: Monkey Map - ARM64 Assembly Solution for macOS
// Navigate a 2D map with wrapping (Part 1: flat, Part 2: cube)

.global _start
.align 4

// Constants
.equ BUFFER_SIZE, 65536
.equ MAX_WIDTH, 200
.equ MAX_HEIGHT, 250
.equ MAX_INSTRUCTIONS, 2048
.equ FACE_SIZE, 50

// Data section
.data
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"
input_path:     .asciz "../input.txt"

.align 8
buffer:         .space BUFFER_SIZE
grid:           .space MAX_WIDTH * MAX_HEIGHT    // The map grid
instructions:   .space MAX_INSTRUCTIONS * 8      // Array of instructions (negative = turn, positive = move)
num_buf:        .space 32

// Variables
grid_width:     .quad 0
grid_height:    .quad 0
num_instructions: .quad 0

// Direction arrays: right=0, down=1, left=2, up=3
dr_array:       .quad 0, 1, 0, -1    // Delta row for each direction
dc_array:       .quad 1, 0, -1, 0    // Delta col for each direction

.text

// ============================================================
// _start: Entry point
// ============================================================
_start:
    // Open input file
    mov x0, #-100               // AT_FDCWD
    adrp x1, input_path@PAGE
    add x1, x1, input_path@PAGEOFF
    mov x2, #0                  // O_RDONLY
    mov x3, #0
    mov x16, #463               // openat syscall
    svc #0

    cmp x0, #0
    b.lt exit_error
    mov x19, x0                 // Save fd in x19

    // Read file
    mov x0, x19
    adrp x1, buffer@PAGE
    add x1, x1, buffer@PAGEOFF
    mov x2, #BUFFER_SIZE
    mov x16, #3                 // read syscall
    svc #0

    mov x20, x0                 // Save bytes read in x20

    // Close file
    mov x0, x19
    mov x16, #6                 // close syscall
    svc #0

    // Parse input
    bl parse_input

    // Part 1
    bl solve_part1
    mov x21, x0                 // Save part1 result

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_string
    mov x0, x21
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    // Part 2
    bl solve_part2
    mov x21, x0                 // Save part2 result

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_string
    mov x0, x21
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    // Exit successfully
    mov x0, #0
    mov x16, #1
    svc #0

exit_error:
    mov x0, #1
    mov x16, #1
    svc #0

// ============================================================
// parse_input: Parse the grid and instructions
// ============================================================
parse_input:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    adrp x19, buffer@PAGE
    add x19, x19, buffer@PAGEOFF           // x19 = buffer ptr
    adrp x20, grid@PAGE
    add x20, x20, grid@PAGEOFF             // x20 = grid ptr

    // First, fill grid with spaces
    mov x0, x20
    mov x1, #MAX_WIDTH
    mov x2, #MAX_HEIGHT
    mul x1, x1, x2
fill_grid:
    cmp x1, #0
    b.eq fill_done
    mov w2, #' '
    strb w2, [x0], #1
    sub x1, x1, #1
    b fill_grid
fill_done:

    mov x21, #0                 // row count
    mov x22, #0                 // max width
    mov x23, #0                 // current column
    mov x24, x19                // current buffer position

    // Parse grid
parse_grid_loop:
    ldrb w0, [x24]

    // Check for double newline (end of grid)
    cmp w0, #10                 // newline
    b.ne not_newline

    // Check if next is also newline
    ldrb w1, [x24, #1]
    cmp w1, #10
    b.eq end_grid_section

    // End of row - update max width and move to next row
    cmp x23, x22
    csel x22, x23, x22, gt      // max_width = max(max_width, current_col)
    add x21, x21, #1            // row++
    mov x23, #0                 // col = 0
    add x24, x24, #1
    b parse_grid_loop

not_newline:
    // Store character in grid: grid[row * MAX_WIDTH + col] = char
    mov x25, x21
    mov x26, #MAX_WIDTH
    mul x25, x25, x26
    add x25, x25, x23           // offset = row * MAX_WIDTH + col
    strb w0, [x20, x25]

    add x23, x23, #1            // col++
    add x24, x24, #1            // buffer++
    b parse_grid_loop

end_grid_section:
    // Handle last row if it had content
    cmp x23, #0
    b.eq skip_last_row_check
    cmp x23, x22
    csel x22, x23, x22, gt
    add x21, x21, #1
skip_last_row_check:

    // Store dimensions
    adrp x0, grid_width@PAGE
    add x0, x0, grid_width@PAGEOFF
    str x22, [x0]

    adrp x0, grid_height@PAGE
    add x0, x0, grid_height@PAGEOFF
    str x21, [x0]

    // Skip to instructions (past double newline)
    add x24, x24, #2

    // Parse instructions
    adrp x25, instructions@PAGE
    add x25, x25, instructions@PAGEOFF
    mov x26, #0                 // instruction count

parse_instr_loop:
    ldrb w0, [x24]

    // Check for end of input
    cmp w0, #0
    b.eq end_parse
    cmp w0, #10
    b.eq end_parse
    cmp w0, #13
    b.eq end_parse

    // Check if it's a digit
    cmp w0, #'0'
    b.lt check_turn
    cmp w0, #'9'
    b.gt check_turn

    // Parse number
    mov x27, #0                 // accumulator
parse_num:
    ldrb w0, [x24]
    cmp w0, #'0'
    b.lt store_num
    cmp w0, #'9'
    b.gt store_num

    // num = num * 10 + digit
    mov x1, #10
    mul x27, x27, x1
    sub w0, w0, #'0'
    add x27, x27, x0
    add x24, x24, #1
    b parse_num

store_num:
    // Store positive number (move forward)
    str x27, [x25, x26, lsl #3]
    add x26, x26, #1
    b parse_instr_loop

check_turn:
    cmp w0, #'R'
    b.eq turn_right
    cmp w0, #'L'
    b.eq turn_left
    add x24, x24, #1
    b parse_instr_loop

turn_right:
    mov x27, #-1                // -1 for right turn
    str x27, [x25, x26, lsl #3]
    add x26, x26, #1
    add x24, x24, #1
    b parse_instr_loop

turn_left:
    mov x27, #-2                // -2 for left turn
    str x27, [x25, x26, lsl #3]
    add x26, x26, #1
    add x24, x24, #1
    b parse_instr_loop

end_parse:
    adrp x0, num_instructions@PAGE
    add x0, x0, num_instructions@PAGEOFF
    str x26, [x0]

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// get_grid_char: Get character at (row, col)
// x0 = row, x1 = col
// Returns char in w0 (space if out of bounds)
// ============================================================
get_grid_char:
    stp x19, x20, [sp, #-16]!

    // Check bounds
    cmp x0, #0
    b.lt return_space

    adrp x2, grid_height@PAGE
    add x2, x2, grid_height@PAGEOFF
    ldr x2, [x2]
    cmp x0, x2
    b.ge return_space

    cmp x1, #0
    b.lt return_space

    adrp x2, grid_width@PAGE
    add x2, x2, grid_width@PAGEOFF
    ldr x2, [x2]
    cmp x1, x2
    b.ge return_space

    // Calculate offset: row * MAX_WIDTH + col
    mov x2, #MAX_WIDTH
    mul x2, x0, x2
    add x2, x2, x1

    adrp x3, grid@PAGE
    add x3, x3, grid@PAGEOFF
    ldrb w0, [x3, x2]
    b done_get_char

return_space:
    mov w0, #' '

done_get_char:
    ldp x19, x20, [sp], #16
    ret

// ============================================================
// find_start: Find starting position (leftmost '.' on row 0)
// Returns: x0 = col
// ============================================================
find_start:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, #0                 // col = 0

find_start_loop:
    mov x0, #0                  // row = 0
    mov x1, x19                 // col
    bl get_grid_char

    cmp w0, #'.'
    b.eq found_start

    add x19, x19, #1
    b find_start_loop

found_start:
    mov x0, x19

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// solve_part1: Part 1 with flat wrapping
// Returns: x0 = password
// ============================================================
solve_part1:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    // Find starting position
    bl find_start
    mov x20, #0                 // row = 0
    mov x21, x0                 // col = starting col
    mov x22, #0                 // facing = 0 (right)

    // Load dimensions
    adrp x23, grid_height@PAGE
    add x23, x23, grid_height@PAGEOFF
    ldr x23, [x23]              // height

    adrp x24, grid_width@PAGE
    add x24, x24, grid_width@PAGEOFF
    ldr x24, [x24]              // width

    // Load instruction count and pointer
    adrp x25, num_instructions@PAGE
    add x25, x25, num_instructions@PAGEOFF
    ldr x25, [x25]

    adrp x26, instructions@PAGE
    add x26, x26, instructions@PAGEOFF

    mov x27, #0                 // instruction index

part1_instr_loop:
    cmp x27, x25
    b.ge part1_done

    ldr x0, [x26, x27, lsl #3]
    add x27, x27, #1

    // Check if turn or move
    cmp x0, #0
    b.lt part1_turn

    // Move forward x0 steps
    mov x28, x0                 // steps remaining

part1_move_loop:
    cmp x28, #0
    b.le part1_instr_loop

    // Get delta for current facing
    adrp x0, dr_array@PAGE
    add x0, x0, dr_array@PAGEOFF
    ldr x1, [x0, x22, lsl #3]   // dr

    adrp x0, dc_array@PAGE
    add x0, x0, dc_array@PAGEOFF
    ldr x2, [x0, x22, lsl #3]   // dc

    // Calculate new position
    add x3, x20, x1             // nr = row + dr
    add x4, x21, x2             // nc = col + dc

    // Check if we need to wrap based on facing
    cmp x22, #0                 // facing right
    b.ne p1_check_facing_1

    // Facing right: check if nc >= width or space
    cmp x4, x24
    b.ge p1_wrap_right
    mov x0, x3
    mov x1, x4
    bl get_grid_char
    cmp w0, #' '
    b.eq p1_wrap_right
    b p1_check_wall

p1_wrap_right:
    mov x4, #0
p1_wrap_right_loop:
    mov x0, x3
    mov x1, x4
    bl get_grid_char
    cmp w0, #' '
    b.ne p1_check_wall
    add x4, x4, #1
    b p1_wrap_right_loop

p1_check_facing_1:
    cmp x22, #1                 // facing down
    b.ne p1_check_facing_2

    // Facing down: check if nr >= height or space
    cmp x3, x23
    b.ge p1_wrap_down
    mov x0, x3
    mov x1, x4
    bl get_grid_char
    cmp w0, #' '
    b.eq p1_wrap_down
    b p1_check_wall

p1_wrap_down:
    mov x3, #0
p1_wrap_down_loop:
    mov x0, x3
    mov x1, x4
    bl get_grid_char
    cmp w0, #' '
    b.ne p1_check_wall
    add x3, x3, #1
    b p1_wrap_down_loop

p1_check_facing_2:
    cmp x22, #2                 // facing left
    b.ne p1_check_facing_3

    // Facing left: check if nc < 0 or space
    cmp x4, #0
    b.lt p1_wrap_left
    mov x0, x3
    mov x1, x4
    bl get_grid_char
    cmp w0, #' '
    b.eq p1_wrap_left
    b p1_check_wall

p1_wrap_left:
    sub x4, x24, #1
p1_wrap_left_loop:
    mov x0, x3
    mov x1, x4
    bl get_grid_char
    cmp w0, #' '
    b.ne p1_check_wall
    sub x4, x4, #1
    b p1_wrap_left_loop

p1_check_facing_3:
    // Facing up: check if nr < 0 or space
    cmp x3, #0
    b.lt p1_wrap_up
    mov x0, x3
    mov x1, x4
    bl get_grid_char
    cmp w0, #' '
    b.eq p1_wrap_up
    b p1_check_wall

p1_wrap_up:
    sub x3, x23, #1
p1_wrap_up_loop:
    mov x0, x3
    mov x1, x4
    bl get_grid_char
    cmp w0, #' '
    b.ne p1_check_wall
    sub x3, x3, #1
    b p1_wrap_up_loop

p1_check_wall:
    // Check if new position is a wall
    mov x0, x3
    mov x1, x4
    bl get_grid_char
    cmp w0, #'#'
    b.eq part1_instr_loop       // Hit wall, stop moving

    // Move to new position
    mov x20, x3
    mov x21, x4
    sub x28, x28, #1
    b part1_move_loop

part1_turn:
    // x0 is -1 (right) or -2 (left) -- need to reload since bl clobbered
    sub x1, x27, #1
    ldr x0, [x26, x1, lsl #3]
    cmp x0, #-1
    b.ne turn_left_p1

    // Turn right: facing = (facing + 1) % 4
    add x22, x22, #1
    and x22, x22, #3
    b part1_instr_loop

turn_left_p1:
    // Turn left: facing = (facing + 3) % 4
    add x22, x22, #3
    and x22, x22, #3
    b part1_instr_loop

part1_done:
    // Calculate password: 1000 * (row + 1) + 4 * (col + 1) + facing
    add x0, x20, #1             // row + 1
    mov x1, #1000
    mul x0, x0, x1

    add x1, x21, #1             // col + 1
    lsl x1, x1, #2              // * 4
    add x0, x0, x1

    add x0, x0, x22             // + facing

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// get_cube_face: Determine which face based on (row, col)
// x0 = row, x1 = col
// Returns: x0 = face (1-6), x1 = local_r, x2 = local_c
// ============================================================
get_cube_face:
    // face_row = row / FACE_SIZE
    // face_col = col / FACE_SIZE
    mov x3, #FACE_SIZE
    udiv x4, x0, x3             // face_row
    udiv x5, x1, x3             // face_col

    // local_r = row % FACE_SIZE
    // local_c = col % FACE_SIZE
    msub x6, x4, x3, x0         // local_r
    msub x7, x5, x3, x1         // local_c

    // Map (face_row, face_col) to face number
    // Layout:
    //   12  (row 0, cols 1,2)
    //   3   (row 1, col 1)
    //  45   (row 2, cols 0,1)
    //  6    (row 3, col 0)

    cmp x4, #0
    b.ne gcf_face_row_1

    // face_row == 0
    cmp x5, #1
    b.ne gcf_face_0_2
    mov x0, #1                  // Face 1: (0,1)
    b gcf_face_found
gcf_face_0_2:
    cmp x5, #2
    b.ne gcf_face_invalid
    mov x0, #2                  // Face 2: (0,2)
    b gcf_face_found

gcf_face_row_1:
    cmp x4, #1
    b.ne gcf_face_row_2
    cmp x5, #1
    b.ne gcf_face_invalid
    mov x0, #3                  // Face 3: (1,1)
    b gcf_face_found

gcf_face_row_2:
    cmp x4, #2
    b.ne gcf_face_row_3
    cmp x5, #0
    b.ne gcf_face_2_1
    mov x0, #4                  // Face 4: (2,0)
    b gcf_face_found
gcf_face_2_1:
    cmp x5, #1
    b.ne gcf_face_invalid
    mov x0, #5                  // Face 5: (2,1)
    b gcf_face_found

gcf_face_row_3:
    cmp x4, #3
    b.ne gcf_face_invalid
    cmp x5, #0
    b.ne gcf_face_invalid
    mov x0, #6                  // Face 6: (3,0)
    b gcf_face_found

gcf_face_invalid:
    mov x0, #-1

gcf_face_found:
    mov x1, x6                  // local_r
    mov x2, x7                  // local_c
    ret

// ============================================================
// wrap_cube: Handle cube wrapping
// x0 = row, x1 = col, x2 = facing
// Returns: x0 = new_row, x1 = new_col, x2 = new_facing
// ============================================================
wrap_cube:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0                 // row
    mov x20, x1                 // col
    mov x21, x2                 // facing

    // Get face and local coordinates
    mov x0, x19
    mov x1, x20
    bl get_cube_face
    mov x22, x0                 // face
    mov x23, x1                 // lr (local_r)
    mov x24, x2                 // lc (local_c)

    // Face 1 transitions
    cmp x22, #1
    b.ne wc_try_face_2

    cmp x21, #3                 // Up
    b.ne wc_face1_left
    // Face 1, Up -> Face 6, left edge, facing right
    // new_row = 3*S + lc, new_col = 0, new_facing = 0
    mov x0, #FACE_SIZE
    mov x1, #3
    mul x0, x0, x1              // 3*S
    add x0, x0, x24             // 3*S + lc
    mov x1, #0
    mov x2, #0
    b wc_wrap_done

wc_face1_left:
    cmp x21, #2                 // Left
    b.ne wc_wrap_unchanged
    // Face 1, Left -> Face 4, left edge, facing right (inverted)
    // new_row = 3*S - 1 - lr, new_col = 0, new_facing = 0
    mov x0, #FACE_SIZE
    mov x1, #3
    mul x0, x0, x1              // 3*S
    sub x0, x0, #1              // 3*S - 1
    sub x0, x0, x23             // 3*S - 1 - lr
    mov x1, #0
    mov x2, #0
    b wc_wrap_done

wc_try_face_2:
    cmp x22, #2
    b.ne wc_try_face_3

    cmp x21, #0                 // Right
    b.ne wc_face2_down
    // Face 2, Right -> Face 5, right edge, facing left (inverted)
    // new_row = 3*S - 1 - lr, new_col = 2*S - 1, new_facing = 2
    mov x0, #FACE_SIZE
    mov x1, #3
    mul x0, x0, x1              // 3*S
    sub x0, x0, #1              // 3*S - 1
    sub x0, x0, x23             // 3*S - 1 - lr
    mov x1, #FACE_SIZE
    lsl x1, x1, #1              // 2*S
    sub x1, x1, #1              // 2*S - 1
    mov x2, #2
    b wc_wrap_done

wc_face2_down:
    cmp x21, #1                 // Down
    b.ne wc_face2_up
    // Face 2, Down -> Face 3, right edge, facing left
    // new_row = S + lc, new_col = 2*S - 1, new_facing = 2
    mov x0, #FACE_SIZE
    add x0, x0, x24             // S + lc
    mov x1, #FACE_SIZE
    lsl x1, x1, #1              // 2*S
    sub x1, x1, #1              // 2*S - 1
    mov x2, #2
    b wc_wrap_done

wc_face2_up:
    cmp x21, #3                 // Up
    b.ne wc_wrap_unchanged
    // Face 2, Up -> Face 6, bottom, facing up
    // new_row = 4*S - 1, new_col = lc, new_facing = 3
    mov x0, #FACE_SIZE
    lsl x0, x0, #2              // 4*S
    sub x0, x0, #1              // 4*S - 1
    mov x1, x24                 // lc
    mov x2, #3
    b wc_wrap_done

wc_try_face_3:
    cmp x22, #3
    b.ne wc_try_face_4

    cmp x21, #0                 // Right
    b.ne wc_face3_left
    // Face 3, Right -> Face 2, bottom, facing up
    // new_row = S - 1, new_col = 2*S + lr, new_facing = 3
    mov x0, #FACE_SIZE
    sub x0, x0, #1              // S - 1
    mov x1, #FACE_SIZE
    lsl x1, x1, #1              // 2*S
    add x1, x1, x23             // 2*S + lr
    mov x2, #3
    b wc_wrap_done

wc_face3_left:
    cmp x21, #2                 // Left
    b.ne wc_wrap_unchanged
    // Face 3, Left -> Face 4, top, facing down
    // new_row = 2*S, new_col = lr, new_facing = 1
    mov x0, #FACE_SIZE
    lsl x0, x0, #1              // 2*S
    mov x1, x23                 // lr
    mov x2, #1
    b wc_wrap_done

wc_try_face_4:
    cmp x22, #4
    b.ne wc_try_face_5

    cmp x21, #3                 // Up
    b.ne wc_face4_left
    // Face 4, Up -> Face 3, left edge, facing right
    // new_row = S + lc, new_col = S, new_facing = 0
    mov x0, #FACE_SIZE
    add x0, x0, x24             // S + lc
    mov x1, #FACE_SIZE          // S
    mov x2, #0
    b wc_wrap_done

wc_face4_left:
    cmp x21, #2                 // Left
    b.ne wc_wrap_unchanged
    // Face 4, Left -> Face 1, left edge, facing right (inverted)
    // new_row = S - 1 - lr, new_col = S, new_facing = 0
    mov x0, #FACE_SIZE
    sub x0, x0, #1              // S - 1
    sub x0, x0, x23             // S - 1 - lr
    mov x1, #FACE_SIZE          // S
    mov x2, #0
    b wc_wrap_done

wc_try_face_5:
    cmp x22, #5
    b.ne wc_try_face_6

    cmp x21, #0                 // Right
    b.ne wc_face5_down
    // Face 5, Right -> Face 2, right edge, facing left (inverted)
    // new_row = S - 1 - lr, new_col = 3*S - 1, new_facing = 2
    mov x0, #FACE_SIZE
    sub x0, x0, #1              // S - 1
    sub x0, x0, x23             // S - 1 - lr
    mov x1, #FACE_SIZE
    mov x3, #3
    mul x1, x1, x3              // 3*S
    sub x1, x1, #1              // 3*S - 1
    mov x2, #2
    b wc_wrap_done

wc_face5_down:
    cmp x21, #1                 // Down
    b.ne wc_wrap_unchanged
    // Face 5, Down -> Face 6, right edge, facing left
    // new_row = 3*S + lc, new_col = S - 1, new_facing = 2
    mov x0, #FACE_SIZE
    mov x3, #3
    mul x0, x0, x3              // 3*S
    add x0, x0, x24             // 3*S + lc
    mov x1, #FACE_SIZE
    sub x1, x1, #1              // S - 1
    mov x2, #2
    b wc_wrap_done

wc_try_face_6:
    cmp x22, #6
    b.ne wc_wrap_unchanged

    cmp x21, #0                 // Right
    b.ne wc_face6_down
    // Face 6, Right -> Face 5, bottom, facing up
    // new_row = 3*S - 1, new_col = S + lr, new_facing = 3
    mov x0, #FACE_SIZE
    mov x3, #3
    mul x0, x0, x3              // 3*S
    sub x0, x0, #1              // 3*S - 1
    mov x1, #FACE_SIZE
    add x1, x1, x23             // S + lr
    mov x2, #3
    b wc_wrap_done

wc_face6_down:
    cmp x21, #1                 // Down
    b.ne wc_face6_left
    // Face 6, Down -> Face 2, top, facing down
    // new_row = 0, new_col = 2*S + lc, new_facing = 1
    mov x0, #0
    mov x1, #FACE_SIZE
    lsl x1, x1, #1              // 2*S
    add x1, x1, x24             // 2*S + lc
    mov x2, #1
    b wc_wrap_done

wc_face6_left:
    cmp x21, #2                 // Left
    b.ne wc_wrap_unchanged
    // Face 6, Left -> Face 1, top, facing down
    // new_row = 0, new_col = S + lr, new_facing = 1
    mov x0, #0
    mov x1, #FACE_SIZE
    add x1, x1, x23             // S + lr
    mov x2, #1
    b wc_wrap_done

wc_wrap_unchanged:
    mov x0, x19
    mov x1, x20
    mov x2, x21

wc_wrap_done:
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// solve_part2: Part 2 with cube wrapping
// Returns: x0 = password
// ============================================================
solve_part2:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    // Find starting position
    bl find_start
    mov x19, #0                 // row = 0
    mov x20, x0                 // col = starting col
    mov x21, #0                 // facing = 0 (right)

    // Load dimensions
    adrp x22, grid_height@PAGE
    add x22, x22, grid_height@PAGEOFF
    ldr x22, [x22]              // height

    adrp x23, grid_width@PAGE
    add x23, x23, grid_width@PAGEOFF
    ldr x23, [x23]              // width

    // Load instruction count and pointer
    adrp x24, num_instructions@PAGE
    add x24, x24, num_instructions@PAGEOFF
    ldr x24, [x24]

    adrp x25, instructions@PAGE
    add x25, x25, instructions@PAGEOFF

    mov x26, #0                 // instruction index

p2_instr_loop:
    cmp x26, x24
    b.ge p2_done

    ldr x0, [x25, x26, lsl #3]
    add x26, x26, #1

    // Check if turn or move
    cmp x0, #0
    b.lt p2_turn

    // Move forward x0 steps
    mov x27, x0                 // steps remaining

p2_move_loop:
    cmp x27, #0
    b.le p2_instr_loop

    // Get delta for current facing
    adrp x0, dr_array@PAGE
    add x0, x0, dr_array@PAGEOFF
    ldr x1, [x0, x21, lsl #3]   // dr

    adrp x0, dc_array@PAGE
    add x0, x0, dc_array@PAGEOFF
    ldr x2, [x0, x21, lsl #3]   // dc

    // Calculate new position
    add x3, x19, x1             // nr = row + dr
    add x4, x20, x2             // nc = col + dc
    mov x5, x21                 // nf = facing

    // Check if we need to wrap
    mov x28, #0                 // need_wrap flag

    // Check row bounds
    cmp x3, #0
    b.lt p2_set_need_wrap
    cmp x3, x22
    b.ge p2_set_need_wrap

    // Check col bounds
    cmp x4, #0
    b.lt p2_set_need_wrap
    cmp x4, x23
    b.ge p2_set_need_wrap

    // Check if space
    mov x0, x3
    mov x1, x4
    bl get_grid_char
    cmp w0, #' '
    b.eq p2_set_need_wrap
    b p2_skip_wrap_check

p2_set_need_wrap:
    mov x28, #1

p2_skip_wrap_check:
    cmp x28, #0
    b.eq p2_check_wall

    // Need to wrap - use cube wrapping
    mov x0, x19                 // current row
    mov x1, x20                 // current col
    mov x2, x21                 // current facing
    bl wrap_cube
    mov x3, x0                  // new row
    mov x4, x1                  // new col
    mov x5, x2                  // new facing

p2_check_wall:
    // Check if new position is a wall
    mov x0, x3
    mov x1, x4
    bl get_grid_char
    cmp w0, #'#'
    b.eq p2_instr_loop          // Hit wall, stop moving

    // Move to new position
    mov x19, x3
    mov x20, x4
    mov x21, x5
    sub x27, x27, #1
    b p2_move_loop

p2_turn:
    // Reload instruction since bl clobbered x0
    sub x1, x26, #1
    ldr x0, [x25, x1, lsl #3]
    cmp x0, #-1
    b.ne p2_turn_left

    // Turn right: facing = (facing + 1) % 4
    add x21, x21, #1
    and x21, x21, #3
    b p2_instr_loop

p2_turn_left:
    // Turn left: facing = (facing + 3) % 4
    add x21, x21, #3
    and x21, x21, #3
    b p2_instr_loop

p2_done:
    // Calculate password: 1000 * (row + 1) + 4 * (col + 1) + facing
    add x0, x19, #1             // row + 1
    mov x1, #1000
    mul x0, x0, x1

    add x1, x20, #1             // col + 1
    lsl x1, x1, #2              // * 4
    add x0, x0, x1

    add x0, x0, x21             // + facing

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// print_string: Print null-terminated string
// x0 = string pointer
// ============================================================
print_string:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x0

    // Find string length
    mov x20, #0
ps_strlen_loop:
    ldrb w1, [x19, x20]
    cmp w1, #0
    b.eq ps_strlen_done
    add x20, x20, #1
    b ps_strlen_loop

ps_strlen_done:
    mov x0, #1                  // stdout
    mov x1, x19
    mov x2, x20
    mov x16, #4                 // write syscall
    svc #0

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// print_number: Print integer
// x0 = number to print
// ============================================================
print_number:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    adrp x19, num_buf@PAGE
    add x19, x19, num_buf@PAGEOFF

    mov x1, x0
    add x0, x19, #20            // Start from end of buffer
    mov x2, #0                  // digit count

    // Handle zero specially
    cmp x1, #0
    b.ne pn_convert_loop
    mov w3, #'0'
    sub x0, x0, #1
    strb w3, [x0]
    mov x2, #1
    b pn_print_num_str

pn_convert_loop:
    cmp x1, #0
    b.eq pn_print_num_str

    mov x3, #10
    udiv x4, x1, x3             // x4 = x1 / 10
    msub x5, x4, x3, x1         // x5 = x1 - (x4 * 10) = x1 % 10

    add w5, w5, #'0'
    sub x0, x0, #1
    strb w5, [x0]
    add x2, x2, #1
    mov x1, x4
    b pn_convert_loop

pn_print_num_str:
    mov x1, x0
    mov x0, #1                  // stdout
    mov x16, #4                 // write syscall
    svc #0

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
