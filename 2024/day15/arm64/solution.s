.global _main
.align 4

.equ MAX_GRID_SIZE, 5120      // 50x100 max for Part 2

.data
filename:   .asciz "../input.txt"
read_mode:  .asciz "r"
format_p1:  .asciz "Part 1: %lld\n"
format_p2:  .asciz "Part 2: %lld\n"

.bss
.align 8
file_buffer:    .skip 65536
grid1:          .skip MAX_GRID_SIZE
grid2:          .skip MAX_GRID_SIZE
moves:          .skip 32768
grid_width:     .skip 8
grid_height:    .skip 8
robot_r:        .skip 8
robot_c:        .skip 8
move_count:     .skip 8
box_buffer:     .skip 8192      // Buffer for collecting boxes (row, col pairs)
box_count:      .skip 8

.text

_main:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    bl      read_file
    bl      part1
    bl      part2

    mov     w0, #0
    ldp     x29, x30, [sp], #16
    ret

read_file:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]

    adrp    x0, filename@PAGE
    add     x0, x0, filename@PAGEOFF
    adrp    x1, read_mode@PAGE
    add     x1, x1, read_mode@PAGEOFF
    bl      _fopen
    mov     x19, x0

    adrp    x0, file_buffer@PAGE
    add     x0, x0, file_buffer@PAGEOFF
    mov     x20, x0
    mov     x1, #1
    mov     x2, #65536
    mov     x3, x19
    bl      _fread

    add     x1, x20, x0
    strb    wzr, [x1]

    mov     x0, x19
    bl      _fclose

    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// Parse input into grid and moves
parse_input:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    // x0 = grid destination
    mov     x19, x0          // grid ptr
    adrp    x20, file_buffer@PAGE
    add     x20, x20, file_buffer@PAGEOFF
    mov     x21, #0          // height
    mov     x22, #0          // width

parse_grid_loop:
    ldrb    w23, [x20]
    cbz     w23, parse_done
    cmp     w23, #'\n'
    b.eq    parse_grid_newline

    // Check for blank line (grid end)
    cmp     w23, #'\n'
    b.ne    1f
    add     x20, x20, #1
    ldrb    w23, [x20]
    cmp     w23, #'\n'
    b.eq    parse_moves_start
    sub     x20, x20, #1
    mov     w23, #'\n'

1:  // Store character
    strb    w23, [x19], #1
    add     x20, x20, #1
    add     x22, x22, #1
    b       parse_grid_loop

parse_grid_newline:
    // Check if next is newline (blank line)
    add     x20, x20, #1
    ldrb    w23, [x20]
    cmp     w23, #'\n'
    b.eq    parse_moves_start

    // Regular newline
    cmp     x22, #0
    b.eq    parse_grid_loop

    // Store width from first line
    adrp    x24, grid_width@PAGE
    add     x24, x24, grid_width@PAGEOFF
    ldr     x23, [x24]
    cbnz    x23, 2f
    str     x22, [x24]

2:  add     x21, x21, #1
    mov     x22, #0
    b       parse_grid_loop

parse_moves_start:
    // Store height
    cmp     x22, #0
    b.eq    1f
    add     x21, x21, #1
1:  adrp    x24, grid_height@PAGE
    add     x24, x24, grid_height@PAGEOFF
    str     x21, [x24]

    // Skip blank line
    add     x20, x20, #1

    // Parse moves
    adrp    x19, moves@PAGE
    add     x19, x19, moves@PAGEOFF
    mov     x21, #0

parse_moves_loop:
    ldrb    w22, [x20]
    cbz     w22, parse_moves_done

    cmp     w22, #'\n'
    b.eq    2f
    cmp     w22, #' '
    b.eq    2f

    strb    w22, [x19], #1
    add     x21, x21, #1

2:  add     x20, x20, #1
    b       parse_moves_loop

parse_moves_done:
    adrp    x24, move_count@PAGE
    add     x24, x24, move_count@PAGEOFF
    str     x21, [x24]

parse_done:
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// Find robot position
find_robot:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    // x0 = grid
    mov     x19, x0
    adrp    x20, grid_height@PAGE
    add     x20, x20, grid_height@PAGEOFF
    ldr     x20, [x20]
    adrp    x21, grid_width@PAGE
    add     x21, x21, grid_width@PAGEOFF
    ldr     x21, [x21]

    mov     x2, #0           // row
find_r_loop:
    cmp     x2, x20
    b.ge    find_done
    mov     x3, #0           // col

find_c_loop:
    cmp     x3, x21
    b.ge    find_r_next

    // Calculate offset
    mul     x4, x2, x21
    add     x4, x4, x3
    ldrb    w5, [x19, x4]
    cmp     w5, #'@'
    b.ne    1f

    // Found robot
    adrp    x22, robot_r@PAGE
    add     x22, x22, robot_r@PAGEOFF
    str     x2, [x22]
    adrp    x22, robot_c@PAGE
    add     x22, x22, robot_c@PAGEOFF
    str     x3, [x22]
    b       find_done

1:  add     x3, x3, #1
    b       find_c_loop

find_r_next:
    add     x2, x2, #1
    b       find_r_loop

find_done:
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// Get grid cell value
// x0 = grid, x1 = row, x2 = col
// Returns character in w0
get_cell:
    adrp    x3, grid_width@PAGE
    add     x3, x3, grid_width@PAGEOFF
    ldr     x3, [x3]
    mul     x4, x1, x3
    add     x4, x4, x2
    ldrb    w0, [x0, x4]
    ret

// Set grid cell
// x0 = grid, x1 = row, x2 = col, w3 = char
set_cell:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    adrp    x4, grid_width@PAGE
    add     x4, x4, grid_width@PAGEOFF
    ldr     x4, [x4]
    mul     x5, x1, x4
    add     x5, x5, x2
    strb    w3, [x0, x5]

    ldp     x29, x30, [sp], #16
    ret

// Move robot (Part 1)
// x0 = grid, w1 = direction char
move_robot:
    sub     sp, sp, #112
    stp     x29, x30, [sp, #96]
    add     x29, sp, #96
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    mov     x19, x0          // grid

    // Get direction deltas
    mov     x20, #0          // dr
    mov     x21, #0          // dc

    cmp     w1, #'<'
    b.ne    1f
    mov     x21, #-1
    b       2f
1:  cmp     w1, #'>'
    b.ne    1f
    mov     x21, #1
    b       2f
1:  cmp     w1, #'^'
    b.ne    1f
    mov     x20, #-1
    b       2f
1:  mov     x20, #1

2:  // Get current robot position
    adrp    x22, robot_r@PAGE
    add     x22, x22, robot_r@PAGEOFF
    ldr     x22, [x22]
    adrp    x23, robot_c@PAGE
    add     x23, x23, robot_c@PAGEOFF
    ldr     x23, [x23]

    // Calculate next position
    add     x24, x22, x20    // nr
    add     x25, x23, x21    // nc

    // Check next cell
    mov     x0, x19
    mov     x1, x24
    mov     x2, x25
    bl      get_cell
    mov     w26, w0

    // If wall, don't move
    cmp     w26, #'#'
    b.eq    move_done

    // If empty, move
    cmp     w26, #'.'
    b.ne    move_box

    // Move robot to empty space
    mov     x0, x19
    mov     x1, x22
    mov     x2, x23
    mov     w3, #'.'
    bl      set_cell

    mov     x0, x19
    mov     x1, x24
    mov     x2, x25
    mov     w3, #'@'
    bl      set_cell

    adrp    x0, robot_r@PAGE
    add     x0, x0, robot_r@PAGEOFF
    str     x24, [x0]
    adrp    x0, robot_c@PAGE
    add     x0, x0, robot_c@PAGEOFF
    str     x25, [x0]
    b       move_done

move_box:
    // Check if it's a box
    cmp     w26, #'O'
    b.ne    move_done

    // Find end of box chain
    mov     x27, x24         // check_r
    mov     x28, x25         // check_c

find_end:
    mov     x0, x19
    mov     x1, x27
    mov     x2, x28
    bl      get_cell
    cmp     w0, #'O'
    b.ne    found_end

    add     x27, x27, x20
    add     x28, x28, x21
    b       find_end

found_end:
    // Check if wall
    cmp     w0, #'#'
    b.eq    move_done

    // Move boxes
    mov     x0, x19
    mov     x1, x27
    mov     x2, x28
    mov     w3, #'O'
    bl      set_cell

    mov     x0, x19
    mov     x1, x22
    mov     x2, x23
    mov     w3, #'.'
    bl      set_cell

    mov     x0, x19
    mov     x1, x24
    mov     x2, x25
    mov     w3, #'@'
    bl      set_cell

    adrp    x0, robot_r@PAGE
    add     x0, x0, robot_r@PAGEOFF
    str     x24, [x0]
    adrp    x0, robot_c@PAGE
    add     x0, x0, robot_c@PAGEOFF
    str     x25, [x0]

move_done:
    ldp     x27, x28, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp, #96]
    add     sp, sp, #112
    ret

// Calculate GPS sum
// x0 = grid, w1 = box char
calc_gps:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x19, x0          // grid
    mov     w20, w1          // box char
    mov     x21, #0          // total

    adrp    x22, grid_height@PAGE
    add     x22, x22, grid_height@PAGEOFF
    ldr     x22, [x22]
    adrp    x23, grid_width@PAGE
    add     x23, x23, grid_width@PAGEOFF
    ldr     x23, [x23]

    mov     x1, #0           // row

gps_r_loop:
    cmp     x1, x22
    b.ge    gps_done
    mov     x2, #0           // col

gps_c_loop:
    cmp     x2, x23
    b.ge    gps_r_next

    mov     x0, x19
    stp     x1, x2, [sp, #-16]!
    bl      get_cell
    ldp     x1, x2, [sp], #16

    cmp     w0, w20
    b.ne    1f

    // Calculate GPS: 100 * r + c
    mov     x24, #100
    mul     x24, x1, x24
    add     x24, x24, x2
    add     x21, x21, x24

1:  add     x2, x2, #1
    b       gps_c_loop

gps_r_next:
    add     x1, x1, #1
    b       gps_r_loop

gps_done:
    mov     x0, x21

    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// Copy grid
// x0 = dest, x1 = src
copy_grid:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]

    adrp    x19, grid_height@PAGE
    add     x19, x19, grid_height@PAGEOFF
    ldr     x19, [x19]
    adrp    x20, grid_width@PAGE
    add     x20, x20, grid_width@PAGEOFF
    ldr     x20, [x20]

    mul     x2, x19, x20

copy_loop:
    cbz     x2, copy_done
    ldrb    w3, [x1], #1
    strb    w3, [x0], #1
    sub     x2, x2, #1
    b       copy_loop

copy_done:
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// Part 1
part1:
    sub     sp, sp, #80
    stp     x29, x30, [sp, #64]
    add     x29, sp, #64
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    str     x23, [sp, #48]

    // Parse input into grid1
    adrp    x0, grid1@PAGE
    add     x0, x0, grid1@PAGEOFF
    bl      parse_input

    // Find robot
    adrp    x0, grid1@PAGE
    add     x0, x0, grid1@PAGEOFF
    bl      find_robot

    // Process moves
    adrp    x19, moves@PAGE
    add     x19, x19, moves@PAGEOFF
    adrp    x20, move_count@PAGE
    add     x20, x20, move_count@PAGEOFF
    ldr     x20, [x20]
    mov     x21, #0

p1_move_loop:
    cmp     x21, x20
    b.ge    p1_calc

    ldrb    w1, [x19, x21]
    adrp    x0, grid1@PAGE
    add     x0, x0, grid1@PAGEOFF
    bl      move_robot

    add     x21, x21, #1
    b       p1_move_loop

p1_calc:
    adrp    x0, grid1@PAGE
    add     x0, x0, grid1@PAGEOFF
    mov     w1, #'O'
    bl      calc_gps

    str     x0, [sp]
    adrp    x0, format_p1@PAGE
    add     x0, x0, format_p1@PAGEOFF
    bl      _printf

    str     x23, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp, #64]
    add     sp, sp, #80
    ret

// Scale grid 2x wide for Part 2
// x0 = src grid, x1 = dest grid
scale_grid:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x19, x0          // src
    mov     x20, x1          // dest

    adrp    x21, grid_height@PAGE
    add     x21, x21, grid_height@PAGEOFF
    ldr     x21, [x21]
    adrp    x22, grid_width@PAGE
    add     x22, x22, grid_width@PAGEOFF
    ldr     x22, [x22]

    mov     x23, #0          // row

scale_row_loop:
    cmp     x23, x21
    b.ge    scale_done
    mov     x24, #0          // src col

scale_col_loop:
    cmp     x24, x22
    b.ge    scale_row_next

    // Get source char
    mul     x2, x23, x22
    add     x2, x2, x24
    ldrb    w3, [x19, x2]

    // Calculate dest offset (row * width*2 + col*2)
    mov     x4, x22
    lsl     x4, x4, #1       // width * 2
    mul     x5, x23, x4
    mov     x6, x24
    lsl     x6, x6, #1
    add     x5, x5, x6

    // Scale character
    cmp     w3, #'#'
    b.ne    1f
    mov     w6, #'#'
    strb    w6, [x20, x5]
    add     x5, x5, #1
    strb    w6, [x20, x5]
    b       scale_next

1:  cmp     w3, #'O'
    b.ne    2f
    mov     w6, #'['
    strb    w6, [x20, x5]
    add     x5, x5, #1
    mov     w6, #']'
    strb    w6, [x20, x5]
    b       scale_next

2:  cmp     w3, #'.'
    b.ne    3f
    mov     w6, #'.'
    strb    w6, [x20, x5]
    add     x5, x5, #1
    strb    w6, [x20, x5]
    b       scale_next

3:  cmp     w3, #'@'
    b.ne    4f
    mov     w6, #'@'
    strb    w6, [x20, x5]
    add     x5, x5, #1
    mov     w6, #'.'
    strb    w6, [x20, x5]
    b       scale_next

4:  // Unknown char, just duplicate
    strb    w3, [x20, x5]
    add     x5, x5, #1
    strb    w3, [x20, x5]

scale_next:
    add     x24, x24, #1
    b       scale_col_loop

scale_row_next:
    add     x23, x23, #1
    b       scale_row_loop

scale_done:
    // Update width to 2x
    adrp    x0, grid_width@PAGE
    add     x0, x0, grid_width@PAGEOFF
    ldr     x1, [x0]
    lsl     x1, x1, #1
    str     x1, [x0]

    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// Check if box at (x1=row, x2=left_col) can move vertically
// x0 = grid, x1 = box_row, x2 = box_left_col, x3 = dr (direction)
// Returns 1 if can move, 0 if blocked
can_move_box_vertical:
    stp     x29, x30, [sp, #-96]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    str     x27, [sp, #80]

    mov     x19, x0          // grid
    mov     x20, x1          // box_row
    mov     x21, x2          // box_left_col
    mov     x22, x3          // dr

    // Calculate next row
    add     x23, x20, x22    // nr = box_row + dr

    // Check left cell (nr, box_left_col)
    mov     x0, x19
    mov     x1, x23
    mov     x2, x21
    bl      get_cell
    mov     w24, w0          // left_target

    // Check right cell (nr, box_left_col + 1)
    mov     x0, x19
    mov     x1, x23
    add     x2, x21, #1
    bl      get_cell
    mov     w25, w0          // right_target

    // If either is wall, can't move
    cmp     w24, #'#'
    b.eq    cmv_blocked
    cmp     w25, #'#'
    b.eq    cmv_blocked

    // Check if left_target is box part
    mov     x26, #0          // boxes_checked counter
    cmp     w24, #'['
    b.ne    1f
    // Left target is '[', check if box at (nr, box_left_col) can move
    mov     x0, x19
    mov     x1, x23
    mov     x2, x21
    mov     x3, x22
    bl      can_move_box_vertical
    cbz     x0, cmv_blocked
    add     x26, x26, #1

1:  cmp     w24, #']'
    b.ne    2f
    // Left target is ']', check if box at (nr, box_left_col-1) can move
    mov     x0, x19
    mov     x1, x23
    sub     x2, x21, #1
    mov     x3, x22
    bl      can_move_box_vertical
    cbz     x0, cmv_blocked
    add     x26, x26, #1

2:  cmp     w25, #'['
    b.ne    3f
    // Right target is '[', check if box at (nr, box_left_col+1) can move
    mov     x0, x19
    mov     x1, x23
    add     x2, x21, #1
    mov     x3, x22
    bl      can_move_box_vertical
    cbz     x0, cmv_blocked
    add     x26, x26, #1

3:  cmp     w25, #']'
    b.ne    4f
    // Right target is ']', check if box at (nr, box_left_col) can move
    // Only check if we haven't already checked this box from left_target
    cmp     w24, #'['
    b.eq    4f
    mov     x0, x19
    mov     x1, x23
    mov     x2, x21
    mov     x3, x22
    bl      can_move_box_vertical
    cbz     x0, cmv_blocked

4:  // Can move
    mov     x0, #1
    b       cmv_done

cmv_blocked:
    mov     x0, #0

cmv_done:
    str     x27, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #96
    ret

// Collect boxes that need to move when pushing box at (x1=row, x2=left_col)
// x0 = grid, x1 = box_row, x2 = box_left_col, x3 = dr
collect_boxes_vertical:
    stp     x29, x30, [sp, #-96]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    mov     x19, x0          // grid
    mov     x20, x1          // box_row
    mov     x21, x2          // box_left_col
    mov     x22, x3          // dr

    // Check if already in buffer
    adrp    x23, box_buffer@PAGE
    add     x23, x23, box_buffer@PAGEOFF
    adrp    x24, box_count@PAGE
    add     x24, x24, box_count@PAGEOFF
    ldr     x25, [x24]       // box_count

    // Check if (box_row, box_left_col) already exists
    mov     x26, #0
cbv_check_loop:
    cmp     x26, x25
    b.ge    cbv_add_box
    lsl     x27, x26, #4     // offset = index * 16
    ldr     x28, [x23, x27]  // row
    cmp     x28, x20
    b.ne    1f
    add     x27, x27, #8
    ldr     x28, [x23, x27]  // col
    cmp     x28, x21
    b.eq    cbv_recurse      // Already in buffer, skip add
1:  add     x26, x26, #1
    b       cbv_check_loop

cbv_add_box:
    // Add to buffer
    lsl     x27, x25, #4
    str     x20, [x23, x27]
    add     x27, x27, #8
    str     x21, [x23, x27]
    add     x25, x25, #1
    str     x25, [x24]

cbv_recurse:
    // Calculate next row
    add     x23, x20, x22    // nr = box_row + dr

    // Check left cell
    mov     x0, x19
    mov     x1, x23
    mov     x2, x21
    bl      get_cell
    mov     w24, w0          // left_target

    // Check right cell
    mov     x0, x19
    mov     x1, x23
    add     x2, x21, #1
    bl      get_cell
    mov     w25, w0          // right_target

    // Collect boxes in left target
    cmp     w24, #'['
    b.ne    1f
    mov     x0, x19
    mov     x1, x23
    mov     x2, x21
    mov     x3, x22
    bl      collect_boxes_vertical

1:  cmp     w24, #']'
    b.ne    2f
    mov     x0, x19
    mov     x1, x23
    sub     x2, x21, #1
    mov     x3, x22
    bl      collect_boxes_vertical

2:  cmp     w25, #'['
    b.ne    3f
    mov     x0, x19
    mov     x1, x23
    add     x2, x21, #1
    mov     x3, x22
    bl      collect_boxes_vertical

3:  cmp     w25, #']'
    b.ne    cbv_done
    // Only if not same box as left
    cmp     w24, #'['
    b.eq    cbv_done
    mov     x0, x19
    mov     x1, x23
    mov     x2, x21
    mov     x3, x22
    bl      collect_boxes_vertical

cbv_done:
    ldp     x27, x28, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #96
    ret

// Move robot in wide grid (Part 2)
// x0 = grid, w1 = direction
move_robot_wide:
    sub     sp, sp, #112
    stp     x29, x30, [sp, #96]
    add     x29, sp, #96
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    mov     x19, x0

    // Get direction
    mov     x20, #0          // dr
    mov     x21, #0          // dc

    cmp     w1, #'<'
    b.ne    1f
    mov     x21, #-1
    b       2f
1:  cmp     w1, #'>'
    b.ne    1f
    mov     x21, #1
    b       2f
1:  cmp     w1, #'^'
    b.ne    1f
    mov     x20, #-1
    b       2f
1:  mov     x20, #1

2:  // Get robot position
    adrp    x22, robot_r@PAGE
    add     x22, x22, robot_r@PAGEOFF
    ldr     x22, [x22]
    adrp    x23, robot_c@PAGE
    add     x23, x23, robot_c@PAGEOFF
    ldr     x23, [x23]

    add     x24, x22, x20    // nr
    add     x25, x23, x21    // nc

    // Get target cell
    mov     x0, x19
    mov     x1, x24
    mov     x2, x25
    bl      get_cell
    mov     w26, w0

    // Wall check
    cmp     w26, #'#'
    b.eq    move_w_done

    // Empty check
    cmp     w26, #'.'
    b.ne    move_w_box

    // Move to empty
    mov     x0, x19
    mov     x1, x22
    mov     x2, x23
    mov     w3, #'.'
    bl      set_cell

    mov     x0, x19
    mov     x1, x24
    mov     x2, x25
    mov     w3, #'@'
    bl      set_cell

    adrp    x0, robot_r@PAGE
    add     x0, x0, robot_r@PAGEOFF
    str     x24, [x0]
    adrp    x0, robot_c@PAGE
    add     x0, x0, robot_c@PAGEOFF
    str     x25, [x0]
    b       move_w_done

move_w_box:
    // Check for box parts
    cmp     w26, #'['
    b.eq    move_w_has_box
    cmp     w26, #']'
    b.eq    move_w_has_box
    b       move_w_done

move_w_has_box:
    // Check if vertical movement
    cbnz    x20, move_w_vertical

    // Horizontal movement - find end of box chain
    mov     x27, x25         // check_c

find_w_end:
    mov     x0, x19
    mov     x1, x24
    mov     x2, x27
    bl      get_cell

    cmp     w0, #'['
    b.eq    1f
    cmp     w0, #']'
    b.ne    found_w_end
1:  add     x27, x27, x21
    b       find_w_end

found_w_end:
    // Check if wall
    cmp     w0, #'#'
    b.eq    move_w_done

    // Shift boxes
    cmp     x21, #0
    b.lt    shift_left

shift_right:
    mov     x2, x27
shift_r_loop:
    cmp     x2, x25
    b.le    shift_r_done

    mov     x0, x19
    mov     x1, x24
    sub     x3, x2, #1
    stp     x2, x3, [sp, #-16]!
    mov     x2, x3
    bl      get_cell
    ldp     x2, x3, [sp], #16

    mov     w3, w0
    mov     x0, x19
    mov     x1, x24
    bl      set_cell

    sub     x2, x2, #1
    b       shift_r_loop

shift_r_done:
    mov     x0, x19
    mov     x1, x22
    mov     x2, x23
    mov     w3, #'.'
    bl      set_cell

    mov     x0, x19
    mov     x1, x24
    mov     x2, x25
    mov     w3, #'@'
    bl      set_cell

    adrp    x0, robot_r@PAGE
    add     x0, x0, robot_r@PAGEOFF
    str     x24, [x0]
    adrp    x0, robot_c@PAGE
    add     x0, x0, robot_c@PAGEOFF
    str     x25, [x0]
    b       move_w_done

shift_left:
    mov     x2, x27
shift_l_loop:
    cmp     x2, x25
    b.ge    shift_l_done

    mov     x0, x19
    mov     x1, x24
    add     x3, x2, #1
    stp     x2, x3, [sp, #-16]!
    mov     x2, x3
    bl      get_cell
    ldp     x2, x3, [sp], #16

    mov     w3, w0
    mov     x0, x19
    mov     x1, x24
    bl      set_cell

    add     x2, x2, #1
    b       shift_l_loop

shift_l_done:
    mov     x0, x19
    mov     x1, x22
    mov     x2, x23
    mov     w3, #'.'
    bl      set_cell

    mov     x0, x19
    mov     x1, x24
    mov     x2, x25
    mov     w3, #'@'
    bl      set_cell

    adrp    x0, robot_r@PAGE
    add     x0, x0, robot_r@PAGEOFF
    str     x24, [x0]
    adrp    x0, robot_c@PAGE
    add     x0, x0, robot_c@PAGEOFF
    str     x25, [x0]
    b       move_w_done

move_w_vertical:
    // Find left edge of box at target position
    mov     x27, x25         // box_left_c
    cmp     w26, #']'
    b.ne    1f
    sub     x27, x27, #1     // If hit ']', box starts one left

1:  // Clear box buffer
    adrp    x28, box_count@PAGE
    add     x28, x28, box_count@PAGEOFF
    str     xzr, [x28]

    // Check if we can move this box
    mov     x0, x19
    mov     x1, x24          // nr (target row)
    mov     x2, x27          // box_left_c
    mov     x3, x20          // dr
    bl      can_move_box_vertical
    cbz     x0, move_w_done  // If can't move, done

    // Collect all boxes to move
    mov     x0, x19
    mov     x1, x24
    mov     x2, x27
    mov     x3, x20
    bl      collect_boxes_vertical

    // Get box buffer and count
    adrp    x27, box_buffer@PAGE
    add     x27, x27, box_buffer@PAGEOFF
    adrp    x28, box_count@PAGE
    add     x28, x28, box_count@PAGEOFF
    ldr     x28, [x28]

    // Sort boxes by row (bubble sort - simple for small counts)
    // If dr > 0 (moving down), sort descending; if dr < 0 (up), ascending
    cmp     x28, #2
    b.lt    move_boxes_start // Skip sort if < 2 boxes

    sub     x0, x28, #1      // outer loop count
sort_outer:
    cbz     x0, move_boxes_start
    mov     x1, #0           // inner index
sort_inner:
    cmp     x1, x0
    b.ge    sort_outer_next

    // Get box[i] and box[i+1] rows
    lsl     x2, x1, #4
    ldr     x3, [x27, x2]    // box[i].row
    add     x2, x2, #16
    ldr     x4, [x27, x2]    // box[i+1].row

    // Compare based on direction
    cmp     x20, #0
    b.lt    sort_ascending

sort_descending:
    cmp     x3, x4
    b.ge    sort_inner_next  // If row[i] >= row[i+1], ok
    b       sort_swap

sort_ascending:
    cmp     x3, x4
    b.le    sort_inner_next  // If row[i] <= row[i+1], ok

sort_swap:
    // Swap box[i] and box[i+1]
    lsl     x2, x1, #4
    add     x5, x27, x2
    ldr     x3, [x5]         // row i
    ldr     x4, [x5, #8]     // col i
    ldr     x6, [x5, #16]    // row i+1
    ldr     x7, [x5, #24]    // col i+1

    str     x6, [x5]         // Store row i+1 at i
    str     x7, [x5, #8]     // Store col i+1 at i
    str     x3, [x5, #16]    // Store row i at i+1
    str     x4, [x5, #24]    // Store col i at i+1

sort_inner_next:
    add     x1, x1, #1
    b       sort_inner

sort_outer_next:
    sub     x0, x0, #1
    b       sort_outer

move_boxes_start:
    // Move all boxes
    mov     x0, #0           // index
move_boxes_loop:
    cmp     x0, x28
    b.ge    move_boxes_done

    lsl     x1, x0, #4
    ldr     x2, [x27, x1]    // box_row
    add     x1, x1, #8
    ldr     x3, [x27, x1]    // box_col

    // Clear old position
    stp     x0, x2, [sp, #-32]!
    stp     x3, x27, [sp, #16]

    mov     x0, x19
    mov     x1, x2
    mov     x2, x3
    mov     w3, #'.'
    bl      set_cell

    ldp     x3, x27, [sp, #16]
    ldp     x0, x2, [sp], #32

    stp     x0, x2, [sp, #-32]!
    stp     x3, x27, [sp, #16]

    mov     x0, x19
    mov     x1, x2
    add     x2, x3, #1
    mov     w3, #'.'
    bl      set_cell

    ldp     x3, x27, [sp, #16]
    ldp     x0, x2, [sp], #32

    // Place in new position
    stp     x0, x2, [sp, #-32]!
    stp     x3, x27, [sp, #16]

    add     x1, x2, x20      // new_row = box_row + dr
    mov     x0, x19
    mov     x2, x3
    mov     w3, #'['
    bl      set_cell

    ldp     x3, x27, [sp, #16]
    ldp     x0, x2, [sp], #32

    stp     x0, x2, [sp, #-32]!
    stp     x3, x27, [sp, #16]

    add     x1, x2, x20
    mov     x0, x19
    add     x2, x3, #1
    mov     w3, #']'
    bl      set_cell

    ldp     x3, x27, [sp, #16]
    ldp     x0, x2, [sp], #32

    add     x0, x0, #1
    b       move_boxes_loop

move_boxes_done:
    // Move robot
    mov     x0, x19
    mov     x1, x22
    mov     x2, x23
    mov     w3, #'.'
    bl      set_cell

    mov     x0, x19
    mov     x1, x24          // nr
    mov     x2, x25          // nc
    mov     w3, #'@'
    bl      set_cell

    adrp    x0, robot_r@PAGE
    add     x0, x0, robot_r@PAGEOFF
    str     x24, [x0]
    adrp    x0, robot_c@PAGE
    add     x0, x0, robot_c@PAGEOFF
    str     x25, [x0]

move_w_done:
    ldp     x27, x28, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp, #96]
    add     sp, sp, #112
    ret

// Part 2: Scale grid and simulate
part2:
    sub     sp, sp, #96
    stp     x29, x30, [sp, #80]
    add     x29, sp, #80
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    str     x25, [sp, #64]

    // Save original width
    adrp    x19, grid_width@PAGE
    add     x19, x19, grid_width@PAGEOFF
    ldr     x20, [x19]

    // Parse into grid1, scale into grid2
    adrp    x0, grid1@PAGE
    add     x0, x0, grid1@PAGEOFF
    bl      parse_input

    // Restore and save width
    str     x20, [x19]

    // Scale grid
    adrp    x0, grid1@PAGE
    add     x0, x0, grid1@PAGEOFF
    adrp    x1, grid2@PAGE
    add     x1, x1, grid2@PAGEOFF
    bl      scale_grid

    // Find robot in scaled grid
    adrp    x0, grid2@PAGE
    add     x0, x0, grid2@PAGEOFF
    bl      find_robot

    // Process moves
    adrp    x19, moves@PAGE
    add     x19, x19, moves@PAGEOFF
    adrp    x20, move_count@PAGE
    add     x20, x20, move_count@PAGEOFF
    ldr     x20, [x20]
    mov     x21, #0

p2_move_loop:
    cmp     x21, x20
    b.ge    p2_calc

    ldrb    w1, [x19, x21]
    adrp    x0, grid2@PAGE
    add     x0, x0, grid2@PAGEOFF
    bl      move_robot_wide

    add     x21, x21, #1
    b       p2_move_loop

p2_calc:
    adrp    x0, grid2@PAGE
    add     x0, x0, grid2@PAGEOFF
    mov     w1, #'['
    bl      calc_gps

    str     x0, [sp]
    adrp    x0, format_p2@PAGE
    add     x0, x0, format_p2@PAGEOFF
    bl      _printf

    str     x25, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp, #80]
    add     sp, sp, #96
    ret
