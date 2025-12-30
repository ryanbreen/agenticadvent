// Day 16: The Floor Will Be Lava - ARM64 Assembly (macOS)
//
// Part 1: Count energized tiles with beam starting at (0,0) heading right
// Part 2: Find maximum energized tiles from any edge starting position
//
// BFS with visited state tracking (row, col, direction)
// Directions: 0=right, 1=down, 2=left, 3=up

.global _start
.align 4

// Constants
.equ MAX_INPUT_SIZE, 65536
.equ MAX_GRID_SIZE, 150
.equ MAX_QUEUE, 50000

// Macro for loading addresses from data section
.macro LOAD_ADDR reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// ============================================================================
// Data Section
// ============================================================================
.data

// String constants
input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"
error_msg:      .asciz "Error reading file\n"

.align 3
// Grid dimensions
rows:           .quad 0
cols:           .quad 0

// File I/O buffer
file_buffer:    .space MAX_INPUT_SIZE

// Grid storage (char per cell)
grid:           .space MAX_GRID_SIZE * MAX_GRID_SIZE

// Visited array for BFS: visited[row * cols * 4 + col * 4 + dir]
// 4 directions per cell
visited:        .space MAX_GRID_SIZE * MAX_GRID_SIZE * 4

// BFS queue: stores (row, col, dir) as 32-bit values packed
// Each entry is 12 bytes: row(4) + col(4) + dir(4)
queue:          .space MAX_QUEUE * 12
queue_head:     .quad 0
queue_tail:     .quad 0

// Direction deltas: dr, dc for each direction (right, down, left, up)
dr:             .word 0, 1, 0, -1
dc:             .word 1, 0, -1, 0

// Mirror mappings:
// For '/' mirror: dir -> new_dir mapping [right->up, down->left, left->down, up->right]
// right(0)->up(3), down(1)->left(2), left(2)->down(1), up(3)->right(0)
slash_map:      .word 3, 2, 1, 0

// For '\' mirror: right->down, down->right, left->up, up->left
// right(0)->down(1), down(1)->right(0), left(2)->up(3), up(3)->left(2)
backslash_map:  .word 1, 0, 3, 2

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_start:
    // Open input file
    LOAD_ADDR x0, input_path
    mov     x1, #0                          // O_RDONLY
    mov     x2, #0
    mov     x16, #5                         // open() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    mov     x19, x0                         // Save fd

    // Read file
    mov     x0, x19
    LOAD_ADDR x1, file_buffer
    mov     x2, #MAX_INPUT_SIZE
    mov     x16, #3                         // read() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    mov     x20, x0                         // Save bytes read

    // Close file
    mov     x0, x19
    mov     x16, #6                         // close() syscall
    svc     #0x80

    // Parse input grid
    bl      parse_grid

    // Solve Part 1: start at (0,0) heading right (dir=0)
    mov     x0, #0                          // start_row
    mov     x1, #0                          // start_col
    mov     x2, #0                          // start_dir (right)
    bl      count_energized
    mov     x21, x0                         // Save part1 result

    // Solve Part 2: find maximum energized from any edge
    bl      solve_part2
    mov     x22, x0                         // Save part2 result

    // Print Part 1 result
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x21
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Print Part 2 result
    LOAD_ADDR x0, part2_msg
    bl      print_str
    mov     x0, x22
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Exit successfully
    mov     x0, #0
    mov     x16, #1                         // exit() syscall
    svc     #0x80

error_exit:
    LOAD_ADDR x0, error_msg
    bl      print_str
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// ============================================================================
// parse_grid: Parse the input into grid array
// Sets rows and cols globals
// ============================================================================
parse_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, file_buffer              // Input pointer
    LOAD_ADDR x20, grid                     // Output pointer
    mov     x21, #0                         // row count
    mov     x22, #0                         // col count (width)

    mov     x23, #0                         // current col in row

parse_loop:
    ldrb    w0, [x19], #1
    cbz     w0, parse_done

    cmp     w0, #'\n'
    b.eq    end_of_line

    // Store character in grid
    strb    w0, [x20], #1
    add     x23, x23, #1
    b       parse_loop

end_of_line:
    // First line sets column count
    cbz     x22, set_cols
    b       next_row
set_cols:
    mov     x22, x23

next_row:
    add     x21, x21, #1
    mov     x23, #0
    b       parse_loop

parse_done:
    // Handle last row if no trailing newline
    cbnz    x23, count_last_row
    b       save_dims

count_last_row:
    add     x21, x21, #1

save_dims:
    // Save dimensions
    LOAD_ADDR x0, rows
    str     x21, [x0]
    LOAD_ADDR x0, cols
    str     x22, [x0]

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// count_energized: Count energized tiles starting from given position
// Input: x0 = start_row, x1 = start_col, x2 = start_dir
// Returns: x0 = count of energized tiles
// ============================================================================
count_energized:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0                         // start_row
    mov     x20, x1                         // start_col
    mov     x21, x2                         // start_dir

    // Load dimensions
    LOAD_ADDR x0, rows
    ldr     x22, [x0]                       // rows
    LOAD_ADDR x0, cols
    ldr     x23, [x0]                       // cols

    // Clear visited array
    LOAD_ADDR x0, visited
    mul     x1, x22, x23
    lsl     x1, x1, #2                      // * 4 directions
clear_visited:
    cbz     x1, init_bfs
    strb    wzr, [x0], #1
    sub     x1, x1, #1
    b       clear_visited

init_bfs:
    // Initialize queue with start position
    LOAD_ADDR x24, queue
    mov     x25, #0                         // queue head
    mov     x26, #1                         // queue tail

    // Enqueue start: row, col, dir
    str     w19, [x24, #0]
    str     w20, [x24, #4]
    str     w21, [x24, #8]

bfs_loop:
    // Check if queue is empty
    cmp     x25, x26
    b.ge    count_result

    // Dequeue: get entry at head
    mov     x0, #12
    mul     x1, x25, x0
    add     x1, x24, x1
    ldr     w27, [x1, #0]                   // row
    ldr     w28, [x1, #4]                   // col
    ldr     w8, [x1, #8]                    // dir
    add     x25, x25, #1

    // Check bounds
    cmp     w27, #0
    b.lt    bfs_loop
    cmp     x27, x22
    b.ge    bfs_loop
    cmp     w28, #0
    b.lt    bfs_loop
    cmp     x28, x23
    b.ge    bfs_loop

    // Calculate visited index: row * cols * 4 + col * 4 + dir
    mul     x9, x27, x23
    add     x9, x9, x28
    lsl     x9, x9, #2
    add     x9, x9, x8

    // Check if already visited this state
    LOAD_ADDR x0, visited
    ldrb    w10, [x0, x9]
    cbnz    w10, bfs_loop

    // Mark as visited
    mov     w10, #1
    strb    w10, [x0, x9]

    // Get grid cell
    mul     x11, x27, x23
    add     x11, x11, x28
    LOAD_ADDR x0, grid
    ldrb    w12, [x0, x11]                  // cell character

    // Process based on cell type
    // w8 = current direction, w12 = cell char

    // Check for '.'
    cmp     w12, #'.'
    b.eq    continue_same

    // Check for '/'
    cmp     w12, #'/'
    b.eq    slash_mirror

    // Check for '\'
    cmp     w12, #'\\'
    b.eq    backslash_mirror

    // Check for '|'
    cmp     w12, #'|'
    b.eq    vertical_splitter

    // Check for '-'
    cmp     w12, #'-'
    b.eq    horizontal_splitter

    // Default: continue same direction
    b       continue_same

slash_mirror:
    // '/' maps: right->up, down->left, left->down, up->right
    LOAD_ADDR x0, slash_map
    ldr     w8, [x0, x8, lsl #2]
    b       continue_same

backslash_mirror:
    // '\' maps: right->down, down->right, left->up, up->left
    LOAD_ADDR x0, backslash_map
    ldr     w8, [x0, x8, lsl #2]
    b       continue_same

vertical_splitter:
    // '|' - if horizontal (right=0 or left=2), split to up(3) and down(1)
    cmp     w8, #0
    b.eq    split_vertical
    cmp     w8, #2
    b.eq    split_vertical
    // Otherwise pass through (already going up or down)
    b       continue_same

split_vertical:
    // Enqueue both up (dir=3) and down (dir=1)
    // First: down
    mov     w8, #1
    bl      enqueue_next
    // Second: up
    mov     w8, #3
    b       continue_same

horizontal_splitter:
    // '-' - if vertical (down=1 or up=3), split to left(2) and right(0)
    cmp     w8, #1
    b.eq    split_horizontal
    cmp     w8, #3
    b.eq    split_horizontal
    // Otherwise pass through (already going left or right)
    b       continue_same

split_horizontal:
    // Enqueue both left (dir=2) and right (dir=0)
    // First: right
    mov     w8, #0
    bl      enqueue_next
    // Second: left
    mov     w8, #2
    b       continue_same

continue_same:
    // Move in direction w8 and enqueue
    bl      enqueue_next
    b       bfs_loop

enqueue_next:
    // Enqueue next cell in direction w8
    // Calculate next row/col
    LOAD_ADDR x0, dr
    ldrsw   x13, [x0, x8, lsl #2]
    LOAD_ADDR x0, dc
    ldrsw   x14, [x0, x8, lsl #2]

    add     w15, w27, w13                   // next_row
    add     w16, w28, w14                   // next_col

    // Add to queue
    mov     x0, #12
    mul     x1, x26, x0
    add     x1, x24, x1
    str     w15, [x1, #0]
    str     w16, [x1, #4]
    str     w8, [x1, #8]
    add     x26, x26, #1

    ret

count_result:
    // Count unique (row, col) pairs that are energized
    // For each cell, check if any direction was visited
    mov     x0, #0                          // count
    mov     x1, #0                          // cell index

    mul     x2, x22, x23                    // total cells

count_loop:
    cmp     x1, x2
    b.ge    count_done

    // Check if any of the 4 directions were visited for this cell
    lsl     x3, x1, #2                      // cell * 4
    LOAD_ADDR x4, visited

    ldrb    w5, [x4, x3]
    add     x3, x3, #1
    ldrb    w6, [x4, x3]
    orr     w5, w5, w6
    add     x3, x3, #1
    ldrb    w6, [x4, x3]
    orr     w5, w5, w6
    add     x3, x3, #1
    ldrb    w6, [x4, x3]
    orr     w5, w5, w6

    cbz     w5, not_energized
    add     x0, x0, #1

not_energized:
    add     x1, x1, #1
    b       count_loop

count_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part2: Find maximum energized tiles from any edge
// Returns: x0 = maximum count
// ============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    // Load dimensions
    LOAD_ADDR x0, rows
    ldr     x19, [x0]                       // rows
    LOAD_ADDR x0, cols
    ldr     x20, [x0]                       // cols

    mov     x21, #0                         // max_energized

    // Top row, heading down (dir=1)
    mov     x22, #0                         // col
top_loop:
    cmp     x22, x20
    b.ge    bottom_edge

    mov     x0, #0                          // row = 0
    mov     x1, x22                         // col
    mov     x2, #1                          // dir = down
    bl      count_energized
    cmp     x0, x21
    csel    x21, x0, x21, gt

    add     x22, x22, #1
    b       top_loop

bottom_edge:
    // Bottom row, heading up (dir=3)
    mov     x22, #0                         // col
    sub     x23, x19, #1                    // last row
bottom_loop:
    cmp     x22, x20
    b.ge    left_edge

    mov     x0, x23                         // row = rows-1
    mov     x1, x22                         // col
    mov     x2, #3                          // dir = up
    bl      count_energized
    cmp     x0, x21
    csel    x21, x0, x21, gt

    add     x22, x22, #1
    b       bottom_loop

left_edge:
    // Left column, heading right (dir=0)
    mov     x22, #0                         // row
left_loop:
    cmp     x22, x19
    b.ge    right_edge

    mov     x0, x22                         // row
    mov     x1, #0                          // col = 0
    mov     x2, #0                          // dir = right
    bl      count_energized
    cmp     x0, x21
    csel    x21, x0, x21, gt

    add     x22, x22, #1
    b       left_loop

right_edge:
    // Right column, heading left (dir=2)
    mov     x22, #0                         // row
    sub     x24, x20, #1                    // last col
right_loop:
    cmp     x22, x19
    b.ge    part2_done

    mov     x0, x22                         // row
    mov     x1, x24                         // col = cols-1
    mov     x2, #2                          // dir = left
    bl      count_energized
    cmp     x0, x21
    csel    x21, x0, x21, gt

    add     x22, x22, #1
    b       right_loop

part2_done:
    mov     x0, x21

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print a null-terminated string
// Input: x0 = pointer to string
// ============================================================================
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Find length
    mov     x20, #0
1:  ldrb    w1, [x19, x20]
    cbz     w1, 2f
    add     x20, x20, #1
    b       1b

2:  // Write to stdout
    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4                         // write() syscall
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print a number
// Input: x0 = number
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

    // Handle zero case
    cbnz    x19, 1f
    sub     x20, x20, #1
    mov     w0, #'0'
    strb    w0, [x20]
    b       2f

1:  cbz     x19, 2f
    mov     x1, #10
    udiv    x2, x19, x1
    msub    x3, x2, x1, x19
    add     w3, w3, #'0'
    sub     x20, x20, #1
    strb    w3, [x20]
    mov     x19, x2
    b       1b

2:  mov     x0, x20
    bl      print_str

    add     sp, sp, #32
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
