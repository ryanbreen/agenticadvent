// Day 10: Hoof It - ARM64 Assembly (macOS)
//
// Part 1: BFS from each trailhead (height 0) to count unique 9s reachable
// Part 2: DFS to count distinct paths from each trailhead to any 9

.global _start
.align 4

// Constants
.equ MAX_INPUT_SIZE, 10240
.equ MAX_GRID_SIZE, 60
.equ MAX_QUEUE, 4096

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
// File I/O buffer
file_buffer:    .space MAX_INPUT_SIZE

// Grid storage
grid:           .space MAX_GRID_SIZE * MAX_GRID_SIZE
rows:           .quad 0
cols:           .quad 0

// BFS queue (for Part 1)
queue:          .space MAX_QUEUE * 8      // Store (row, col) as 32-bit pairs in 64-bit words
queue_head:     .quad 0
queue_tail:     .quad 0

// Visited set (for Part 1) - bitmap
visited:        .space MAX_GRID_SIZE * MAX_GRID_SIZE

// Set of reachable 9s (for Part 1)
nines_set:      .space MAX_GRID_SIZE * MAX_GRID_SIZE

// Directions: up, down, left, right (dr, dc pairs)
directions:     .quad -1, 0      // up
                .quad 1, 0       // down
                .quad 0, -1      // left
                .quad 0, 1       // right

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_start:
    // Open and read input file
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

    // Parse input into grid
    bl      parse_grid

    // Solve Part 1
    bl      solve_part1
    mov     x21, x0                         // Save part1 result

    // Solve Part 2
    bl      solve_part2
    mov     x22, x0                         // Save part2 result

    // Print results
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x21
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    LOAD_ADDR x0, part2_msg
    bl      print_str
    mov     x0, x22
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Exit
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
// parse_grid: Parse input into 2D grid
// ============================================================================
parse_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, file_buffer              // Input pointer
    LOAD_ADDR x20, grid                     // Grid pointer
    mov     x21, #0                         // Row counter
    mov     x22, #0                         // Col counter

parse_loop:
    ldrb    w0, [x19], #1                   // Load next character

    // Check for end of input
    cbz     w0, parse_done

    cmp     w0, #'\n'
    b.eq    parse_next_row

    cmp     w0, #'0'
    b.lt    parse_loop
    cmp     w0, #'9'
    b.gt    parse_loop

    // Store digit (convert ASCII to number)
    sub     w0, w0, #'0'
    strb    w0, [x20], #1
    add     x22, x22, #1
    b       parse_loop

parse_next_row:
    cbz     x22, parse_loop                 // Skip empty lines
    add     x21, x21, #1
    mov     x22, #0
    b       parse_loop

parse_done:
    // Save grid dimensions
    LOAD_ADDR x0, rows
    str     x21, [x0]

    // Calculate cols from first row
    LOAD_ADDR x19, file_buffer
    mov     x1, #0
1:  ldrb    w2, [x19], #1
    cmp     w2, #'\n'
    b.eq    2f
    cbz     w2, 2f
    add     x1, x1, #1
    b       1b
2:  LOAD_ADDR x0, cols
    str     x1, [x0]

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// get_cell: Get grid value at (row, col)
// Input: x0 = row, x1 = col
// Output: w0 = value (or -1 if out of bounds)
// ============================================================================
get_cell:
    LOAD_ADDR x2, rows
    ldr     x2, [x2]
    cmp     x0, #0
    b.lt    out_of_bounds
    cmp     x0, x2
    b.ge    out_of_bounds

    LOAD_ADDR x2, cols
    ldr     x2, [x2]
    cmp     x1, #0
    b.lt    out_of_bounds
    cmp     x1, x2
    b.ge    out_of_bounds

    // Calculate offset: row * cols + col
    mul     x3, x0, x2
    add     x3, x3, x1
    LOAD_ADDR x4, grid
    ldrb    w0, [x4, x3]
    ret

out_of_bounds:
    mov     w0, #-1
    ret

// ============================================================================
// bfs_count_nines: BFS from (start_r, start_c) to count unique 9s
// Input: x0 = start_r, x1 = start_c
// Output: x0 = count of unique 9s reachable
// ============================================================================
bfs_count_nines:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0                         // start_r
    mov     x20, x1                         // start_c

    // Clear visited array
    LOAD_ADDR x0, visited
    mov     x1, #0
    mov     x2, #MAX_GRID_SIZE * MAX_GRID_SIZE
1:  strb    wzr, [x0, x1]
    add     x1, x1, #1
    cmp     x1, x2
    b.lt    1b

    // Clear nines_set
    LOAD_ADDR x0, nines_set
    mov     x1, #0
    mov     x2, #MAX_GRID_SIZE * MAX_GRID_SIZE
1:  strb    wzr, [x0, x1]
    add     x1, x1, #1
    cmp     x1, x2
    b.lt    1b

    // Initialize queue with starting position
    LOAD_ADDR x21, queue
    LOAD_ADDR x0, cols
    ldr     x0, [x0]
    mul     x1, x19, x0
    add     x1, x1, x20                     // offset = start_r * cols + start_c
    str     x1, [x21]                       // queue[0] = offset
    LOAD_ADDR x0, queue_head
    str     xzr, [x0]
    LOAD_ADDR x0, queue_tail
    mov     x1, #1
    str     x1, [x0]

    // Mark starting position as visited
    LOAD_ADDR x0, visited
    LOAD_ADDR x2, cols
    ldr     x2, [x2]
    mul     x3, x19, x2
    add     x3, x3, x20
    mov     w1, #1
    strb    w1, [x0, x3]

bfs_loop:
    // Check if queue is empty
    LOAD_ADDR x0, queue_head
    ldr     x22, [x0]                       // head
    LOAD_ADDR x0, queue_tail
    ldr     x23, [x0]                       // tail
    cmp     x22, x23
    b.ge    bfs_done

    // Dequeue
    ldr     x0, [x21, x22, lsl #3]          // offset = queue[head]
    add     x22, x22, #1
    LOAD_ADDR x1, queue_head
    str     x22, [x1]

    // Convert offset to row, col
    LOAD_ADDR x1, cols
    ldr     x1, [x1]
    udiv    x24, x0, x1                     // row = offset / cols
    msub    x25, x24, x1, x0                // col = offset % cols

    // Get current height
    mov     x0, x24
    mov     x1, x25
    bl      get_cell
    mov     w26, w0                         // current_height

    // If height is 9, add to nines_set
    cmp     w26, #9
    b.ne    try_neighbors

    LOAD_ADDR x0, nines_set
    LOAD_ADDR x1, cols
    ldr     x1, [x1]
    mul     x2, x24, x1
    add     x2, x2, x25
    mov     w1, #1
    strb    w1, [x0, x2]
    b       bfs_loop

try_neighbors:
    // Try all 4 directions
    LOAD_ADDR x27, directions
    mov     x28, #0                         // direction index

dir_loop:
    cmp     x28, #4
    b.ge    bfs_loop

    // Get direction deltas (each direction is 16 bytes: dr at +0, dc at +8)
    lsl     x0, x28, #4                     // offset for direction
    ldr     x0, [x27, x0]                   // dr
    lsl     x1, x28, #4                     // offset for direction
    add     x1, x1, #8                      // offset for dc
    ldr     x1, [x27, x1]                   // dc

    // Calculate new position
    add     x2, x24, x0                     // nr = row + dr
    add     x3, x25, x1                     // nc = col + dc

    // Check if neighbor is valid and not visited
    mov     x0, x2
    mov     x1, x3
    stp     x2, x3, [sp, #-16]!
    bl      get_cell
    ldp     x2, x3, [sp], #16

    cmp     w0, #0
    b.lt    next_dir                        // Out of bounds

    // Check if height is current_height + 1
    add     w1, w26, #1
    cmp     w0, w1
    b.ne    next_dir

    // Check if already visited
    LOAD_ADDR x4, visited
    LOAD_ADDR x5, cols
    ldr     x5, [x5]
    mul     x6, x2, x5
    add     x6, x6, x3
    ldrb    w7, [x4, x6]
    cbnz    w7, next_dir

    // Mark as visited
    mov     w7, #1
    strb    w7, [x4, x6]

    // Enqueue
    LOAD_ADDR x4, queue
    LOAD_ADDR x5, queue_tail
    ldr     x7, [x5]
    str     x6, [x4, x7, lsl #3]
    add     x7, x7, #1
    str     x7, [x5]

next_dir:
    add     x28, x28, #1
    b       dir_loop

bfs_done:
    // Count nines in nines_set
    LOAD_ADDR x0, nines_set
    LOAD_ADDR x1, rows
    ldr     x1, [x1]
    LOAD_ADDR x2, cols
    ldr     x2, [x2]
    mul     x3, x1, x2                      // total cells
    mov     x4, #0                          // count
    mov     x5, #0                          // index

count_loop:
    cmp     x5, x3
    b.ge    count_done
    ldrb    w6, [x0, x5]
    add     x4, x4, x6
    add     x5, x5, #1
    b       count_loop

count_done:
    mov     x0, x4                          // Return count

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// dfs_count_paths: DFS to count all distinct paths from (r, c) to any 9
// Input: x0 = r, x1 = c
// Output: x0 = count of distinct paths
// ============================================================================
dfs_count_paths:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    mov     x19, x0                         // r
    mov     x20, x1                         // c

    // Get current height
    mov     x0, x19
    mov     x1, x20
    bl      get_cell
    mov     w21, w0                         // current_height

    // Base case: if height is 9, return 1
    cmp     w21, #9
    b.ne    dfs_recurse
    mov     x0, #1
    b       dfs_return

dfs_recurse:
    // Try all 4 directions and sum paths
    mov     x22, #0                         // total paths
    LOAD_ADDR x23, directions
    mov     x24, #0                         // direction index

dfs_dir_loop:
    cmp     x24, #4
    b.ge    dfs_sum_done

    // Get direction deltas (each direction is 16 bytes: dr at +0, dc at +8)
    lsl     x0, x24, #4                     // offset for direction
    ldr     x0, [x23, x0]                   // dr
    lsl     x1, x24, #4                     // offset for direction
    add     x1, x1, #8                      // offset for dc
    ldr     x1, [x23, x1]                   // dc

    // Calculate new position
    add     x2, x19, x0                     // nr = r + dr
    add     x3, x20, x1                     // nc = c + dc

    // Check if neighbor is valid
    mov     x0, x2
    mov     x1, x3
    stp     x2, x3, [sp, #-16]!
    bl      get_cell
    ldp     x2, x3, [sp], #16

    cmp     w0, #0
    b.lt    dfs_next_dir                    // Out of bounds

    // Check if height is current_height + 1
    add     w1, w21, #1
    cmp     w0, w1
    b.ne    dfs_next_dir

    // Recurse
    mov     x0, x2
    mov     x1, x3
    bl      dfs_count_paths
    add     x22, x22, x0                    // total += paths

dfs_next_dir:
    add     x24, x24, #1
    b       dfs_dir_loop

dfs_sum_done:
    mov     x0, x22                         // Return total

dfs_return:
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part1: Find all trailheads and sum their scores
// ============================================================================
solve_part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x19, rows
    ldr     x19, [x19]
    LOAD_ADDR x20, cols
    ldr     x20, [x20]
    mov     x21, #0                         // total_score
    mov     x22, #0                         // row

row_loop:
    cmp     x22, x19
    b.ge    part1_done
    mov     x23, #0                         // col

col_loop:
    cmp     x23, x20
    b.ge    next_row

    // Check if this is a trailhead (height 0)
    mov     x0, x22
    mov     x1, x23
    bl      get_cell
    cmp     w0, #0
    b.ne    next_col

    // Count reachable nines from this trailhead
    mov     x0, x22
    mov     x1, x23
    bl      bfs_count_nines
    add     x21, x21, x0                    // total_score += score

next_col:
    add     x23, x23, #1
    b       col_loop

next_row:
    add     x22, x22, #1
    b       row_loop

part1_done:
    mov     x0, x21                         // Return total_score

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part2: Find all trailheads and sum their ratings
// ============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x19, rows
    ldr     x19, [x19]
    LOAD_ADDR x20, cols
    ldr     x20, [x20]
    mov     x21, #0                         // total_rating
    mov     x22, #0                         // row

p2_row_loop:
    cmp     x22, x19
    b.ge    part2_done
    mov     x23, #0                         // col

p2_col_loop:
    cmp     x23, x20
    b.ge    p2_next_row

    // Check if this is a trailhead (height 0)
    mov     x0, x22
    mov     x1, x23
    bl      get_cell
    cmp     w0, #0
    b.ne    p2_next_col

    // Count distinct paths from this trailhead
    mov     x0, x22
    mov     x1, x23
    bl      dfs_count_paths
    add     x21, x21, x0                    // total_rating += rating

p2_next_col:
    add     x23, x23, #1
    b       p2_col_loop

p2_next_row:
    add     x22, x22, #1
    b       p2_row_loop

part2_done:
    mov     x0, x21                         // Return total_rating

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print a null-terminated string
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
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print a number
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
