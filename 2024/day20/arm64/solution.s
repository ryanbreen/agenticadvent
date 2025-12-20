// Day 20: Race Condition - ARM64 Assembly (macOS)
//
// Algorithm:
// 1. Parse grid, find S (start) and E (end) positions
// 2. BFS from start to trace the single path, recording distance for each track cell
// 3. For each pair of track positions, check if a "cheat" saves >= 100 picoseconds
//    - Part 1: max cheat time = 2
//    - Part 2: max cheat time = 20
// 4. Savings = dist[end] - dist[start] - manhattan_distance

.global _start
.align 4

// Constants
.equ MAX_INPUT_SIZE, 32768
.equ GRID_SIZE, 141
.equ GRID_CELLS, 141 * 141       // 19881
.equ MAX_QUEUE, 32768
.equ MIN_SAVINGS, 100

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

// Grid storage (characters as bytes)
grid:           .space GRID_CELLS

// Grid dimensions
grid_rows:      .quad 0
grid_cols:      .quad 0

// Start and End positions
start_row:      .quad 0
start_col:      .quad 0
end_row:        .quad 0
end_col:        .quad 0

// Distance array (-1 means not visited/wall)
// Using 4 bytes per cell for distance
distances:      .space GRID_CELLS * 4

// BFS queue (stores row,col pairs as 32-bit each)
queue:          .space MAX_QUEUE * 8
queue_head:     .quad 0
queue_tail:     .quad 0

// Track positions list (row, col pairs for all track cells)
track_positions:    .space GRID_CELLS * 8
num_track_positions: .quad 0

// Directions: up, down, left, right (dr, dc pairs)
directions:     .word -1, 0     // up
                .word 1, 0      // down
                .word 0, -1     // left
                .word 0, 1      // right

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

    // Parse grid
    mov     x0, x20                         // bytes read
    bl      parse_grid

    // Build distance map via BFS
    bl      trace_path

    // Build list of track positions
    bl      build_track_list

    // Solve Part 1 (max cheat = 2)
    mov     x0, #2
    bl      count_cheats
    mov     x21, x0                         // Save part1 result

    // Solve Part 2 (max cheat = 20)
    mov     x0, #20
    bl      count_cheats
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
// parse_grid: Parse the input into grid array, find S and E
// Input: x0 = bytes read
// ============================================================================
parse_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    mov     x19, x0                         // bytes_read
    LOAD_ADDR x20, file_buffer              // src
    LOAD_ADDR x21, grid                     // dst
    mov     x22, #0                         // row
    mov     x23, #0                         // col
    mov     x24, #0                         // index in file

parse_loop:
    cmp     x24, x19
    b.ge    parse_done

    ldrb    w0, [x20, x24]

    // Check for newline
    cmp     w0, #'\n'
    b.ne    not_newline

    // End of row - save cols if first row
    cbz     x22, save_cols
    b       next_row

save_cols:
    LOAD_ADDR x1, grid_cols
    str     x23, [x1]

next_row:
    add     x22, x22, #1                    // row++
    mov     x23, #0                         // col = 0
    add     x24, x24, #1
    b       parse_loop

not_newline:
    // Calculate grid index
    mov     x1, #GRID_SIZE
    mul     x2, x22, x1
    add     x2, x2, x23                     // index = row * GRID_SIZE + col

    // Store character
    strb    w0, [x21, x2]

    // Check for S
    cmp     w0, #'S'
    b.ne    check_end
    LOAD_ADDR x1, start_row
    str     x22, [x1]
    LOAD_ADDR x1, start_col
    str     x23, [x1]
    b       advance_col

check_end:
    cmp     w0, #'E'
    b.ne    advance_col
    LOAD_ADDR x1, end_row
    str     x22, [x1]
    LOAD_ADDR x1, end_col
    str     x23, [x1]

advance_col:
    add     x23, x23, #1
    add     x24, x24, #1
    b       parse_loop

parse_done:
    // Save final row count
    LOAD_ADDR x1, grid_rows
    // If last char wasn't newline, add 1
    sub     x0, x24, #1
    ldrb    w0, [x20, x0]
    cmp     w0, #'\n'
    b.eq    store_rows
    add     x22, x22, #1
store_rows:
    str     x22, [x1]

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// trace_path: BFS from start, recording distance for each track cell
// ============================================================================
trace_path:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    // Initialize all distances to -1
    LOAD_ADDR x0, distances
    mov     x1, #GRID_CELLS
    mov     w2, #-1
init_dist:
    cbz     x1, init_done
    str     w2, [x0], #4
    sub     x1, x1, #1
    b       init_dist

init_done:
    // Get start position
    LOAD_ADDR x0, start_row
    ldr     x19, [x0]                       // start_row
    LOAD_ADDR x0, start_col
    ldr     x20, [x0]                       // start_col

    // Calculate start index
    mov     x1, #GRID_SIZE
    mul     x21, x19, x1
    add     x21, x21, x20                   // start_index

    // Set distance[start] = 0
    LOAD_ADDR x0, distances
    str     wzr, [x0, x21, lsl #2]

    // Initialize queue
    LOAD_ADDR x22, queue
    stp     w19, w20, [x22]                 // queue[0] = (row, col)
    LOAD_ADDR x0, queue_head
    str     xzr, [x0]
    LOAD_ADDR x0, queue_tail
    mov     x1, #1
    str     x1, [x0]

bfs_loop:
    // Check if queue empty
    LOAD_ADDR x0, queue_head
    ldr     x23, [x0]                       // head
    LOAD_ADDR x0, queue_tail
    ldr     x24, [x0]                       // tail
    cmp     x23, x24
    b.ge    bfs_done

    // Dequeue (row, col)
    lsl     x0, x23, #3                     // head * 8
    add     x1, x22, x0                     // queue + offset
    ldp     w25, w26, [x1]                  // row, col
    add     x23, x23, #1
    LOAD_ADDR x0, queue_head
    str     x23, [x0]

    // Calculate current index
    mov     w0, #GRID_SIZE
    mul     w27, w25, w0
    add     w27, w27, w26                   // cur_index

    // Get current distance
    LOAD_ADDR x0, distances
    ldr     w28, [x0, x27, lsl #2]          // cur_dist

    // Try all 4 directions
    LOAD_ADDR x1, directions
    mov     w2, #0                          // dir index

dir_loop:
    cmp     w2, #4
    b.ge    bfs_loop

    // Get dr, dc
    lsl     w3, w2, #3                      // dir * 8
    ldrsw   x4, [x1, x3]                    // dr
    add     w3, w3, #4
    ldrsw   x5, [x1, x3]                    // dc

    // Calculate new row, col
    add     w6, w25, w4                     // nr = row + dr
    add     w7, w26, w5                     // nc = col + dc

    // Check bounds
    cmp     w6, #0
    b.lt    next_dir
    cmp     w6, #GRID_SIZE
    b.ge    next_dir
    cmp     w7, #0
    b.lt    next_dir
    cmp     w7, #GRID_SIZE
    b.ge    next_dir

    // Calculate new index
    mov     w3, #GRID_SIZE
    mul     w8, w6, w3
    add     w8, w8, w7                      // new_index

    // Check if wall
    LOAD_ADDR x0, grid
    ldrb    w9, [x0, x8]
    cmp     w9, #'#'
    b.eq    next_dir

    // Check if already visited (distance != -1)
    LOAD_ADDR x0, distances
    ldr     w9, [x0, x8, lsl #2]
    cmn     w9, #1                          // compare with -1
    b.ne    next_dir

    // Set distance
    add     w9, w28, #1                     // new_dist = cur_dist + 1
    str     w9, [x0, x8, lsl #2]

    // Enqueue
    LOAD_ADDR x0, queue_tail
    ldr     x10, [x0]
    lsl     x11, x10, #3                    // tail * 8
    add     x12, x22, x11                   // queue + offset
    stp     w6, w7, [x12]                   // queue[tail] = (nr, nc)
    add     x10, x10, #1
    str     x10, [x0]

next_dir:
    add     w2, w2, #1
    b       dir_loop

bfs_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// build_track_list: Build list of all track positions (distance != -1)
// ============================================================================
build_track_list:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, distances
    LOAD_ADDR x20, track_positions
    mov     x21, #0                         // count
    mov     x22, #0                         // index

build_loop:
    mov     x0, #GRID_CELLS
    cmp     x22, x0
    b.ge    build_done

    // Check if distance != -1
    ldr     w0, [x19, x22, lsl #2]
    cmn     w0, #1
    b.eq    next_cell

    // Calculate row, col from index
    mov     x1, #GRID_SIZE
    udiv    x2, x22, x1                     // row = index / GRID_SIZE
    msub    x3, x2, x1, x22                 // col = index % GRID_SIZE

    // Store (row, col)
    lsl     x4, x21, #3                     // count * 8
    add     x5, x20, x4                     // track_positions + offset
    stp     w2, w3, [x5]
    add     x21, x21, #1

next_cell:
    add     x22, x22, #1
    b       build_loop

build_done:
    LOAD_ADDR x0, num_track_positions
    str     x21, [x0]

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// count_cheats: Count cheats saving >= 100 picoseconds
// Input: x0 = max_cheat_time
// Output: x0 = count
// ============================================================================
count_cheats:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0                         // max_cheat_time
    mov     x20, #0                         // count

    LOAD_ADDR x0, num_track_positions
    ldr     x21, [x0]                       // num_positions

    LOAD_ADDR x22, track_positions
    LOAD_ADDR x23, distances

    mov     x24, #0                         // i

outer_loop:
    cmp     x24, x21
    b.ge    count_done

    // Get position i: (r1, c1)
    lsl     x0, x24, #3
    add     x1, x22, x0                     // track_positions + offset
    ldp     w25, w26, [x1]                  // r1, c1

    // Calculate index1 and get dist1
    mov     w0, #GRID_SIZE
    mul     w1, w25, w0
    add     w1, w1, w26                     // index1
    ldr     w27, [x23, x1, lsl #2]          // dist1

    mov     x28, #0                         // j

inner_loop:
    cmp     x28, x21
    b.ge    next_outer

    // Get position j: (r2, c2)
    lsl     x0, x28, #3
    add     x1, x22, x0                     // track_positions + offset
    ldp     w2, w3, [x1]                    // r2, c2

    // Calculate Manhattan distance = |r2-r1| + |c2-c1|
    sub     w4, w2, w25                     // r2 - r1
    cmp     w4, #0
    b.ge    abs_dr_done
    neg     w4, w4
abs_dr_done:

    sub     w5, w3, w26                     // c2 - c1
    cmp     w5, #0
    b.ge    abs_dc_done
    neg     w5, w5
abs_dc_done:

    add     w6, w4, w5                      // cheat_cost = |dr| + |dc|

    // Check if cheat_cost <= max_cheat_time
    cmp     w6, w19
    b.gt    next_inner

    // Calculate index2 and get dist2
    mov     w0, #GRID_SIZE
    mul     w7, w2, w0
    add     w7, w7, w3                      // index2
    ldr     w8, [x23, x7, lsl #2]           // dist2

    // Calculate savings = dist2 - dist1 - cheat_cost
    sub     w9, w8, w27                     // dist2 - dist1
    sub     w9, w9, w6                      // - cheat_cost

    // Check if savings >= MIN_SAVINGS
    cmp     w9, #MIN_SAVINGS
    b.lt    next_inner

    add     x20, x20, #1                    // count++

next_inner:
    add     x28, x28, #1
    b       inner_loop

next_outer:
    add     x24, x24, #1
    b       outer_loop

count_done:
    mov     x0, x20                         // return count

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
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
