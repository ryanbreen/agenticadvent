.global _main
.align 4

.equ BUFFER_SIZE, 131072  // 128KB buffer for input

.data
filename:       .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

.bss
.align 4
buffer:         .space BUFFER_SIZE
grid_rows:      .space 8    // Number of rows in grid
grid_cols:      .space 8    // Number of columns in grid
start_col:      .space 8    // Starting column (where S is)
line_ptrs:      .space 8*256 // Pointers to each line (max 256 lines)

// For Part 1: active_beams array (boolean array indexed by column)
active_beams:   .space 256
new_beams:      .space 256

// For Part 2: timeline counts (8 bytes per column)
timelines:      .space 8*256
new_timelines:  .space 8*256

output_buffer:  .space 32    // For number to string conversion

.text

// Macro to load address
.macro LOAD_ADDR reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

_main:
    // Save frame pointer
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Open and read file
    bl      read_input_file

    // Parse the grid
    bl      parse_grid

    // Part 1
    LOAD_ADDR x0, part1_msg
    bl      print_string
    bl      solve_part1
    bl      print_number
    LOAD_ADDR x0, newline
    bl      print_string

    // Part 2
    LOAD_ADDR x0, part2_msg
    bl      print_string
    bl      solve_part2
    bl      print_number
    LOAD_ADDR x0, newline
    bl      print_string

    // Exit
    mov     x0, #0
    movz    x16, #0x1
    movk    x16, #0x200, lsl #16
    svc     #0x80

// Read input file into buffer
// Returns: x0 = number of bytes read
read_input_file:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // open(filename, O_RDONLY)
    LOAD_ADDR x0, filename
    mov     x1, #0
    mov     x2, #0
    movz    x16, #0x5
    movk    x16, #0x200, lsl #16
    svc     #0x80

    // Save file descriptor
    mov     x19, x0

    // read(fd, buffer, BUFFER_SIZE)
    mov     x0, x19
    LOAD_ADDR x1, buffer
    mov     x2, #BUFFER_SIZE
    movz    x16, #0x3
    movk    x16, #0x200, lsl #16
    svc     #0x80

    // Save bytes read
    mov     x20, x0

    // close(fd)
    mov     x0, x19
    movz    x16, #0x6
    movk    x16, #0x200, lsl #16
    svc     #0x80

    // Return bytes read
    mov     x0, x20

    ldp     x29, x30, [sp], #16
    ret

// Parse grid: find dimensions, starting position, and line pointers
parse_grid:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, buffer    // Current position in buffer
    mov     x20, #0          // Row counter
    mov     x21, #0          // Column counter (for current row)
    LOAD_ADDR x22, line_ptrs // Pointer to line_ptrs array

    // Store pointer to first line
    str     x19, [x22], #8

parse_grid_loop:
    ldrb    w0, [x19]
    cbz     w0, parse_grid_done   // Null terminator

    cmp     w0, #'\n'
    b.eq    parse_grid_newline

    // Check if this is the first row (to count columns)
    cbnz    x20, parse_grid_skip_col
    add     x21, x21, #1

    // Check if this is 'S' in first row
    cmp     w0, #'S'
    b.ne    parse_grid_skip_col
    sub     x0, x21, #1      // Column index (0-based)
    LOAD_ADDR x1, start_col
    str     x0, [x1]

parse_grid_skip_col:
    add     x19, x19, #1
    b       parse_grid_loop

parse_grid_newline:
    // Save column count from first row
    cbnz    x20, parse_grid_newline_skip_cols
    LOAD_ADDR x0, grid_cols
    str     x21, [x0]

parse_grid_newline_skip_cols:
    // Move past newline
    add     x19, x19, #1

    // Store pointer to next line if not at end
    ldrb    w0, [x19]
    cbz     w0, parse_grid_done
    str     x19, [x22], #8

    // Increment row counter
    add     x20, x20, #1

    // Reset column counter for next line
    mov     x21, #0

    b       parse_grid_loop

parse_grid_done:
    // Save grid dimensions - rows
    add     x20, x20, #1     // Total rows is row_counter + 1
    LOAD_ADDR x0, grid_rows
    str     x20, [x0]

    // grid_cols should already be set from first newline

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Solve Part 1
// Returns: x0 = split count
solve_part1:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    // Initialize active_beams with start_col
    LOAD_ADDR x0, active_beams
    mov     x1, #256
    bl      memzero

    LOAD_ADDR x0, start_col
    ldr     x1, [x0]         // x1 = start_col
    LOAD_ADDR x0, active_beams
    mov     w2, #1
    strb    w2, [x0, x1]

    mov     x19, #0          // split_count = 0
    mov     x20, #1          // row = 1 (start from row 1)

    LOAD_ADDR x0, grid_rows
    ldr     x21, [x0]        // x21 = rows
    LOAD_ADDR x0, grid_cols
    ldr     x22, [x0]        // x22 = cols

part1_row_loop:
    cmp     x20, x21
    b.ge    part1_done

    // Clear new_beams
    LOAD_ADDR x0, new_beams
    mov     x1, #256
    bl      memzero

    // Process each active beam column
    mov     x23, #0          // col = 0

part1_col_loop:
    cmp     x23, x22
    b.ge    part1_row_done

    // Check if beam is active at this column
    LOAD_ADDR x0, active_beams
    ldrb    w1, [x0, x23]
    cbz     w1, part1_next_col

    // Get character at grid[row][col]
    LOAD_ADDR x0, line_ptrs
    lsl     x1, x20, #3
    ldr     x2, [x0, x1]     // x2 = pointer to current row
    ldrb    w3, [x2, x23]    // w3 = character at [row][col]

    cmp     w3, #'^'
    b.eq    part1_splitter

    // If '.' or anything else, beam continues straight
    LOAD_ADDR x0, new_beams
    mov     w1, #1
    strb    w1, [x0, x23]
    b       part1_next_col

part1_splitter:
    // Increment split count
    add     x19, x19, #1

    // Left beam (col-1)
    sub     x24, x23, #1
    cmp     x24, #0
    b.lt    part1_right_beam
    LOAD_ADDR x0, new_beams
    mov     w1, #1
    strb    w1, [x0, x24]

part1_right_beam:
    // Right beam (col+1)
    add     x24, x23, #1
    cmp     x24, x22
    b.ge    part1_next_col
    LOAD_ADDR x0, new_beams
    mov     w1, #1
    strb    w1, [x0, x24]

part1_next_col:
    add     x23, x23, #1
    b       part1_col_loop

part1_row_done:
    // Copy new_beams to active_beams
    LOAD_ADDR x0, active_beams
    LOAD_ADDR x1, new_beams
    mov     x2, #256
    bl      memcpy

    // Check if any beams are active
    LOAD_ADDR x0, active_beams
    mov     x1, #256
    bl      has_any_set
    cbz     x0, part1_done

    add     x20, x20, #1
    b       part1_row_loop

part1_done:
    mov     x0, x19          // Return split_count

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Solve Part 2
// Returns: x0 = total timeline count
solve_part2:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    // Initialize timelines with start_col = 1
    LOAD_ADDR x0, timelines
    mov     x1, #256*8
    bl      memzero

    LOAD_ADDR x0, start_col
    ldr     x1, [x0]         // x1 = start_col
    LOAD_ADDR x0, timelines
    lsl     x2, x1, #3       // offset = start_col * 8
    mov     x3, #1
    str     x3, [x0, x2]

    mov     x20, #1          // row = 1 (start from row 1)

    LOAD_ADDR x0, grid_rows
    ldr     x21, [x0]        // x21 = rows
    LOAD_ADDR x0, grid_cols
    ldr     x22, [x0]        // x22 = cols

part2_row_loop:
    cmp     x20, x21
    b.ge    part2_done

    // Clear new_timelines
    LOAD_ADDR x0, new_timelines
    mov     x1, #256*8
    bl      memzero

    // Process each column
    mov     x23, #0          // col = 0

part2_col_loop:
    cmp     x23, x22
    b.ge    part2_row_done

    // Get timeline count at this column
    LOAD_ADDR x0, timelines
    lsl     x1, x23, #3
    ldr     x19, [x0, x1]    // x19 = count
    cbz     x19, part2_next_col

    // Get character at grid[row][col]
    LOAD_ADDR x0, line_ptrs
    lsl     x1, x20, #3
    ldr     x2, [x0, x1]     // x2 = pointer to current row
    ldrb    w3, [x2, x23]    // w3 = character at [row][col]

    cmp     w3, #'^'
    b.eq    part2_splitter

    // If '.' or anything else, timelines continue straight
    LOAD_ADDR x0, new_timelines
    lsl     x1, x23, #3
    ldr     x2, [x0, x1]
    add     x2, x2, x19
    str     x2, [x0, x1]
    b       part2_next_col

part2_splitter:
    // Left timeline (col-1)
    sub     x24, x23, #1
    cmp     x24, #0
    b.lt    part2_right_timeline
    LOAD_ADDR x0, new_timelines
    lsl     x1, x24, #3
    ldr     x2, [x0, x1]
    add     x2, x2, x19
    str     x2, [x0, x1]

part2_right_timeline:
    // Right timeline (col+1)
    add     x24, x23, #1
    cmp     x24, x22
    b.ge    part2_next_col
    LOAD_ADDR x0, new_timelines
    lsl     x1, x24, #3
    ldr     x2, [x0, x1]
    add     x2, x2, x19
    str     x2, [x0, x1]

part2_next_col:
    add     x23, x23, #1
    b       part2_col_loop

part2_row_done:
    // Copy new_timelines to timelines
    LOAD_ADDR x0, timelines
    LOAD_ADDR x1, new_timelines
    mov     x2, #256*8
    bl      memcpy

    // Check if any timelines exist
    LOAD_ADDR x0, timelines
    mov     x1, #256
    bl      sum_timelines
    cbz     x0, part2_done

    add     x20, x20, #1
    b       part2_row_loop

part2_done:
    // Sum all timelines
    LOAD_ADDR x0, timelines
    mov     x1, #256
    bl      sum_timelines

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Sum timeline counts
// x0 = pointer to timeline array
// x1 = number of columns
// Returns: x0 = sum
sum_timelines:
    mov     x2, #0           // sum = 0
    mov     x3, #0           // i = 0
sum_loop:
    cmp     x3, x1
    b.ge    sum_done
    lsl     x4, x3, #3
    ldr     x5, [x0, x4]
    add     x2, x2, x5
    add     x3, x3, #1
    b       sum_loop
sum_done:
    mov     x0, x2
    ret

// Check if any byte is set in array
// x0 = pointer to array
// x1 = length
// Returns: x0 = 1 if any set, 0 otherwise
has_any_set:
    mov     x2, #0           // i = 0
has_any_loop:
    cmp     x2, x1
    b.ge    has_any_none
    ldrb    w3, [x0, x2]
    cbnz    w3, has_any_found
    add     x2, x2, #1
    b       has_any_loop
has_any_found:
    mov     x0, #1
    ret
has_any_none:
    mov     x0, #0
    ret

// Memory copy
// x0 = dest, x1 = src, x2 = length
memcpy:
    cbz     x2, memcpy_done
    mov     x3, #0
memcpy_loop:
    cmp     x3, x2
    b.ge    memcpy_done
    ldrb    w4, [x1, x3]
    strb    w4, [x0, x3]
    add     x3, x3, #1
    b       memcpy_loop
memcpy_done:
    ret

// Memory zero
// x0 = dest, x1 = length
memzero:
    cbz     x1, memzero_done
    mov     x2, #0
memzero_loop:
    cmp     x2, x1
    b.ge    memzero_done
    strb    wzr, [x0, x2]
    add     x2, x2, #1
    b       memzero_loop
memzero_done:
    ret

// Print string (null-terminated)
// x0 = pointer to string
print_string:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Calculate length
    mov     x20, #0
print_strlen_loop:
    ldrb    w1, [x19, x20]
    cbz     w1, print_strlen_done
    add     x20, x20, #1
    b       print_strlen_loop
print_strlen_done:

    // write(1, string, length)
    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    movz    x16, #0x4
    movk    x16, #0x200, lsl #16
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Print number
// x0 = number to print
print_number:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0          // Number to convert
    LOAD_ADDR x20, output_buffer
    add     x21, x20, #31    // End of buffer
    strb    wzr, [x21]       // Null terminator

    // Handle zero specially
    cbnz    x19, print_num_loop
    sub     x21, x21, #1
    mov     w0, #'0'
    strb    w0, [x21]
    b       print_num_done

print_num_loop:
    cbz     x19, print_num_done

    // Divide by 10
    mov     x22, #10
    udiv    x0, x19, x22
    msub    x1, x0, x22, x19  // remainder = num - (num/10)*10

    // Convert digit to ASCII
    add     w1, w1, #'0'
    sub     x21, x21, #1
    strb    w1, [x21]

    mov     x19, x0
    b       print_num_loop

print_num_done:
    mov     x0, x21
    bl      print_string

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
