// ARM64 Assembly solution for AoC 2025 Day 6 - Simplified version
// macOS syscalls

.global _start
.align 2

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"
debug_msg: .asciz "Debug: "

.align 3
file_buffer: .skip 65536
grid: .skip 10 * 5000  // 10 rows x 5000 cols
num_rows: .skip 8
num_cols: .skip 8
output_buffer: .skip 32

.text
_start:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Read file and parse into grid
    bl read_and_parse

    // Solve Part 1
    mov x0, #1
    bl solve
    mov x19, x0

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str
    mov x0, x19
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Solve Part 2
    mov x0, #2
    bl solve
    mov x20, x0

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str
    mov x0, x20
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Exit
    mov x0, #0
    ldp x29, x30, [sp], #16
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    svc #0x80

// Read file and parse into grid
read_and_parse:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    // Open file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0005
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0
    mov x2, #0
    svc #0x80
    mov x19, x0  // fd

    // Read file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0003
    mov x0, x19
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #65536
    svc #0x80
    mov x20, x0  // bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Parse into grid
    adrp x19, file_buffer@PAGE
    add x19, x19, file_buffer@PAGEOFF
    adrp x21, grid@PAGE
    add x21, x21, grid@PAGEOFF

    mov x22, #0  // Current row
    mov x23, #0  // Current col
    mov x24, #0  // Max cols seen
    mov x25, x19  // Current position
    add x26, x19, x20  // End position

parse_loop:
    cmp x25, x26
    b.ge parse_done

    ldrb w0, [x25]
    cmp w0, #'\n'
    b.eq parse_newline

    // Store character in grid[row][col]
    mov x1, #5000
    mul x2, x22, x1
    add x2, x2, x23
    strb w0, [x21, x2]

    add x23, x23, #1
    add x25, x25, #1
    b parse_loop

parse_newline:
    // Update max cols
    cmp x23, x24
    csel x24, x23, x24, gt

    // Next row
    add x22, x22, #1
    mov x23, #0
    add x25, x25, #1
    b parse_loop

parse_done:
    // Store dimensions
    adrp x0, num_rows@PAGE
    add x0, x0, num_rows@PAGEOFF
    str x22, [x0]
    adrp x0, num_cols@PAGE
    add x0, x0, num_cols@PAGEOFF
    str x24, [x0]

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Get grid char at (row, col), returns ' ' if out of bounds
// x0 = row, x1 = col, returns char in x0
get_grid_char:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    adrp x2, num_rows@PAGE
    add x2, x2, num_rows@PAGEOFF
    ldr x2, [x2]
    cmp x0, x2
    b.ge ggc_space

    adrp x2, num_cols@PAGE
    add x2, x2, num_cols@PAGEOFF
    ldr x2, [x2]
    cmp x1, x2
    b.ge ggc_space

    adrp x2, grid@PAGE
    add x2, x2, grid@PAGEOFF
    mov x3, #5000
    mul x4, x0, x3
    add x4, x4, x1
    ldrb w0, [x2, x4]

    cbz w0, ggc_space
    b ggc_done

ggc_space:
    mov x0, #' '

ggc_done:
    ldp x29, x30, [sp], #16
    ret

// Check if column is separator (all spaces in ALL rows)
// x0 = col, returns 1 if separator, 0 otherwise
is_sep_col:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov x19, x0  // col
    adrp x20, num_rows@PAGE
    add x20, x20, num_rows@PAGEOFF
    ldr x20, [x20]  // num_rows

    mov x2, #0  // row counter

isc_loop:
    cmp x2, x20
    b.ge isc_yes

    mov x0, x2
    mov x1, x19
    stp x2, xzr, [sp, #-16]!
    bl get_grid_char
    ldp x2, xzr, [sp], #16

    cmp x0, #' '
    b.ne isc_no

    add x2, x2, #1
    b isc_loop

isc_yes:
    mov x0, #1
    b isc_done

isc_no:
    mov x0, #0

isc_done:
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Solve part (1 or 2)
// x0 = part number
solve:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0  // part

    adrp x20, num_cols@PAGE
    add x20, x20, num_cols@PAGEOFF
    ldr x20, [x20]  // max_cols

    mov x21, #0  // grand total
    mov x22, #0  // current col

solve_main_loop:
    cmp x22, x20
    b.ge solve_done

    // Skip separator columns
    mov x0, x22
    bl is_sep_col
    cmp x0, #1
    b.ne solve_find_problem

    add x22, x22, #1
    b solve_main_loop

solve_find_problem:
    // Found start of problem
    mov x23, x22  // start_col

solve_find_end:
    cmp x22, x20
    b.ge solve_found_problem

    mov x0, x22
    bl is_sep_col
    cmp x0, #1
    b.eq solve_found_problem

    add x22, x22, #1
    b solve_find_end

solve_found_problem:
    mov x24, x22  // end_col

    // Get operator (last row, start col)
    adrp x0, num_rows@PAGE
    add x0, x0, num_rows@PAGEOFF
    ldr x0, [x0]
    sub x0, x0, #1
    mov x1, x23
    bl get_grid_char
    mov x25, x0  // operator

    // Calculate based on part
    cmp x19, #1
    b.eq solve_p1

    // Part 2
    mov x0, x23
    mov x1, x24
    mov x2, x25
    bl calc_part2
    b solve_add_result

solve_p1:
    mov x0, x23
    mov x1, x24
    mov x2, x25
    bl calc_part1

solve_add_result:
    add x21, x21, x0
    b solve_main_loop

solve_done:
    mov x0, x21

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Calculate Part 1: each row in column range is a number
// x0 = start_col, x1 = end_col, x2 = operator
// Returns result in x0
calc_part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0  // start_col
    mov x20, x1  // end_col
    mov x21, x2  // operator

    adrp x22, num_rows@PAGE
    add x22, x22, num_rows@PAGEOFF
    ldr x22, [x22]
    sub x22, x22, #1  // num_data_rows (exclude operator row)

    // Initialize accumulator
    cmp x21, #'+'
    b.eq cp1_init_0
    mov x23, #1
    b cp1_rows

cp1_init_0:
    mov x23, #0

cp1_rows:
    mov x24, #0  // row counter

cp1_row_loop:
    cmp x24, x22
    b.ge cp1_result

    // Extract number from this row in column range [start_col, end_col)
    mov x25, #0  // number accumulator
    mov x26, x19  // col counter

cp1_col_loop:
    cmp x26, x20
    b.ge cp1_apply_num

    mov x0, x24
    mov x1, x26
    stp x24, x26, [sp, #-16]!
    bl get_grid_char
    ldp x24, x26, [sp], #16

    // Check if digit
    cmp x0, #'0'
    b.lt cp1_next_col
    cmp x0, #'9'
    b.gt cp1_next_col

    // Accumulate digit
    sub x0, x0, #'0'
    mov x1, #10
    mul x25, x25, x1
    add x25, x25, x0

cp1_next_col:
    add x26, x26, #1
    b cp1_col_loop

cp1_apply_num:
    // Apply number to accumulator (skip if zero - no digits found)
    cbz x25, cp1_next_row

    cmp x21, #'+'
    b.eq cp1_add
    mul x23, x23, x25
    b cp1_next_row

cp1_add:
    add x23, x23, x25

cp1_next_row:
    add x24, x24, #1
    b cp1_row_loop

cp1_result:
    mov x0, x23

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Calculate Part 2: right-to-left, each column is a number
// x0 = start_col, x1 = end_col, x2 = operator
// Returns result in x0
calc_part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0  // start_col
    mov x20, x1  // end_col
    mov x21, x2  // operator

    adrp x22, num_rows@PAGE
    add x22, x22, num_rows@PAGEOFF
    ldr x22, [x22]
    sub x22, x22, #1  // num_data_rows

    // Initialize accumulator
    cmp x21, #'+'
    b.eq cp2_init_0
    mov x23, #1
    b cp2_cols

cp2_init_0:
    mov x23, #0

cp2_cols:
    sub x24, x20, #1  // Start from rightmost column

cp2_col_loop:
    cmp x24, x19
    b.lt cp2_result

    // Extract number from this column reading top-to-bottom
    mov x25, #0  // number accumulator
    mov x26, #0  // row counter

cp2_row_loop:
    cmp x26, x22
    b.ge cp2_apply_num

    mov x0, x26
    mov x1, x24
    stp x24, x26, [sp, #-16]!
    bl get_grid_char
    ldp x24, x26, [sp], #16

    // Check if digit
    cmp x0, #'0'
    b.lt cp2_next_row
    cmp x0, #'9'
    b.gt cp2_next_row

    // Accumulate digit
    sub x0, x0, #'0'
    mov x1, #10
    mul x25, x25, x1
    add x25, x25, x0

cp2_next_row:
    add x26, x26, #1
    b cp2_row_loop

cp2_apply_num:
    // Apply number to accumulator (skip if zero)
    cbz x25, cp2_next_col

    cmp x21, #'+'
    b.eq cp2_add
    mul x23, x23, x25
    b cp2_next_col

cp2_add:
    add x23, x23, x25

cp2_next_col:
    sub x24, x24, #1
    b cp2_col_loop

cp2_result:
    mov x0, x23

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print string
print_str:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    mov x20, #0
ps_loop:
    ldrb w1, [x19, x20]
    cbz w1, ps_write
    add x20, x20, #1
    b ps_loop
ps_write:
    movz x16, #0x2000, lsl #16
    movk x16, #0x0004
    mov x0, #1
    mov x1, x19
    mov x2, x20
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print number
print_number:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    adrp x19, output_buffer@PAGE
    add x19, x19, output_buffer@PAGEOFF
    add x19, x19, #31
    mov w1, #0
    strb w1, [x19]
    mov x20, x0
    mov x2, #10

pn_loop:
    udiv x3, x20, x2
    msub x4, x3, x2, x20
    add w4, w4, #'0'
    sub x19, x19, #1
    strb w4, [x19]
    mov x20, x3
    cbnz x20, pn_loop

    mov x0, x19
    bl print_str

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
