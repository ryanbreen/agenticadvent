// Advent of Code 2023 Day 3: Gear Ratios - ARM64 Assembly for macOS

.global _main
.align 4

.data
    filename: .asciz "../input.txt"
    msg_part1: .asciz "Part 1: "
    msg_part2: .asciz "Part 2: "
    newline: .asciz "\n"

.bss
    .align 4
    buffer: .skip 32768           // File contents
    grid: .skip 32768             // Parsed grid
    width: .skip 8
    height: .skip 8
    num_buffer: .skip 32

.text

_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    bl read_file
    cbz x0, exit_error

    bl parse_grid

    // Part 1
    adrp x0, msg_part1@PAGE
    add x0, x0, msg_part1@PAGEOFF
    bl print_str

    bl solve_part1
    bl print_num

    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2
    adrp x0, msg_part2@PAGE
    add x0, x0, msg_part2@PAGEOFF
    bl print_str

    bl solve_part2
    bl print_num

    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    mov x0, #0
    ldp x29, x30, [sp], #16
    mov x16, #1
    svc #0x80

exit_error:
    mov x0, #1
    ldp x29, x30, [sp], #16
    mov x16, #1
    svc #0x80

// Read file into buffer, returns size in x0
read_file:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    // Open file
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0              // O_RDONLY
    mov x16, #5             // SYS_OPEN
    svc #0x80
    cmp x0, #0
    b.lt read_error
    mov x19, x0             // Save fd

    // Read file
    mov x0, x19
    adrp x1, buffer@PAGE
    add x1, x1, buffer@PAGEOFF
    mov x2, #32768
    mov x16, #3             // SYS_READ
    svc #0x80
    mov x20, x0             // Save size

    // Close file
    mov x0, x19
    mov x16, #6             // SYS_CLOSE
    svc #0x80

    mov x0, x20
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

read_error:
    mov x0, #0
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Parse grid from buffer
parse_grid:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    adrp x19, buffer@PAGE
    add x19, x19, buffer@PAGEOFF
    adrp x20, grid@PAGE
    add x20, x20, grid@PAGEOFF
    mov x21, #0             // width
    mov x22, #0             // height
    mov x23, #0             // position

parse_loop:
    ldrb w24, [x19, x23]
    cbz w24, parse_done
    cmp w24, #10            // newline
    b.eq parse_newline

    strb w24, [x20], #1
    add x21, x21, #1
    add x23, x23, #1
    b parse_loop

parse_newline:
    cbz x21, skip_empty
    // Save width on first line
    cmp x22, #0
    b.ne 1f
    adrp x25, width@PAGE
    add x25, x25, width@PAGEOFF
    str x21, [x25]
1:
    add x22, x22, #1
    mov x21, #0
skip_empty:
    add x23, x23, #1
    b parse_loop

parse_done:
    // Handle last line
    cbz x21, 1f
    add x22, x22, #1
1:
    adrp x25, height@PAGE
    add x25, x25, height@PAGEOFF
    str x22, [x25]

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Get char at (row=x0, col=x1), returns char in w0 (0 if out of bounds)
get_char:
    stp x29, x30, [sp, #-16]!

    // Check row bounds
    adrp x2, height@PAGE
    add x2, x2, height@PAGEOFF
    ldr x2, [x2]
    cmp x0, x2
    b.ge oob
    cmp x0, #0
    b.lt oob

    // Check col bounds
    adrp x2, width@PAGE
    add x2, x2, width@PAGEOFF
    ldr x2, [x2]
    cmp x1, x2
    b.ge oob
    cmp x1, #0
    b.lt oob

    // Calculate offset
    mul x3, x0, x2
    add x3, x3, x1

    adrp x4, grid@PAGE
    add x4, x4, grid@PAGEOFF
    ldrb w0, [x4, x3]

    ldp x29, x30, [sp], #16
    ret

oob:
    mov w0, #0
    ldp x29, x30, [sp], #16
    ret

// Check if char is digit: w0=char, returns 1 if digit
is_digit:
    cmp w0, #'0'
    b.lt not_digit
    cmp w0, #'9'
    b.gt not_digit
    mov x0, #1
    ret
not_digit:
    mov x0, #0
    ret

// Check if char is symbol (not digit, not '.', not 0)
is_symbol:
    stp x29, x30, [sp, #-16]!
    str x19, [sp, #-16]!
    mov w19, w0

    bl is_digit
    cbnz x0, not_sym

    cmp w19, #'.'
    b.eq not_sym
    cmp w19, #0
    b.eq not_sym

    mov x0, #1
    ldr x19, [sp], #16
    ldp x29, x30, [sp], #16
    ret

not_sym:
    mov x0, #0
    ldr x19, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Check if number at (row, start_col, end_col) has adjacent symbol
// x0=row, x1=start_col, x2=end_col, returns 1 if adjacent
has_adjacent:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0             // row
    mov x20, x1             // start
    mov x21, x2             // end

    sub x22, x19, #1        // check_row start
    add x23, x19, #1        // check_row end

adj_row_loop:
    cmp x22, x23
    b.gt no_adj

    sub x24, x20, #1        // check_col start

adj_col_loop:
    cmp x24, x21
    b.gt adj_next_row

    mov x0, x22
    mov x1, x24
    bl get_char
    bl is_symbol
    cbnz x0, found_adj

    add x24, x24, #1
    b adj_col_loop

adj_next_row:
    add x22, x22, #1
    b adj_row_loop

no_adj:
    mov x0, #0
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

found_adj:
    mov x0, #1
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Solve Part 1
solve_part1:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, #0             // sum
    mov x20, #0             // row

    adrp x25, height@PAGE
    add x25, x25, height@PAGEOFF
    ldr x25, [x25]

p1_row_loop:
    cmp x20, x25
    b.ge p1_done

    mov x21, #0             // col
    adrp x26, width@PAGE
    add x26, x26, width@PAGEOFF
    ldr x26, [x26]

p1_col_loop:
    cmp x21, x26
    b.ge p1_next_row

    mov x0, x20
    mov x1, x21
    bl get_char
    bl is_digit
    cbz x0, p1_skip_col

    // Found number start
    mov x22, x21            // start_col
    mov x23, #0             // value

p1_num_loop:
    cmp x21, x26
    b.ge p1_num_done

    mov x0, x20
    mov x1, x21
    bl get_char
    mov w24, w0
    bl is_digit
    cbz x0, p1_num_done

    // Accumulate digit
    mov x0, #10
    mul x23, x23, x0
    sub w24, w24, #'0'
    add x23, x23, x24

    add x21, x21, #1
    b p1_num_loop

p1_num_done:
    // Check adjacent
    mov x0, x20
    mov x1, x22
    mov x2, x21
    bl has_adjacent
    cbz x0, p1_next_col

    add x19, x19, x23

p1_next_col:
    add x21, x21, #1
    b p1_col_loop

p1_skip_col:
    add x21, x21, #1
    b p1_col_loop

p1_next_row:
    add x20, x20, #1
    b p1_row_loop

p1_done:
    mov x0, x19
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Solve Part 2 - Find gears with exactly 2 adjacent numbers
solve_part2:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    mov x19, #0             // sum

    adrp x25, height@PAGE
    add x25, x25, height@PAGEOFF
    ldr x25, [x25]
    adrp x26, width@PAGE
    add x26, x26, width@PAGEOFF
    ldr x26, [x26]

    mov x20, #0             // row

p2_row_loop:
    cmp x20, x25
    b.ge p2_done

    mov x21, #0             // col

p2_col_loop:
    cmp x21, x26
    b.ge p2_next_row

    // Check if this is a gear (*)
    mov x0, x20
    mov x1, x21
    bl get_char
    cmp w0, #'*'
    b.ne p2_skip_col

    // Found gear - count adjacent numbers
    mov x22, #0             // count
    mov x23, #1             // product

    // Check all 8 directions + handle multi-digit numbers
    sub x27, x20, #1        // check_row = row - 1
    add x28, x20, #1        // row_end = row + 1

p2_check_row:
    cmp x27, x28
    b.gt p2_gear_done

    sub x24, x21, #1        // check_col = col - 1

p2_check_col:
    add x9, x21, #1         // col_end = col + 1
    cmp x24, x9
    b.gt p2_check_next_row

    mov x0, x27
    mov x1, x24
    bl get_char
    bl is_digit
    cbz x0, p2_check_next_col

    // Found a digit - find the full number
    mov x0, x27
    mov x1, x24
    bl find_number_at
    // x0 = number value, x1 = start_col

    // Check if we already counted this number (same row, overlapping position)
    // Simple check: if start_col < our check position and we're not at start
    cmp x1, x24
    b.lt p2_check_next_col

    // Count this number
    add x22, x22, #1
    mul x23, x23, x0

p2_check_next_col:
    add x24, x24, #1
    b p2_check_col

p2_check_next_row:
    add x27, x27, #1
    b p2_check_row

p2_gear_done:
    // If exactly 2 numbers, add product to sum
    cmp x22, #2
    b.ne p2_skip_col
    add x19, x19, x23

p2_skip_col:
    add x21, x21, #1
    b p2_col_loop

p2_next_row:
    add x20, x20, #1
    b p2_row_loop

p2_done:
    mov x0, x19
    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Find full number at position, returns value in x0, start_col in x1
// Input: x0=row, x1=col
find_number_at:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0             // row
    mov x20, x1             // col

    // Find start of number (go left)
find_start:
    cmp x20, #0
    b.le found_start
    sub x20, x20, #1
    mov x0, x19
    mov x1, x20
    bl get_char
    bl is_digit
    cbnz x0, find_start
    add x20, x20, #1        // went one too far

found_start:
    mov x21, x20            // save start_col
    mov x22, #0             // value

    // Parse number
parse_num:
    mov x0, x19
    mov x1, x20
    bl get_char
    mov w23, w0
    bl is_digit
    cbz x0, parse_done2

    mov x0, #10
    mul x22, x22, x0
    sub w23, w23, #'0'
    add x22, x22, x23
    add x20, x20, #1
    b parse_num

parse_done2:
    mov x0, x22
    mov x1, x21

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print string at x0
print_str:
    stp x29, x30, [sp, #-16]!
    mov x19, x0

    // Find length
    mov x1, #0
1:  ldrb w2, [x19, x1]
    cbz w2, 2f
    add x1, x1, #1
    b 1b

2:  mov x2, x1
    mov x1, x19
    mov x0, #1              // stdout
    mov x16, #4             // SYS_WRITE
    svc #0x80

    ldp x29, x30, [sp], #16
    ret

// Print number in x0
print_num:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    adrp x20, num_buffer@PAGE
    add x20, x20, num_buffer@PAGEOFF
    add x20, x20, #30
    strb wzr, [x20]

    mov x2, #10
1:  sub x20, x20, #1
    udiv x3, x19, x2
    msub x4, x3, x2, x19
    add w4, w4, #'0'
    strb w4, [x20]
    mov x19, x3
    cbnz x19, 1b

    mov x0, x20
    bl print_str

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
