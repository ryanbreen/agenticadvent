.global _start
.align 4

// macOS syscall numbers
.equ SYS_READ, 0x2000003
.equ SYS_WRITE, 0x2000004
.equ SYS_OPEN, 0x2000005
.equ SYS_CLOSE, 0x2000006
.equ SYS_EXIT, 0x2000001

// File flags
.equ O_RDONLY, 0x0000

// Constants
.equ MAX_FILE_SIZE, 65536
.equ MAX_PATTERNS, 200
.equ MAX_LINES_PER_PATTERN, 50
.equ MAX_LINE_LENGTH, 50

.data
input_path: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.bss
.align 4
file_buffer: .skip MAX_FILE_SIZE
// Store patterns as array of line pointers
// Each pattern: [line_count, width, line1_ptr, line2_ptr, ...]
patterns: .skip MAX_PATTERNS * (2 + MAX_LINES_PER_PATTERN) * 8
pattern_count: .skip 8
current_pattern: .skip (2 + MAX_LINES_PER_PATTERN) * 8
output_buffer: .skip 32
file_size: .skip 8

.text
_start:
    // Open and read input file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0005
    adrp x0, input_path@PAGE
    add x0, x0, input_path@PAGEOFF
    mov x1, #O_RDONLY
    mov x2, #0
    svc #0

    mov x19, x0              // Save fd

    // Read file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0003
    mov x0, x19
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #MAX_FILE_SIZE
    svc #0

    // Save file size to global
    mov x1, x0
    adrp x0, file_size@PAGE
    add x0, x0, file_size@PAGEOFF
    str x1, [x0]

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0

    // Parse patterns
    bl parse_patterns

    // Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str

    mov x0, #0               // smudge_mode = 0 (no smudge)
    bl solve
    bl print_num

    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str

    mov x0, #1               // smudge_mode = 1 (with smudge)
    bl solve
    bl print_num

    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Exit
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    mov x0, #0
    svc #0

// Parse patterns from file buffer
// Returns: pattern_count set
parse_patterns:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    adrp x19, file_buffer@PAGE     // Current position in file
    add x19, x19, file_buffer@PAGEOFF
    adrp x20, patterns@PAGE        // Current pattern storage
    add x20, x20, patterns@PAGEOFF
    mov x21, #0              // Pattern count
    adrp x22, current_pattern@PAGE // Current pattern being built
    add x22, x22, current_pattern@PAGEOFF
    mov x23, #0              // Line count in current pattern
    mov x24, #0              // Width of current pattern

parse_loop:
    // Check if we've reached end of file
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    adrp x1, file_size@PAGE
    add x1, x1, file_size@PAGEOFF
    ldr x1, [x1]             // Load file_size
    add x0, x0, x1           // file_buffer + file_size
    cmp x19, x0
    b.ge parse_done

    // Check for blank line (pattern separator)
    ldrb w0, [x19]
    cmp w0, #10              // '\n'
    b.ne parse_line

    // Blank line - save current pattern if it has lines
    cmp x23, #0
    b.eq skip_blank

    // Save pattern
    str x23, [x20], #8       // Store line count
    str x24, [x20], #8       // Store width

    // Copy line pointers
    adrp x0, current_pattern@PAGE
    add x0, x0, current_pattern@PAGEOFF
    add x0, x0, #16          // Skip line_count and width placeholders
    mov x1, x23
copy_lines_loop:
    cbz x1, copy_lines_done
    ldr x2, [x0], #8
    str x2, [x20], #8
    sub x1, x1, #1
    b copy_lines_loop
copy_lines_done:

    add x21, x21, #1         // Increment pattern count
    mov x23, #0              // Reset line count
    mov x24, #0              // Reset width

skip_blank:
    add x19, x19, #1         // Skip newline
    b parse_loop

parse_line:
    // Store pointer to start of line
    adrp x0, current_pattern@PAGE
    add x0, x0, current_pattern@PAGEOFF
    add x0, x0, #16          // Skip line_count and width
    add x0, x0, x23, lsl #3  // Add offset for line number
    str x19, [x0]            // Store line pointer

    // Find end of line and calculate width
    mov x0, x19
find_eol:
    ldrb w1, [x0]
    cmp w1, #10              // '\n'
    b.eq found_eol
    cmp w1, #0               // EOF
    b.eq found_eol
    add x0, x0, #1
    b find_eol
found_eol:
    sub x1, x0, x19          // Width of line

    // Set width if first line
    cmp x23, #0
    b.ne skip_set_width
    mov x24, x1
skip_set_width:

    add x23, x23, #1         // Increment line count

    // Move to next line
    add x19, x0, #1
    b parse_loop

parse_done:
    // Save last pattern if it has lines
    cmp x23, #0
    b.eq parse_really_done

    str x23, [x20], #8       // Store line count
    str x24, [x20], #8       // Store width

    // Copy line pointers
    adrp x0, current_pattern@PAGE
    add x0, x0, current_pattern@PAGEOFF
    add x0, x0, #16
    mov x1, x23
copy_last_lines_loop:
    cbz x1, copy_last_lines_done
    ldr x2, [x0], #8
    str x2, [x20], #8
    sub x1, x1, #1
    b copy_last_lines_loop
copy_last_lines_done:

    add x21, x21, #1

parse_really_done:
    // Store pattern count
    adrp x0, pattern_count@PAGE
    add x0, x0, pattern_count@PAGEOFF
    str x21, [x0]

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Solve for all patterns
// x0 = smudge_mode (0 = no smudge, 1 = with smudge)
// Returns: sum in x0
solve:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0              // Save smudge_mode
    mov x20, #0              // Sum

    adrp x21, pattern_count@PAGE
    add x21, x21, pattern_count@PAGEOFF
    ldr x21, [x21]           // Load pattern count
    adrp x22, patterns@PAGE        // Pattern pointer
    add x22, x22, patterns@PAGEOFF

solve_loop:
    cbz x21, solve_done

    // Load pattern info
    ldr x0, [x22], #8        // line_count
    ldr x1, [x22], #8        // width
    mov x2, x22              // lines pointer
    mov x3, x19              // smudge_mode

    bl find_reflection
    add x20, x20, x0         // Add to sum

    // Move to next pattern (skip line pointers)
    ldr x0, [x22, #-16]      // Get line_count again
    add x22, x22, x0, lsl #3 // Skip line pointers

    sub x21, x21, #1
    b solve_loop

solve_done:
    mov x0, x20

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Find reflection for a pattern
// x0 = line_count, x1 = width, x2 = lines pointer, x3 = smudge_mode
// Returns: summary value in x0
find_reflection:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0              // line_count
    mov x20, x1              // width
    mov x21, x2              // lines
    mov x22, x3              // smudge_mode

    // Try vertical reflection
    mov x0, x19
    mov x1, x20
    mov x2, x21
    mov x3, x22
    bl find_vertical_reflection

    cmp x0, #0
    b.eq try_horizontal

    // Found vertical, return column count
    b find_reflection_done

try_horizontal:
    // Try horizontal reflection
    mov x0, x19
    mov x1, x20
    mov x2, x21
    mov x3, x22
    bl find_horizontal_reflection

    // Multiply by 100
    mov x1, #100
    mul x0, x0, x1

find_reflection_done:
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Find vertical reflection
// x0 = line_count, x1 = width, x2 = lines, x3 = smudge_mode
// Returns: columns to left, or 0
find_vertical_reflection:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0              // line_count
    mov x20, x1              // width
    mov x21, x2              // lines
    mov x22, x3              // smudge_mode
    mov x23, #1              // col (starting from 1)

vert_col_loop:
    cmp x23, x20             // col < width
    b.ge vert_not_found

    mov x24, #0              // total_diff
    mov x25, #0              // row index

vert_row_loop:
    cmp x25, x19             // row < line_count
    b.ge vert_check_diff

    // Get line pointer
    ldr x0, [x21, x25, lsl #3]
    mov x1, x23              // col
    mov x2, x20              // width
    bl count_line_diff

    add x24, x24, x0         // total_diff += diff

    // Early exit if too many diffs
    cmp x22, #0
    b.eq vert_check_perfect
    cmp x24, #1
    b.gt vert_next_col
    b vert_row_continue

vert_check_perfect:
    cmp x24, #0
    b.ne vert_next_col

vert_row_continue:
    add x25, x25, #1
    b vert_row_loop

vert_check_diff:
    cmp x22, #0
    b.eq vert_check_perfect_match
    cmp x24, #1
    b.eq vert_found
    b vert_next_col

vert_check_perfect_match:
    cmp x24, #0
    b.eq vert_found

vert_next_col:
    add x23, x23, #1
    b vert_col_loop

vert_found:
    mov x0, x23
    b vert_done

vert_not_found:
    mov x0, #0

vert_done:
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Find horizontal reflection
// x0 = line_count, x1 = width, x2 = lines, x3 = smudge_mode
// Returns: rows above, or 0
find_horizontal_reflection:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0              // line_count
    mov x20, x1              // width
    mov x21, x2              // lines
    mov x22, x3              // smudge_mode
    mov x23, #1              // row (starting from 1)

horiz_row_loop:
    cmp x23, x19             // row < line_count
    b.ge horiz_not_found

    mov x24, #0              // total_diff

    // Calculate min_len = min(row, line_count - row)
    sub x0, x19, x23         // line_count - row
    cmp x23, x0
    csel x25, x23, x0, lt    // min_len

    mov x26, #0              // i
horiz_compare_loop:
    cmp x26, x25             // i < min_len
    b.ge horiz_check_diff

    // Compare line[row - 1 - i] with line[row + i]
    sub x0, x23, #1
    sub x0, x0, x26          // top_idx = row - 1 - i
    add x1, x23, x26         // bottom_idx = row + i

    ldr x0, [x21, x0, lsl #3]  // top line
    ldr x1, [x21, x1, lsl #3]  // bottom line
    mov x2, x20              // width
    bl compare_lines

    add x24, x24, x0         // total_diff += diff

    // Early exit if too many diffs
    cmp x22, #0
    b.eq horiz_check_perfect_early
    cmp x24, #1
    b.gt horiz_next_row
    b horiz_compare_continue

horiz_check_perfect_early:
    cmp x24, #0
    b.ne horiz_next_row

horiz_compare_continue:
    add x26, x26, #1
    b horiz_compare_loop

horiz_check_diff:
    cmp x22, #0
    b.eq horiz_check_perfect_match
    cmp x24, #1
    b.eq horiz_found
    b horiz_next_row

horiz_check_perfect_match:
    cmp x24, #0
    b.eq horiz_found

horiz_next_row:
    add x23, x23, #1
    b horiz_row_loop

horiz_found:
    mov x0, x23
    b horiz_done

horiz_not_found:
    mov x0, #0

horiz_done:
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Count differences in a line around a vertical reflection at col
// x0 = line pointer, x1 = col, x2 = width
// Returns: difference count in x0
count_line_diff:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0              // line
    mov x20, x1              // col
    mov x21, x2              // width
    mov x22, #0              // diff count

    // min_len = min(col, width - col)
    sub x0, x21, x20         // width - col
    cmp x20, x0
    csel x21, x20, x0, lt    // min_len in x21

    mov x0, #0               // i
count_diff_loop:
    cmp x0, x21
    b.ge count_diff_done

    // Compare line[col - 1 - i] with line[col + i]
    sub x1, x20, #1
    sub x1, x1, x0           // left_idx = col - 1 - i
    add x2, x20, x0          // right_idx = col + i

    ldrb w3, [x19, x1]       // left char
    ldrb w4, [x19, x2]       // right char

    cmp w3, w4
    b.eq count_diff_same
    add x22, x22, #1

count_diff_same:
    add x0, x0, #1
    b count_diff_loop

count_diff_done:
    mov x0, x22

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Compare two lines
// x0 = line1, x1 = line2, x2 = width
// Returns: difference count in x0
compare_lines:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, #0              // diff count
    mov x20, #0              // i

compare_loop:
    cmp x20, x2
    b.ge compare_done

    ldrb w3, [x0, x20]
    ldrb w4, [x1, x20]

    cmp w3, w4
    b.eq compare_same
    add x19, x19, #1

compare_same:
    add x20, x20, #1
    b compare_loop

compare_done:
    mov x0, x19

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print string (null-terminated)
// x0 = string address
print_str:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x0

    // Find string length
    mov x20, #0
strlen_loop:
    ldrb w1, [x19, x20]
    cbz w1, strlen_done
    add x20, x20, #1
    b strlen_loop
strlen_done:

    // Write
    movz x16, #0x2000, lsl #16
    movk x16, #0x0004
    mov x0, #1
    mov x1, x19
    mov x2, x20
    svc #0

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print number in x0
print_num:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0
    adrp x20, output_buffer@PAGE
    add x20, x20, output_buffer@PAGEOFF
    add x20, x20, #31        // Start from end of buffer
    mov x21, #0              // digit count

    // Handle zero
    cbnz x19, convert_loop
    mov w22, #'0'
    strb w22, [x20, #-1]!
    mov x21, #1
    b print_num_write

convert_loop:
    cbz x19, print_num_write

    mov x1, #10
    udiv x2, x19, x1
    msub x3, x2, x1, x19     // remainder

    add w3, w3, #'0'
    strb w3, [x20, #-1]!
    add x21, x21, #1

    mov x19, x2
    b convert_loop

print_num_write:
    movz x16, #0x2000, lsl #16
    movk x16, #0x0004
    mov x0, #1
    mov x1, x20
    mov x2, x21
    svc #0

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
