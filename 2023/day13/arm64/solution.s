// =============================================================================
// Advent of Code 2023 - Day 13: Point of Incidence
// ARM64 Assembly Solution for macOS
//
// Problem: Find lines of reflection (mirrors) in 2D patterns of ash/rocks.
// Part 1: Find perfect reflections (zero differences across mirror line)
// Part 2: Find "smudged" reflections (exactly one difference across mirror)
//
// Algorithm: Unified diff-counting approach
// - For each potential reflection axis, count total character differences
// - Part 1 seeks diff_count == 0 (perfect match)
// - Part 2 seeks diff_count == 1 (exactly one smudge)
// - This unifies both parts with a single parameterized search
// =============================================================================

.global _start
.align 4

// -----------------------------------------------------------------------------
// macOS ARM64 BSD syscall numbers
// Note: macOS uses 0x2000000 + unix_syscall_number
// These constants are split for use with movz/movk due to ARM64 immediate limits
// -----------------------------------------------------------------------------
.equ SYSCALL_BASE_HI, 0x2000       // High 16 bits of syscall base
.equ SYS_EXIT_LO,     0x0001       // exit()
.equ SYS_READ_LO,     0x0003       // read()
.equ SYS_WRITE_LO,    0x0004       // write()
.equ SYS_OPEN_LO,     0x0005       // open()
.equ SYS_CLOSE_LO,    0x0006       // close()

// File access flags
.equ O_RDONLY, 0x0000

// -----------------------------------------------------------------------------
// Buffer size constants
// -----------------------------------------------------------------------------
.equ MAX_FILE_SIZE,        65536
.equ MAX_PATTERNS,         200
.equ MAX_LINES_PER_PATTERN, 50

// =============================================================================
// DATA SECTION
// =============================================================================
.data
input_path: .asciz "../input.txt"
part1_msg:  .asciz "Part 1: "
part2_msg:  .asciz "Part 2: "
newline:    .asciz "\n"

// =============================================================================
// BSS SECTION (uninitialized data)
// =============================================================================
.bss
.align 4
file_buffer:     .skip MAX_FILE_SIZE
// Pattern storage: each pattern = [line_count, width, line_ptr_1, line_ptr_2, ...]
patterns:        .skip MAX_PATTERNS * (2 + MAX_LINES_PER_PATTERN) * 8
pattern_count:   .skip 8
current_pattern: .skip (2 + MAX_LINES_PER_PATTERN) * 8
output_buffer:   .skip 32
file_size:       .skip 8

// =============================================================================
// TEXT SECTION
// =============================================================================
.text

// -----------------------------------------------------------------------------
// _start: Program entry point
// Opens input file, reads content, parses patterns, solves both parts
// -----------------------------------------------------------------------------
_start:
    // -------------------------------------------------------------------------
    // Open input file: open("../input.txt", O_RDONLY, 0)
    // -------------------------------------------------------------------------
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_OPEN_LO
    adrp    x0, input_path@PAGE
    add     x0, x0, input_path@PAGEOFF
    mov     x1, #O_RDONLY
    mov     x2, #0
    svc     #0
    mov     x19, x0                     // x19 = file descriptor

    // -------------------------------------------------------------------------
    // Read entire file: read(fd, buffer, max_size)
    // -------------------------------------------------------------------------
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_READ_LO
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #MAX_FILE_SIZE
    svc     #0

    // Store bytes read as file_size
    adrp    x1, file_size@PAGE
    add     x1, x1, file_size@PAGEOFF
    str     x0, [x1]

    // -------------------------------------------------------------------------
    // Close file: close(fd)
    // -------------------------------------------------------------------------
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_CLOSE_LO
    mov     x0, x19
    svc     #0

    // -------------------------------------------------------------------------
    // Parse all patterns from file buffer
    // -------------------------------------------------------------------------
    bl      parse_patterns

    // -------------------------------------------------------------------------
    // Part 1: Find perfect reflections (diff_target = 0)
    // -------------------------------------------------------------------------
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    bl      print_str

    mov     x0, #0                      // diff_target = 0 (perfect match)
    bl      solve
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // -------------------------------------------------------------------------
    // Part 2: Find smudged reflections (diff_target = 1)
    // -------------------------------------------------------------------------
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    bl      print_str

    mov     x0, #1                      // diff_target = 1 (one smudge)
    bl      solve
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // -------------------------------------------------------------------------
    // Exit program: exit(0)
    // -------------------------------------------------------------------------
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_EXIT_LO
    mov     x0, #0
    svc     #0

// =============================================================================
// parse_patterns: Parse input file into pattern data structures
//
// Input format: Patterns separated by blank lines, each pattern is grid of
//               '.' (ash) and '#' (rock) characters
//
// Output: Fills 'patterns' array and sets 'pattern_count'
//
// Register allocation:
//   x19 - current position in file buffer (preserved)
//   x20 - current position in patterns array (preserved)
//   x21 - total pattern count (preserved)
//   x22 - temp: current_pattern base address (preserved)
//   x23 - line count within current pattern (preserved)
//   x24 - width of current pattern (preserved)
// =============================================================================
parse_patterns:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    // Initialize parsing state
    adrp    x19, file_buffer@PAGE
    add     x19, x19, file_buffer@PAGEOFF
    adrp    x20, patterns@PAGE
    add     x20, x20, patterns@PAGEOFF
    mov     x21, #0                     // pattern_count = 0
    adrp    x22, current_pattern@PAGE
    add     x22, x22, current_pattern@PAGEOFF
    mov     x23, #0                     // line_count = 0
    mov     x24, #0                     // width = 0

.L_parse_loop:
    // Check for end of file
    adrp    x0, file_buffer@PAGE
    add     x0, x0, file_buffer@PAGEOFF
    adrp    x1, file_size@PAGE
    add     x1, x1, file_size@PAGEOFF
    ldr     x1, [x1]
    add     x0, x0, x1                  // x0 = file_buffer + file_size
    cmp     x19, x0
    b.ge    .L_parse_done

    // Check for blank line (pattern separator)
    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.ne    .L_parse_content_line

    // Blank line encountered - finalize current pattern if non-empty
    cbz     x23, .L_skip_blank_line
    bl      .L_save_current_pattern
    mov     x23, #0                     // Reset for next pattern
    mov     x24, #0

.L_skip_blank_line:
    add     x19, x19, #1                // Advance past newline
    b       .L_parse_loop

.L_parse_content_line:
    // Store pointer to this line in current_pattern
    add     x0, x22, #16                // Skip header (line_count, width)
    str     x19, [x0, x23, lsl #3]      // lines[line_count] = current_pos

    // Scan to end of line to determine width
    mov     x0, x19
.L_find_eol:
    ldrb    w1, [x0]
    cmp     w1, #'\n'
    b.eq    .L_found_eol
    cbz     w1, .L_found_eol            // Also stop at EOF
    add     x0, x0, #1
    b       .L_find_eol

.L_found_eol:
    sub     x1, x0, x19                 // Line width = end - start

    // Set width from first line of pattern
    cbnz    x23, .L_width_already_set
    mov     x24, x1
.L_width_already_set:

    add     x23, x23, #1                // line_count++
    add     x19, x0, #1                 // Advance past this line
    b       .L_parse_loop

.L_parse_done:
    // Finalize last pattern if non-empty
    cbz     x23, .L_parse_complete
    bl      .L_save_current_pattern

.L_parse_complete:
    // Store final pattern count
    adrp    x0, pattern_count@PAGE
    add     x0, x0, pattern_count@PAGEOFF
    str     x21, [x0]

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Helper: Save current pattern to patterns array
// Uses: x20 (patterns ptr), x21 (count), x22 (current_pattern), x23 (lines), x24 (width)
.L_save_current_pattern:
    str     x23, [x20], #8              // Store line_count
    str     x24, [x20], #8              // Store width

    // Copy line pointers from current_pattern to patterns
    add     x0, x22, #16                // Source: current_pattern.lines
    mov     x1, x23                     // Count
.L_copy_lines:
    cbz     x1, .L_copy_done
    ldr     x2, [x0], #8
    str     x2, [x20], #8
    sub     x1, x1, #1
    b       .L_copy_lines
.L_copy_done:
    add     x21, x21, #1                // pattern_count++
    ret

// =============================================================================
// solve: Sum reflection values for all patterns
//
// Input:
//   x0 - diff_target: 0 for Part 1 (perfect), 1 for Part 2 (smudged)
//
// Output:
//   x0 - Sum of all reflection values
//
// Register allocation:
//   x19 - diff_target (preserved)
//   x20 - running sum (preserved)
//   x21 - remaining pattern count (preserved)
//   x22 - current pattern pointer (preserved)
// =============================================================================
solve:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0                     // diff_target
    mov     x20, #0                     // sum = 0

    adrp    x21, pattern_count@PAGE
    add     x21, x21, pattern_count@PAGEOFF
    ldr     x21, [x21]                  // remaining patterns
    adrp    x22, patterns@PAGE
    add     x22, x22, patterns@PAGEOFF

.L_solve_loop:
    cbz     x21, .L_solve_done

    // Load pattern metadata
    ldr     x0, [x22], #8               // line_count
    ldr     x1, [x22], #8               // width
    mov     x2, x22                     // lines array pointer
    mov     x3, x19                     // diff_target

    bl      find_reflection
    add     x20, x20, x0                // sum += reflection_value

    // Advance past line pointers to next pattern
    ldr     x0, [x22, #-16]             // Re-read line_count
    add     x22, x22, x0, lsl #3        // Skip line_count * 8 bytes

    sub     x21, x21, #1
    b       .L_solve_loop

.L_solve_done:
    mov     x0, x20                     // Return sum

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// find_reflection: Find reflection axis in a single pattern
//
// Tries vertical reflection first, then horizontal.
// Returns: cols_left for vertical, 100 * rows_above for horizontal
//
// Input:
//   x0 - line_count
//   x1 - width
//   x2 - lines array pointer
//   x3 - diff_target
//
// Output:
//   x0 - Summary value (cols or rows*100)
//
// Register allocation:
//   x19 - line_count (preserved)
//   x20 - width (preserved)
//   x21 - lines pointer (preserved)
//   x22 - diff_target (preserved)
// =============================================================================
find_reflection:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0
    mov     x20, x1
    mov     x21, x2
    mov     x22, x3

    // Try vertical reflection first
    mov     x0, x19
    mov     x1, x20
    mov     x2, x21
    mov     x3, x22
    bl      find_vertical_reflection
    cbnz    x0, .L_find_refl_done       // Found vertical, return cols

    // Try horizontal reflection
    mov     x0, x19
    mov     x1, x20
    mov     x2, x21
    mov     x3, x22
    bl      find_horizontal_reflection
    mov     x1, #100
    mul     x0, x0, x1                  // Return rows * 100

.L_find_refl_done:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// find_vertical_reflection: Search for vertical mirror axis
//
// For each potential column axis (1 to width-1), count total differences
// across all rows when reflecting around that axis.
//
// Input:
//   x0 - line_count
//   x1 - width
//   x2 - lines array
//   x3 - diff_target
//
// Output:
//   x0 - Column position (1-indexed), or 0 if not found
//
// Register allocation:
//   x19 - line_count (preserved)
//   x20 - width (preserved)
//   x21 - lines array (preserved)
//   x22 - diff_target (preserved)
//   x23 - col: current column being tested (preserved)
//   x24 - total_diff: accumulated difference count (preserved)
//   x25 - row: current row index (preserved)
// =============================================================================
find_vertical_reflection:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0
    mov     x20, x1
    mov     x21, x2
    mov     x22, x3
    mov     x23, #1                     // Start at column 1

.L_vert_col_loop:
    cmp     x23, x20                    // col < width?
    b.ge    .L_vert_not_found

    mov     x24, #0                     // total_diff = 0
    mov     x25, #0                     // row = 0

.L_vert_row_loop:
    cmp     x25, x19                    // row < line_count?
    b.ge    .L_vert_check_result

    // Count differences for this row around column axis
    ldr     x0, [x21, x25, lsl #3]      // lines[row]
    mov     x1, x23                     // col
    mov     x2, x20                     // width
    bl      count_line_diff
    add     x24, x24, x0                // total_diff += diff

    // Early termination: if diff exceeds target, skip to next column
    cbz     x22, .L_vert_check_zero
    cmp     x24, #1
    b.gt    .L_vert_next_col
    b       .L_vert_row_next

.L_vert_check_zero:
    cbnz    x24, .L_vert_next_col       // Part 1: any diff means failure

.L_vert_row_next:
    add     x25, x25, #1
    b       .L_vert_row_loop

.L_vert_check_result:
    // Check if total_diff matches target
    cmp     x24, x22
    b.eq    .L_vert_found

.L_vert_next_col:
    add     x23, x23, #1
    b       .L_vert_col_loop

.L_vert_found:
    mov     x0, x23
    b       .L_vert_done

.L_vert_not_found:
    mov     x0, #0

.L_vert_done:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// find_horizontal_reflection: Search for horizontal mirror axis
//
// For each potential row axis (1 to line_count-1), count total differences
// when reflecting rows around that axis.
//
// Input:
//   x0 - line_count
//   x1 - width
//   x2 - lines array
//   x3 - diff_target
//
// Output:
//   x0 - Row position (1-indexed), or 0 if not found
//
// Register allocation:
//   x19 - line_count (preserved)
//   x20 - width (preserved)
//   x21 - lines array (preserved)
//   x22 - diff_target (preserved)
//   x23 - row: current row axis being tested (preserved)
//   x24 - total_diff: accumulated difference count (preserved)
//   x25 - min_len: pairs to compare (preserved)
//   x26 - i: pair index (preserved)
// =============================================================================
find_horizontal_reflection:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0
    mov     x20, x1
    mov     x21, x2
    mov     x22, x3
    mov     x23, #1                     // Start at row 1

.L_horiz_row_loop:
    cmp     x23, x19                    // row < line_count?
    b.ge    .L_horiz_not_found

    mov     x24, #0                     // total_diff = 0

    // Calculate reflection span: min(row, line_count - row)
    sub     x0, x19, x23
    cmp     x23, x0
    csel    x25, x23, x0, lt            // min_len

    mov     x26, #0                     // i = 0

.L_horiz_compare_loop:
    cmp     x26, x25                    // i < min_len?
    b.ge    .L_horiz_check_result

    // Compare lines[row - 1 - i] with lines[row + i]
    sub     x0, x23, #1
    sub     x0, x0, x26                 // top_idx
    add     x1, x23, x26                // bottom_idx

    ldr     x0, [x21, x0, lsl #3]       // top line
    ldr     x1, [x21, x1, lsl #3]       // bottom line
    mov     x2, x20                     // width
    bl      compare_lines
    add     x24, x24, x0                // total_diff += diff

    // Early termination check
    cbz     x22, .L_horiz_check_zero
    cmp     x24, #1
    b.gt    .L_horiz_next_row
    b       .L_horiz_compare_next

.L_horiz_check_zero:
    cbnz    x24, .L_horiz_next_row      // Part 1: any diff means failure

.L_horiz_compare_next:
    add     x26, x26, #1
    b       .L_horiz_compare_loop

.L_horiz_check_result:
    cmp     x24, x22
    b.eq    .L_horiz_found

.L_horiz_next_row:
    add     x23, x23, #1
    b       .L_horiz_row_loop

.L_horiz_found:
    mov     x0, x23
    b       .L_horiz_done

.L_horiz_not_found:
    mov     x0, #0

.L_horiz_done:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// count_line_diff: Count character differences in one row around vertical axis
//
// Compares pairs: line[col-1] vs line[col], line[col-2] vs line[col+1], etc.
// Only compares positions that exist on both sides.
//
// Input:
//   x0 - line pointer
//   x1 - col (reflection axis, 1-indexed)
//   x2 - width
//
// Output:
//   x0 - Number of character differences
//
// Register allocation:
//   x19 - line pointer (preserved)
//   x20 - col (preserved)
//   x21 - min_len: pairs to compare (preserved)
//   x22 - diff_count (preserved)
// =============================================================================
count_line_diff:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0
    mov     x20, x1
    mov     x22, #0                     // diff_count = 0

    // Calculate comparison span: min(col, width - col)
    sub     x0, x2, x20                 // width - col
    cmp     x20, x0
    csel    x21, x20, x0, lt            // min_len

    mov     x0, #0                      // i = 0
.L_diff_loop:
    cmp     x0, x21
    b.ge    .L_diff_done

    // Compare line[col - 1 - i] with line[col + i]
    sub     x1, x20, #1
    sub     x1, x1, x0                  // left_idx
    add     x2, x20, x0                 // right_idx

    ldrb    w3, [x19, x1]               // left char
    ldrb    w4, [x19, x2]               // right char

    cmp     w3, w4
    cinc    x22, x22, ne                // diff_count++ if not equal

    add     x0, x0, #1
    b       .L_diff_loop

.L_diff_done:
    mov     x0, x22

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// compare_lines: Count character differences between two lines
//
// Input:
//   x0 - line1 pointer
//   x1 - line2 pointer
//   x2 - width (number of characters to compare)
//
// Output:
//   x0 - Number of differences
//
// Register allocation:
//   x19 - diff_count (preserved)
//   x20 - i: loop counter (preserved)
// =============================================================================
compare_lines:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, #0                     // diff_count = 0
    mov     x20, #0                     // i = 0

.L_cmp_loop:
    cmp     x20, x2
    b.ge    .L_cmp_done

    ldrb    w3, [x0, x20]
    ldrb    w4, [x1, x20]

    cmp     w3, w4
    cinc    x19, x19, ne                // diff_count++ if not equal

    add     x20, x20, #1
    b       .L_cmp_loop

.L_cmp_done:
    mov     x0, x19

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// UTILITY FUNCTIONS
// =============================================================================

// -----------------------------------------------------------------------------
// print_str: Write null-terminated string to stdout
//
// Input:
//   x0 - String address
// -----------------------------------------------------------------------------
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0                     // Save string pointer

    // Calculate string length
    mov     x20, #0
.L_strlen:
    ldrb    w1, [x19, x20]
    cbz     w1, .L_strlen_done
    add     x20, x20, #1
    b       .L_strlen
.L_strlen_done:

    // Write to stdout: write(1, buffer, length)
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_WRITE_LO
    mov     x0, #1                      // fd = stdout
    mov     x1, x19                     // buffer
    mov     x2, x20                     // length
    svc     #0

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// -----------------------------------------------------------------------------
// print_num: Write decimal number to stdout
//
// Input:
//   x0 - Number to print
//
// Register allocation:
//   x19 - remaining value (preserved)
//   x20 - buffer write position (preserved)
//   x21 - digit count (preserved)
// -----------------------------------------------------------------------------
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0
    adrp    x20, output_buffer@PAGE
    add     x20, x20, output_buffer@PAGEOFF
    add     x20, x20, #31               // Start from end of buffer
    mov     x21, #0                     // digit_count = 0

    // Special case: zero
    cbnz    x19, .L_convert_loop
    mov     w22, #'0'
    strb    w22, [x20, #-1]!
    mov     x21, #1
    b       .L_print_digits

.L_convert_loop:
    cbz     x19, .L_print_digits

    mov     x1, #10
    udiv    x2, x19, x1                 // quotient
    msub    x3, x2, x1, x19             // remainder = value - quotient * 10

    add     w3, w3, #'0'                // Convert to ASCII
    strb    w3, [x20, #-1]!
    add     x21, x21, #1

    mov     x19, x2
    b       .L_convert_loop

.L_print_digits:
    // Write digits to stdout: write(1, buffer, digit_count)
    movz    x16, #SYSCALL_BASE_HI, lsl #16
    movk    x16, #SYS_WRITE_LO
    mov     x0, #1                      // fd = stdout
    mov     x1, x20                     // buffer
    mov     x2, x21                     // length
    svc     #0

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
