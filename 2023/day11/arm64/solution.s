// ============================================================================
// Day 11: Cosmic Expansion - ARM64 Assembly for macOS
// ============================================================================
//
// Algorithm Overview:
// ------------------
// This solution solves the Advent of Code Day 11 "Cosmic Expansion" puzzle.
// We need to find the sum of Manhattan distances between all pairs of galaxies,
// accounting for empty rows/columns that expand.
//
// Part 1: Empty rows/columns count as 2 units (expansion factor 2)
// Part 2: Empty rows/columns count as 1,000,000 units (expansion factor 1000000)
//
// Key Insight:
// Instead of actually expanding the grid, we can calculate distances by:
// 1. Base Manhattan distance = |r2-r1| + |c2-c1|
// 2. Add (expansion_factor - 1) for each empty row/column crossed
//
// Data Structures:
//   - grid[]: Character grid storing the image
//   - galaxies[]: Array of (row, col) pairs for each galaxy
//   - empty_rows[]: Boolean array marking empty rows
//   - empty_cols[]: Boolean array marking empty columns
//
// ============================================================================

.global _start
.align 4

// ============================================================================
// Constants
// ============================================================================
.equ BUFFER_SIZE, 32768
.equ MAX_GRID, 150
.equ MAX_GALAXIES, 500

// System call numbers (macOS ARM64)
.equ SYS_EXIT, 1
.equ SYS_READ, 3
.equ SYS_WRITE, 4
.equ SYS_OPEN, 5
.equ SYS_CLOSE, 6

// ============================================================================
// Macros for address loading
// ============================================================================

.macro load_addr reg, symbol
    adrp    \reg, \symbol@PAGE
    add     \reg, \reg, \symbol@PAGEOFF
.endm

.macro load_word reg, symbol, tmp
    adrp    \tmp, \symbol@PAGE
    add     \tmp, \tmp, \symbol@PAGEOFF
    ldr     \reg, [\tmp]
.endm

.macro store_word reg, symbol, tmp
    adrp    \tmp, \symbol@PAGE
    add     \tmp, \tmp, \symbol@PAGEOFF
    str     \reg, [\tmp]
.endm

// ============================================================================
// Data Section
// ============================================================================
.data
filename:       .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

.align 4
buffer:         .space BUFFER_SIZE
grid:           .space MAX_GRID * MAX_GRID          // The grid (flattened)
galaxies:       .space MAX_GALAXIES * 8             // Galaxy positions (row, col) pairs
empty_rows:     .space MAX_GRID                     // 1 if row is empty, 0 otherwise
empty_cols:     .space MAX_GRID                     // 1 if col is empty, 0 otherwise
num_buffer:     .space 32                           // For number to string conversion

// Grid dimensions and galaxy count
rows:           .word 0
cols:           .word 0
num_galaxies:   .word 0

// ============================================================================
// Text Section
// ============================================================================
.text

_start:
    // Open input file
    mov     x16, #SYS_OPEN
    load_addr x0, filename
    mov     x1, #0              // O_RDONLY
    mov     x2, #0
    svc     #0x80

    // Check for error
    cmp     x0, #0
    b.lt    exit_error

    mov     x19, x0             // Save file descriptor

    // Read file contents
    mov     x16, #SYS_READ
    mov     x0, x19
    load_addr x1, buffer
    mov     x2, #BUFFER_SIZE
    svc     #0x80

    mov     x20, x0             // Save bytes read

    // Close file
    mov     x16, #SYS_CLOSE
    mov     x0, x19
    svc     #0x80

    // Parse grid and find dimensions
    bl      parse_grid

    // Find all galaxy positions
    bl      find_galaxies

    // Find empty rows and columns
    bl      find_empty_rows_and_cols

    // Part 1: Calculate sum of distances with expansion factor 2
    mov     x0, #2
    bl      calculate_distances
    mov     x21, x0             // Save part 1 result

    // Part 2: Calculate sum of distances with expansion factor 1,000,000
    // 1000000 = 0xF4240 = 0x000F << 16 | 0x4240
    movz    x0, #0x4240
    movk    x0, #0x000F, lsl #16
    bl      calculate_distances
    mov     x22, x0             // Save part 2 result

    // Print Part 1 result
    load_addr x0, part1_msg
    mov     x1, #8
    bl      print_str

    mov     x0, x21
    bl      print_num

    load_addr x0, newline
    mov     x1, #1
    bl      print_str

    // Print Part 2 result
    load_addr x0, part2_msg
    mov     x1, #8
    bl      print_str

    mov     x0, x22
    bl      print_num

    load_addr x0, newline
    mov     x1, #1
    bl      print_str

    // Exit success
    mov     x0, #0
    mov     x16, #SYS_EXIT
    svc     #0x80

exit_error:
    mov     x0, #1
    mov     x16, #SYS_EXIT
    svc     #0x80

// ============================================================================
// parse_grid: Parse input buffer into grid array
// Sets: rows, cols global variables
// ============================================================================
parse_grid:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    load_addr x19, buffer
    load_addr x21, grid

    mov     x22, #0             // Current row
    mov     x23, #0             // Current col
    mov     x24, #0             // Max cols found
    mov     x25, x20            // Bytes to process

parse_grid_loop:
    cbz     x25, parse_grid_done
    ldrb    w0, [x19], #1
    sub     x25, x25, #1

    cmp     w0, #10             // newline
    b.eq    parse_grid_newline

    cmp     w0, #13             // carriage return - skip
    b.eq    parse_grid_loop

    // Store character using MAX_GRID stride
    mov     x1, x22             // row
    mov     x2, #MAX_GRID
    mul     x1, x1, x2
    add     x1, x1, x23         // + col
    strb    w0, [x21, x1]

    add     x23, x23, #1        // col++
    cmp     x23, x24
    csel    x24, x23, x24, gt   // max_cols = max(max_cols, col)

    b       parse_grid_loop

parse_grid_newline:
    cbz     x23, parse_grid_loop     // Skip empty lines
    add     x22, x22, #1             // row++
    mov     x23, #0                  // col = 0
    b       parse_grid_loop

parse_grid_done:
    // If last line didn't end with newline
    cbnz    x23, parse_grid_add_row
    b       parse_grid_store

parse_grid_add_row:
    add     x22, x22, #1

parse_grid_store:
    store_word w22, rows, x0
    store_word w24, cols, x0

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// find_galaxies: Find all '#' positions and store them in galaxies array
// Sets: num_galaxies global variable
// ============================================================================
find_galaxies:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    load_addr x19, grid
    load_addr x20, galaxies
    load_word w21, rows, x21
    load_word w22, cols, x22

    mov     x23, #0             // current row
    mov     x24, #0             // galaxy count

find_galaxies_row_loop:
    cmp     w23, w21
    b.ge    find_galaxies_done

    mov     x25, #0             // current col

find_galaxies_col_loop:
    cmp     w25, w22
    b.ge    find_galaxies_next_row

    // Calculate grid index
    mov     x0, x23
    mov     x1, #MAX_GRID
    mul     x0, x0, x1
    add     x0, x0, x25
    ldrb    w1, [x19, x0]

    cmp     w1, #'#'
    b.ne    find_galaxies_next_col

    // Store galaxy position
    lsl     x0, x24, #3         // galaxy_index * 8
    add     x0, x20, x0
    str     w23, [x0]           // row
    str     w25, [x0, #4]       // col
    add     x24, x24, #1

find_galaxies_next_col:
    add     x25, x25, #1
    b       find_galaxies_col_loop

find_galaxies_next_row:
    add     x23, x23, #1
    b       find_galaxies_row_loop

find_galaxies_done:
    store_word w24, num_galaxies, x0

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// find_empty_rows_and_cols: Identify rows/columns with no galaxies
// Sets: empty_rows[], empty_cols[] arrays
// ============================================================================
find_empty_rows_and_cols:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    load_addr x19, grid
    load_addr x20, empty_rows
    load_addr x21, empty_cols
    load_word w22, rows, x22
    load_word w23, cols, x23

    // Initialize empty_rows to 1 (assume empty)
    mov     x0, #0
find_empty_init_rows:
    cmp     w0, w22
    b.ge    find_empty_init_cols
    mov     w1, #1
    strb    w1, [x20, x0]
    add     x0, x0, #1
    b       find_empty_init_rows

find_empty_init_cols:
    // Initialize empty_cols to 1 (assume empty)
    mov     x0, #0
find_empty_init_cols_loop:
    cmp     w0, w23
    b.ge    find_empty_scan
    mov     w1, #1
    strb    w1, [x21, x0]
    add     x0, x0, #1
    b       find_empty_init_cols_loop

find_empty_scan:
    // Scan grid for '#' - mark row/col as not empty
    mov     x24, #0             // current row

find_empty_scan_row:
    cmp     w24, w22
    b.ge    find_empty_done

    mov     x25, #0             // current col

find_empty_scan_col:
    cmp     w25, w23
    b.ge    find_empty_scan_next_row

    // Check character at (row, col)
    mov     x0, x24
    mov     x1, #MAX_GRID
    mul     x0, x0, x1
    add     x0, x0, x25
    ldrb    w1, [x19, x0]

    cmp     w1, #'#'
    b.ne    find_empty_scan_next_col

    // Found a galaxy - mark row and col as not empty
    mov     w1, #0
    strb    w1, [x20, x24]      // empty_rows[row] = 0
    strb    w1, [x21, x25]      // empty_cols[col] = 0

find_empty_scan_next_col:
    add     x25, x25, #1
    b       find_empty_scan_col

find_empty_scan_next_row:
    add     x24, x24, #1
    b       find_empty_scan_row

find_empty_done:
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// calculate_distances: Calculate sum of Manhattan distances with expansion
// Input: x0 = expansion factor
// Output: x0 = sum of all pair distances
// ============================================================================
calculate_distances:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0             // expansion factor
    sub     x19, x19, #1        // expansion_factor - 1 (extra distance per empty row/col)

    load_addr x20, galaxies
    load_addr x21, empty_rows
    load_addr x22, empty_cols
    load_word w23, num_galaxies, x23

    mov     x24, #0             // total sum
    mov     x25, #0             // i (first galaxy index)

calc_dist_outer:
    cmp     w25, w23
    b.ge    calc_dist_done

    add     x26, x25, #1        // j = i + 1 (second galaxy index)

calc_dist_inner:
    cmp     w26, w23
    b.ge    calc_dist_next_outer

    // Get galaxy i position
    lsl     x0, x25, #3
    add     x0, x20, x0
    ldr     w1, [x0]            // r1
    ldr     w2, [x0, #4]        // c1

    // Get galaxy j position
    lsl     x0, x26, #3
    add     x0, x20, x0
    ldr     w3, [x0]            // r2
    ldr     w4, [x0, #4]        // c2

    // Save registers for use
    mov     w27, w1             // r1
    mov     w28, w2             // c1

    // Calculate row distance with expansion
    // min_r = min(r1, r2), max_r = max(r1, r2)
    cmp     w1, w3
    csel    w5, w1, w3, lt      // min_r
    csel    w6, w3, w1, lt      // max_r

    // row_dist = max_r - min_r
    sub     w7, w6, w5

    // Count empty rows between min_r and max_r (exclusive of max_r)
    mov     w8, w5              // current row = min_r

calc_dist_count_empty_rows:
    cmp     w8, w6
    b.ge    calc_dist_cols

    ldrb    w9, [x21, w8, uxtw]
    cbz     w9, calc_dist_next_empty_row

    // Empty row - add expansion_factor - 1
    add     x7, x7, x19

calc_dist_next_empty_row:
    add     w8, w8, #1
    b       calc_dist_count_empty_rows

calc_dist_cols:
    // Calculate column distance with expansion
    // min_c = min(c1, c2), max_c = max(c1, c2)
    cmp     w28, w4
    csel    w5, w28, w4, lt     // min_c
    csel    w6, w4, w28, lt     // max_c

    // col_dist = max_c - min_c
    sub     w8, w6, w5

    // Count empty cols between min_c and max_c (exclusive of max_c)
    mov     w9, w5              // current col = min_c

calc_dist_count_empty_cols:
    cmp     w9, w6
    b.ge    calc_dist_add_to_total

    ldrb    w10, [x22, w9, uxtw]
    cbz     w10, calc_dist_next_empty_col

    // Empty col - add expansion_factor - 1
    add     x8, x8, x19

calc_dist_next_empty_col:
    add     w9, w9, #1
    b       calc_dist_count_empty_cols

calc_dist_add_to_total:
    // total += row_dist + col_dist
    add     x0, x7, x8
    add     x24, x24, x0

    add     x26, x26, #1        // j++
    b       calc_dist_inner

calc_dist_next_outer:
    add     x25, x25, #1        // i++
    b       calc_dist_outer

calc_dist_done:
    mov     x0, x24

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print a string to stdout
// Input: x0 = string pointer, x1 = length
// ============================================================================
print_str:
    mov     x2, x1
    mov     x1, x0
    mov     x0, #1
    mov     x16, #SYS_WRITE
    svc     #0x80
    ret

// ============================================================================
// print_num: Print a decimal number to stdout
// Input: x0 = number to print
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    load_addr x20, num_buffer
    add     x20, x20, #30       // End of buffer
    mov     x1, #0              // Length

    // Handle 0 case
    cbnz    x19, print_num_loop
    mov     w2, #'0'
    sub     x20, x20, #1
    strb    w2, [x20]
    mov     x1, #1
    b       print_num_output

print_num_loop:
    cbz     x19, print_num_output
    mov     x2, #10
    udiv    x3, x19, x2
    msub    x4, x3, x2, x19     // remainder
    add     w4, w4, #'0'
    sub     x20, x20, #1
    strb    w4, [x20]
    add     x1, x1, #1
    mov     x19, x3
    b       print_num_loop

print_num_output:
    mov     x0, x20
    bl      print_str

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
