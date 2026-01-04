// Advent of Code 2023 Day 24 - Never Tell Me The Odds
// ARM64 Assembly for macOS
//
// Part 1: Count 2D intersections within bounds [2e14, 4e14]
// Part 2: Find rock position that hits all hailstones

.global _start
.align 4

// System call numbers for macOS
.equ SYS_EXIT, 1
.equ SYS_READ, 3
.equ SYS_WRITE, 4
.equ SYS_OPEN, 5
.equ SYS_CLOSE, 6

// Constants
.equ O_RDONLY, 0
.equ MAX_HAILSTONES, 400
.equ HAILSTONE_SIZE, 48      // 6 x 8 bytes: px, py, pz, vx, vy, vz

.section __DATA,__data
filename:       .asciz "../input.txt"
part1_prefix:   .asciz "Part 1: "
part2_prefix:   .asciz "Part 2: "
newline:        .asciz "\n"

// Bounds for Part 1 (200000000000000 = 0xB5E620F48000)
.align 8
min_bound:      .quad 200000000000000
max_bound:      .quad 400000000000000
epsilon:        .double 1.0e-10

.section __BSS,__bss
.align 8
file_buffer:    .space 32768       // Input file buffer
hailstones:     .space MAX_HAILSTONES * HAILSTONE_SIZE  // Array of hailstones
hailstone_count: .space 8          // Number of hailstones
num_buffer:     .space 32          // For number output
matrix:         .space 512         // 4x5 matrix for Gaussian elimination (rational pairs)
solution:       .space 64          // Solution vector

.section __TEXT,__text

//=============================================================================
// Main entry point
//=============================================================================
_start:
    // Open input file
    mov     x16, #SYS_OPEN
    adrp    x0, filename@PAGE
    add     x0, x0, filename@PAGEOFF
    mov     x1, #O_RDONLY
    mov     x2, #0
    svc     #0x80

    // Check for error
    cmp     x0, #0
    b.lt    exit_error

    mov     x19, x0             // Save file descriptor

    // Read file content
    mov     x16, #SYS_READ
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #32768
    svc     #0x80

    mov     x20, x0             // Save bytes read

    // Close file
    mov     x16, #SYS_CLOSE
    mov     x0, x19
    svc     #0x80

    // Parse input
    adrp    x0, file_buffer@PAGE
    add     x0, x0, file_buffer@PAGEOFF
    add     x1, x0, x20         // End of buffer
    bl      parse_input

    // Part 1
    bl      part1
    mov     x21, x0             // Save Part 1 result

    // Print Part 1
    adrp    x0, part1_prefix@PAGE
    add     x0, x0, part1_prefix@PAGEOFF
    bl      print_string
    mov     x0, x21
    bl      print_number
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Part 2
    bl      part2
    mov     x21, x0             // Save Part 2 result

    // Print Part 2
    adrp    x0, part2_prefix@PAGE
    add     x0, x0, part2_prefix@PAGEOFF
    bl      print_string
    mov     x0, x21
    bl      print_number
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Exit success
    mov     x0, #0
    mov     x16, #SYS_EXIT
    svc     #0x80

exit_error:
    mov     x0, #1
    mov     x16, #SYS_EXIT
    svc     #0x80

//=============================================================================
// Parse input: Parse hailstones from buffer
// Input: x0 = buffer start, x1 = buffer end
//=============================================================================
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    mov     x19, x0             // Current position
    mov     x20, x1             // End of buffer
    adrp    x21, hailstones@PAGE
    add     x21, x21, hailstones@PAGEOFF  // Hailstone array
    mov     x22, #0             // Hailstone count

parse_loop:
    cmp     x19, x20
    b.ge    parse_done

    // Skip whitespace
    ldrb    w23, [x19]
    cmp     w23, #' '
    b.eq    skip_ws
    cmp     w23, #'\n'
    b.eq    skip_ws
    cmp     w23, #'\r'
    b.eq    skip_ws
    b       parse_hailstone

skip_ws:
    add     x19, x19, #1
    b       parse_loop

parse_hailstone:
    // Parse px
    mov     x0, x19
    mov     x1, x20
    bl      parse_signed_number
    mov     x19, x1             // Update position
    str     x0, [x21, #0]       // Store px

    // Skip ", "
    bl      skip_separator
    mov     x19, x0

    // Parse py
    mov     x0, x19
    mov     x1, x20
    bl      parse_signed_number
    mov     x19, x1
    str     x0, [x21, #8]       // Store py

    // Skip ", "
    bl      skip_separator
    mov     x19, x0

    // Parse pz
    mov     x0, x19
    mov     x1, x20
    bl      parse_signed_number
    mov     x19, x1
    str     x0, [x21, #16]      // Store pz

    // Skip " @ "
    bl      skip_at_separator
    mov     x19, x0

    // Parse vx
    mov     x0, x19
    mov     x1, x20
    bl      parse_signed_number
    mov     x19, x1
    str     x0, [x21, #24]      // Store vx

    // Skip ", "
    bl      skip_separator
    mov     x19, x0

    // Parse vy
    mov     x0, x19
    mov     x1, x20
    bl      parse_signed_number
    mov     x19, x1
    str     x0, [x21, #32]      // Store vy

    // Skip ", "
    bl      skip_separator
    mov     x19, x0

    // Parse vz
    mov     x0, x19
    mov     x1, x20
    bl      parse_signed_number
    mov     x19, x1
    str     x0, [x21, #40]      // Store vz

    // Move to next hailstone
    add     x21, x21, #HAILSTONE_SIZE
    add     x22, x22, #1
    b       parse_loop

parse_done:
    adrp    x0, hailstone_count@PAGE
    add     x0, x0, hailstone_count@PAGEOFF
    str     x22, [x0]

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Skip comma and optional spaces
// Input: x19 = current position
// Output: x0 = new position
skip_separator:
    mov     x0, x19
skip_sep_loop:
    ldrb    w1, [x0]
    cmp     w1, #','
    b.eq    skip_sep_next
    cmp     w1, #' '
    b.eq    skip_sep_next
    ret
skip_sep_next:
    add     x0, x0, #1
    b       skip_sep_loop

// Skip " @ " separator
// Input: x19 = current position
// Output: x0 = new position
skip_at_separator:
    mov     x0, x19
skip_at_loop:
    ldrb    w1, [x0]
    cmp     w1, #'@'
    b.eq    skip_at_next
    cmp     w1, #' '
    b.eq    skip_at_next
    ret
skip_at_next:
    add     x0, x0, #1
    b       skip_at_loop

// Parse signed number
// Input: x0 = string position, x1 = end
// Output: x0 = number, x1 = new position
parse_signed_number:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    mov     x20, #0             // Negative flag

    // Check for negative sign
    ldrb    w2, [x19]
    cmp     w2, #'-'
    b.ne    parse_num_start
    mov     x20, #1
    add     x19, x19, #1

parse_num_start:
    mov     x0, #0              // Result

parse_num_loop:
    ldrb    w2, [x19]
    cmp     w2, #'0'
    b.lt    parse_num_done
    cmp     w2, #'9'
    b.gt    parse_num_done

    // Multiply by 10 and add digit
    mov     x3, #10
    mul     x0, x0, x3
    sub     w2, w2, #'0'
    add     x0, x0, x2
    add     x19, x19, #1
    b       parse_num_loop

parse_num_done:
    // Apply negative if needed
    cbz     x20, parse_num_ret
    neg     x0, x0

parse_num_ret:
    mov     x1, x19
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

//=============================================================================
// Part 1: Count 2D intersections in test area
// Uses floating-point arithmetic
//=============================================================================
part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!
    stp     d8, d9, [sp, #-16]!
    stp     d10, d11, [sp, #-16]!
    stp     d12, d13, [sp, #-16]!
    stp     d14, d15, [sp, #-16]!

    // Load bounds into FP registers
    adrp    x0, min_bound@PAGE
    add     x0, x0, min_bound@PAGEOFF
    ldr     x1, [x0]
    scvtf   d8, x1              // d8 = min_bound as double

    adrp    x0, max_bound@PAGE
    add     x0, x0, max_bound@PAGEOFF
    ldr     x1, [x0]
    scvtf   d9, x1              // d9 = max_bound as double

    // Get hailstone count
    adrp    x0, hailstone_count@PAGE
    add     x0, x0, hailstone_count@PAGEOFF
    ldr     x19, [x0]           // x19 = count

    adrp    x20, hailstones@PAGE
    add     x20, x20, hailstones@PAGEOFF  // x20 = hailstones array

    mov     x21, #0             // x21 = i (outer loop)
    mov     x25, #0             // x25 = intersection count

part1_outer:
    cmp     x21, x19
    b.ge    part1_done

    // Calculate offset for hailstone i
    mov     x0, #HAILSTONE_SIZE
    mul     x22, x21, x0        // x22 = offset for i

    add     x23, x21, #1        // x23 = j (inner loop)

part1_inner:
    cmp     x23, x19
    b.ge    part1_next_outer

    // Calculate offset for hailstone j
    mov     x0, #HAILSTONE_SIZE
    mul     x24, x23, x0        // x24 = offset for j

    // Load hailstone i values
    add     x0, x20, x22
    ldr     x1, [x0, #0]        // px1
    scvtf   d0, x1
    ldr     x1, [x0, #8]        // py1
    scvtf   d1, x1
    ldr     x1, [x0, #24]       // vx1
    scvtf   d2, x1
    ldr     x1, [x0, #32]       // vy1
    scvtf   d3, x1

    // Load hailstone j values
    add     x0, x20, x24
    ldr     x1, [x0, #0]        // px2
    scvtf   d4, x1
    ldr     x1, [x0, #8]        // py2
    scvtf   d5, x1
    ldr     x1, [x0, #24]       // vx2
    scvtf   d6, x1
    ldr     x1, [x0, #32]       // vy2
    scvtf   d7, x1

    // Calculate determinant: det = vx1 * (-vy2) - (-vx2) * vy1
    // = -vx1*vy2 + vx2*vy1
    // = vx2*vy1 - vx1*vy2
    fmul    d10, d6, d3         // vx2 * vy1
    fmul    d11, d2, d7         // vx1 * vy2
    fsub    d10, d10, d11       // det = vx2*vy1 - vx1*vy2

    // Check if parallel (det == 0)
    fabs    d11, d10
    adrp    x0, epsilon@PAGE
    add     x0, x0, epsilon@PAGEOFF
    ldr     d12, [x0]
    fcmp    d11, d12
    b.lt    part1_next_j        // Skip if parallel

    // Calculate dx = px2 - px1, dy = py2 - py1
    fsub    d11, d4, d0         // dx
    fsub    d12, d5, d1         // dy

    // t1 = (dx * (-vy2) - (-vx2) * dy) / det
    //    = (-dx*vy2 + vx2*dy) / det
    //    = (vx2*dy - dx*vy2) / det
    fmul    d13, d6, d12        // vx2 * dy
    fmul    d14, d11, d7        // dx * vy2
    fsub    d13, d13, d14       // vx2*dy - dx*vy2
    fdiv    d13, d13, d10       // t1

    // t2 = (vx1 * dy - dx * vy1) / det
    fmul    d14, d2, d12        // vx1 * dy
    fmul    d15, d11, d3        // dx * vy1
    fsub    d14, d14, d15       // vx1*dy - dx*vy1
    fdiv    d14, d14, d10       // t2

    // Check if t1 >= 0 and t2 >= 0 (future intersection)
    fmov    d15, #0.0
    fcmp    d13, d15
    b.lt    part1_next_j
    fcmp    d14, d15
    b.lt    part1_next_j

    // Calculate intersection point: x = px1 + vx1 * t1
    fmul    d11, d2, d13        // vx1 * t1
    fadd    d11, d0, d11        // x = px1 + vx1 * t1

    // y = py1 + vy1 * t1
    fmul    d12, d3, d13        // vy1 * t1
    fadd    d12, d1, d12        // y = py1 + vy1 * t1

    // Check if x in bounds
    fcmp    d11, d8             // x >= min
    b.lt    part1_next_j
    fcmp    d11, d9             // x <= max
    b.gt    part1_next_j

    // Check if y in bounds
    fcmp    d12, d8             // y >= min
    b.lt    part1_next_j
    fcmp    d12, d9             // y <= max
    b.gt    part1_next_j

    // Valid intersection found
    add     x25, x25, #1

part1_next_j:
    add     x23, x23, #1
    b       part1_inner

part1_next_outer:
    add     x21, x21, #1
    b       part1_outer

part1_done:
    mov     x0, x25

    ldp     d14, d15, [sp], #16
    ldp     d12, d13, [sp], #16
    ldp     d10, d11, [sp], #16
    ldp     d8, d9, [sp], #16
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

//=============================================================================
// Part 2: Find rock position using Gaussian elimination with floating-point
//
// Strategy: Use the linearization technique from Python solution.
// For hailstones i and j, we get linear equation:
// (vy_i - vy_j)*rx + (vx_j - vx_i)*ry + (py_j - py_i)*rvx + (px_i - px_j)*rvy
//   = px_i*vy_i - py_i*vx_i - (px_j*vy_j - py_j*vx_j)
//
// Using double-precision floating-point for Gaussian elimination.
//=============================================================================

part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!
    stp     d8, d9, [sp, #-16]!
    stp     d10, d11, [sp, #-16]!
    stp     d12, d13, [sp, #-16]!
    stp     d14, d15, [sp, #-16]!
    sub     sp, sp, #320        // Local stack for matrices (2 * 4*5*8 = 320 bytes)

    // Use first 5 hailstones for 4 equations
    adrp    x19, hailstones@PAGE
    add     x19, x19, hailstones@PAGEOFF

    // Build 4x5 augmented matrix for XY plane (as doubles)
    // Each row: [a, b, c, d, e] where a*rx + b*ry + c*rvx + d*rvy = e
    // Total: 4 rows * 5 cols * 8 bytes = 160 bytes

    mov     x20, sp             // XY matrix base pointer
    mov     x21, #0             // Row counter

build_xy_loop_fp:
    cmp     x21, #4
    b.ge    solve_xy_matrix_fp

    // Get hailstone i and i+1
    mov     x0, #HAILSTONE_SIZE
    mul     x22, x21, x0        // Offset for hailstone i
    add     x23, x21, #1
    mul     x23, x23, x0        // Offset for hailstone i+1

    add     x24, x19, x22       // Pointer to h[i]
    add     x25, x19, x23       // Pointer to h[i+1]

    // Load h[i] values and convert to double
    ldr     x0, [x24, #0]       // px1
    scvtf   d0, x0
    ldr     x0, [x24, #8]       // py1
    scvtf   d1, x0
    ldr     x0, [x24, #24]      // vx1
    scvtf   d2, x0
    ldr     x0, [x24, #32]      // vy1
    scvtf   d3, x0

    // Load h[i+1] values and convert to double
    ldr     x0, [x25, #0]       // px2
    scvtf   d4, x0
    ldr     x0, [x25, #8]       // py2
    scvtf   d5, x0
    ldr     x0, [x25, #24]      // vx2
    scvtf   d6, x0
    ldr     x0, [x25, #32]      // vy2
    scvtf   d7, x0

    // Calculate coefficients:
    // a = vy1 - vy2
    fsub    d8, d3, d7
    // b = vx2 - vx1
    fsub    d9, d6, d2
    // c = py2 - py1
    fsub    d10, d5, d1
    // d = px1 - px2
    fsub    d11, d0, d4

    // e = px1*vy1 - py1*vx1 - (px2*vy2 - py2*vx2)
    fmul    d12, d0, d3         // px1 * vy1
    fmul    d13, d1, d2         // py1 * vx1
    fsub    d12, d12, d13       // px1*vy1 - py1*vx1

    fmul    d13, d4, d7         // px2 * vy2
    fmul    d14, d5, d6         // py2 * vx2
    fsub    d13, d13, d14       // px2*vy2 - py2*vx2

    fsub    d12, d12, d13       // e

    // Store row (5 doubles = 40 bytes per row)
    mov     x26, #40
    mul     x26, x21, x26       // Row offset
    add     x27, x20, x26       // Row pointer

    str     d8, [x27, #0]       // a
    str     d9, [x27, #8]       // b
    str     d10, [x27, #16]     // c
    str     d11, [x27, #24]     // d
    str     d12, [x27, #32]     // e

    add     x21, x21, #1
    b       build_xy_loop_fp

solve_xy_matrix_fp:
    // Perform Gaussian elimination on XY matrix
    mov     x0, x20             // Matrix pointer
    mov     x1, #4              // Number of rows/columns
    bl      gaussian_eliminate_fp

    // Extract rx, ry from solution (row 0 and row 1, last column)
    // rx = matrix[0][4]
    ldr     d8, [x20, #32]      // rx
    // ry = matrix[1][4]
    add     x0, x20, #40
    ldr     d9, [x0, #32]       // ry

    // Now build matrix for XZ plane to get rz
    add     x20, sp, #160       // Second matrix area
    mov     x21, #0             // Row counter

build_xz_loop_fp:
    cmp     x21, #4
    b.ge    solve_xz_matrix_fp

    // Get hailstone i and i+1
    mov     x0, #HAILSTONE_SIZE
    mul     x24, x21, x0        // Offset for hailstone i
    add     x25, x21, #1
    mul     x25, x25, x0        // Offset for hailstone i+1

    add     x26, x19, x24       // Pointer to h[i]
    add     x27, x19, x25       // Pointer to h[i+1]

    // Load h[i] values and convert to double
    ldr     x0, [x26, #0]       // px1
    scvtf   d0, x0
    ldr     x0, [x26, #16]      // pz1
    scvtf   d1, x0
    ldr     x0, [x26, #24]      // vx1
    scvtf   d2, x0
    ldr     x0, [x26, #40]      // vz1
    scvtf   d3, x0

    // Load h[i+1] values and convert to double
    ldr     x0, [x27, #0]       // px2
    scvtf   d4, x0
    ldr     x0, [x27, #16]      // pz2
    scvtf   d5, x0
    ldr     x0, [x27, #24]      // vx2
    scvtf   d6, x0
    ldr     x0, [x27, #40]      // vz2
    scvtf   d7, x0

    // Calculate coefficients (same structure as XY but with Z):
    // a = vz1 - vz2
    fsub    d10, d3, d7
    // b = vx2 - vx1
    fsub    d11, d6, d2
    // c = pz2 - pz1
    fsub    d12, d5, d1
    // d = px1 - px2
    fsub    d13, d0, d4

    // e = px1*vz1 - pz1*vx1 - (px2*vz2 - pz2*vx2)
    fmul    d14, d0, d3         // px1 * vz1
    fmul    d15, d1, d2         // pz1 * vx1
    fsub    d14, d14, d15

    fmul    d15, d4, d7         // px2 * vz2
    fmul    d16, d5, d6         // pz2 * vx2
    fsub    d15, d15, d16

    fsub    d14, d14, d15       // e

    // Store row
    mov     x28, #40
    mul     x28, x21, x28
    add     x28, x20, x28

    str     d10, [x28, #0]      // a
    str     d11, [x28, #8]      // b
    str     d12, [x28, #16]     // c
    str     d13, [x28, #24]     // d
    str     d14, [x28, #32]     // e

    add     x21, x21, #1
    b       build_xz_loop_fp

solve_xz_matrix_fp:
    // Solve XZ matrix
    mov     x0, x20
    mov     x1, #4
    bl      gaussian_eliminate_fp

    // Extract rz from row 1 (second unknown)
    add     x0, x20, #40
    ldr     d10, [x0, #32]      // rz

    // Result = rx + ry + rz (d8 + d9 + d10)
    fadd    d0, d8, d9
    fadd    d0, d0, d10

    // Round to nearest integer
    frintn  d0, d0
    fcvtzs  x0, d0

    add     sp, sp, #320
    ldp     d14, d15, [sp], #16
    ldp     d12, d13, [sp], #16
    ldp     d10, d11, [sp], #16
    ldp     d8, d9, [sp], #16
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

//=============================================================================
// Gaussian elimination for 4x5 augmented matrix (floating-point)
// Input: x0 = matrix pointer, x1 = size (4)
// Matrix is stored as rows of 5 doubles
// Each row is 40 bytes (5 * 8)
//=============================================================================
gaussian_eliminate_fp:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0             // Matrix pointer
    mov     x20, x1             // Size (4)

    mov     x21, #0             // col (pivot column)

gauss_col_loop_fp:
    cmp     x21, x20
    b.ge    gauss_back_sub_fp

    // Find pivot row (max absolute value in column)
    mov     x22, x21            // max_row = col
    mov     x23, #40
    mul     x23, x21, x23       // Row offset for col
    add     x23, x19, x23       // Pointer to row[col]

    // Get current pivot value
    mov     x24, #8
    mul     x24, x21, x24       // Column offset
    add     x24, x23, x24       // Pointer to matrix[col][col]
    ldr     d0, [x24]           // pivot value
    fabs    d1, d0              // |pivot|

    // Search for better pivot
    add     x28, x21, #1        // row = col + 1
find_pivot_loop_fp:
    cmp     x28, x20
    b.ge    do_swap_fp

    // Get matrix[row][col]
    mov     x0, #40
    mul     x0, x28, x0
    add     x0, x19, x0
    mov     x1, #8
    mul     x1, x21, x1
    add     x0, x0, x1
    ldr     d2, [x0]            // value
    fabs    d3, d2              // |value|

    // Compare with max
    fcmp    d3, d1
    b.le    next_pivot_row_fp
    fmov    d1, d3
    mov     x22, x28            // max_row = row

next_pivot_row_fp:
    add     x28, x28, #1
    b       find_pivot_loop_fp

do_swap_fp:
    // Swap rows col and max_row if needed
    cmp     x22, x21
    b.eq    no_swap_fp

    // Calculate row pointers
    mov     x0, #40
    mul     x23, x21, x0        // Offset for row col
    add     x23, x19, x23
    mul     x24, x22, x0        // Offset for row max_row
    add     x24, x19, x24

    // Swap all 5 entries (40 bytes)
    mov     x25, #0
swap_loop_fp:
    cmp     x25, #40
    b.ge    no_swap_fp
    ldr     d0, [x23, x25]
    ldr     d1, [x24, x25]
    str     d1, [x23, x25]
    str     d0, [x24, x25]
    add     x25, x25, #8
    b       swap_loop_fp

no_swap_fp:
    // Get pivot value
    mov     x0, #40
    mul     x0, x21, x0
    add     x0, x19, x0
    mov     x1, #8
    mul     x1, x21, x1
    add     x0, x0, x1
    ldr     d0, [x0]            // pivot

    // Skip if pivot is zero
    fmov    d1, #0.0
    fcmp    d0, d1
    b.eq    gauss_next_col_fp

    // Eliminate column in lower rows
    add     x25, x21, #1        // row = col + 1

eliminate_loop_fp:
    cmp     x25, x20
    b.ge    gauss_next_col_fp

    // Get matrix[row][col]
    mov     x0, #40
    mul     x26, x25, x0
    add     x26, x19, x26       // Pointer to row[row]
    mov     x0, #8
    mul     x0, x21, x0
    add     x27, x26, x0        // Pointer to matrix[row][col]
    ldr     d1, [x27]           // row_val = matrix[row][col]

    // Skip if already zero
    fmov    d2, #0.0
    fcmp    d1, d2
    b.eq    eliminate_next_fp

    // factor = matrix[row][col] / pivot
    fdiv    d2, d1, d0          // d2 = factor

    // Subtract factor * pivot_row from current row
    // For each column j from col to 4:
    //   matrix[row][j] -= factor * matrix[col][j]

    mov     x3, x21             // j = col

subtract_loop_fp:
    cmp     x3, #5
    b.ge    eliminate_next_fp

    // Get pivot row entry: matrix[col][j]
    mov     x4, #40
    mul     x4, x21, x4
    add     x4, x19, x4         // row col
    mov     x5, #8
    mul     x5, x3, x5
    add     x4, x4, x5          // matrix[col][j]
    ldr     d3, [x4]            // pivot_val

    // Get current row entry: matrix[row][j]
    mov     x4, #8
    mul     x4, x3, x4
    add     x4, x26, x4         // matrix[row][j]
    ldr     d4, [x4]            // curr_val

    // new_val = curr_val - factor * pivot_val
    fmul    d5, d2, d3          // factor * pivot_val
    fsub    d4, d4, d5          // new_val
    str     d4, [x4]            // store new value

    add     x3, x3, #1
    b       subtract_loop_fp

eliminate_next_fp:
    add     x25, x25, #1
    b       eliminate_loop_fp

gauss_next_col_fp:
    add     x21, x21, #1
    b       gauss_col_loop_fp

gauss_back_sub_fp:
    // Back substitution
    // For i from n-1 down to 0:
    //   solution[i] = (matrix[i][n] - sum(matrix[i][j]*solution[j] for j>i)) / matrix[i][i]

    sub     x21, x20, #1        // i = n-1

back_sub_loop_fp:
    cmp     x21, #0
    b.lt    gauss_done_fp

    // Get matrix[i][n] (RHS)
    mov     x0, #40
    mul     x0, x21, x0
    add     x22, x19, x0        // row i pointer
    ldr     d0, [x22, #32]      // rhs = matrix[i][4]

    // Subtract contributions from known variables
    add     x25, x21, #1        // j = i + 1

back_sum_loop_fp:
    cmp     x25, x20
    b.ge    back_divide_fp

    // Get matrix[i][j]
    mov     x0, #8
    mul     x0, x25, x0
    add     x0, x22, x0
    ldr     d1, [x0]            // coef

    // Get solution[j] from matrix[j][4]
    mov     x0, #40
    mul     x0, x25, x0
    add     x0, x19, x0
    ldr     d2, [x0, #32]       // solution[j]

    // rhs -= coef * solution[j]
    fmul    d3, d1, d2
    fsub    d0, d0, d3

    add     x25, x25, #1
    b       back_sum_loop_fp

back_divide_fp:
    // Get matrix[i][i] (pivot)
    mov     x0, #8
    mul     x0, x21, x0
    add     x0, x22, x0
    ldr     d1, [x0]            // pivot

    // solution = rhs / pivot
    fdiv    d0, d0, d1

    // Store back in matrix[i][4]
    str     d0, [x22, #32]

    sub     x21, x21, #1
    b       back_sub_loop_fp

gauss_done_fp:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

//=============================================================================
// Print string
// Input: x0 = null-terminated string
//=============================================================================
print_string:
    stp     x29, x30, [sp, #-16]!
    mov     x19, x0

    // Calculate length
    mov     x1, x0
1:  ldrb    w2, [x1]
    cbz     w2, 2f
    add     x1, x1, #1
    b       1b
2:  sub     x2, x1, x0          // Length

    mov     x16, #SYS_WRITE
    mov     x0, #1              // stdout
    mov     x1, x19
    svc     #0x80

    ldp     x29, x30, [sp], #16
    ret

//=============================================================================
// Print signed 64-bit number
// Input: x0 = number
//=============================================================================
print_number:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Handle negative
    cmp     x19, #0
    b.ge    print_positive

    // Print minus sign
    adrp    x0, num_buffer@PAGE
    add     x0, x0, num_buffer@PAGEOFF
    mov     w1, #'-'
    strb    w1, [x0]
    mov     x16, #SYS_WRITE
    mov     x0, #1
    adrp    x1, num_buffer@PAGE
    add     x1, x1, num_buffer@PAGEOFF
    mov     x2, #1
    svc     #0x80

    neg     x19, x19

print_positive:
    adrp    x20, num_buffer@PAGE
    add     x20, x20, num_buffer@PAGEOFF
    add     x20, x20, #31       // End of buffer
    mov     w1, #0
    strb    w1, [x20]           // Null terminator

    // Handle zero
    cbnz    x19, convert_loop
    sub     x20, x20, #1
    mov     w1, #'0'
    strb    w1, [x20]
    b       print_result

convert_loop:
    cbz     x19, print_result

    mov     x0, #10
    udiv    x1, x19, x0         // x1 = x19 / 10
    msub    x2, x1, x0, x19     // x2 = x19 % 10
    mov     x19, x1

    add     w2, w2, #'0'
    sub     x20, x20, #1
    strb    w2, [x20]
    b       convert_loop

print_result:
    mov     x0, x20
    bl      print_string

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
