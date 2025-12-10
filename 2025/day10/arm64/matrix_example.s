// ============================================================================
// Matrix Row Operations Example using Rational Arithmetic Library
// ============================================================================
//
// This demonstrates how the rational arithmetic library can be integrated
// into matrix operations for Gaussian elimination.
//
// Example: Perform row operation R2 = R2 - (3/2) * R1
//
// Starting matrix:
// [ 2/1  4/1 | 8/1 ]     Row 1
// [ 3/1  7/1 | 15/1]     Row 2
//
// After operation R2 = R2 - (3/2) * R1:
// [ 2/1  4/1 | 8/1 ]     Row 1 (unchanged)
// [ 0/1  1/1 | 3/1 ]     Row 2 (modified)
//
// This is a key operation in Gaussian elimination for reducing a matrix
// to row echelon form.
// ============================================================================

.global _matrix_example_main
.align 4

// Macro to load address
.macro load_addr reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// ============================================================================
// External functions from rational.s
// ============================================================================
.extern rat_new
.extern rat_mul
.extern rat_sub
.extern rat_print
.extern print_string

// ============================================================================
// Data Section
// ============================================================================
.data
    .align 4

header:
    .asciz "=== Matrix Row Operations Example ===\n"
initial_matrix:
    .asciz "\nInitial Matrix:\n"
row1_label:
    .asciz "Row 1: [ "
row2_label:
    .asciz "Row 2: [ "
space:
    .asciz " "
pipe:
    .asciz " | "
close_bracket:
    .asciz " ]\n"
operation_label:
    .asciz "\nPerforming: R2 = R2 - (3/2) * R1\n"
final_matrix:
    .asciz "\nFinal Matrix:\n"

.bss
    .align 8

// Matrix storage: 2 rows x 3 cols (2 coefficients + 1 augmented)
// Each cell is a rational (16 bytes)
matrix:
    .space 2 * 3 * 16       // 2 rows * 3 cols * 16 bytes/rational = 96 bytes

// Temporary rationals for computation
factor:
    .space 16               // Factor (3/2)
temp1:
    .space 16               // Temporary result 1
temp2:
    .space 16               // Temporary result 2

// ============================================================================
// Main function
// ============================================================================
.text

_matrix_example_main:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Print header
    load_addr x0, header
    bl      print_string

    // Initialize matrix
    bl      init_matrix

    // Print initial matrix
    load_addr x0, initial_matrix
    bl      print_string
    bl      print_matrix

    // Perform row operation
    load_addr x0, operation_label
    bl      print_string
    bl      perform_row_operation

    // Print final matrix
    load_addr x0, final_matrix
    bl      print_string
    bl      print_matrix

    // Exit
    mov     x0, #0
    mov     w16, #1
    orr     w16, w16, #0x2000000
    svc     #0x80

// ============================================================================
// init_matrix: Initialize the example matrix
// ============================================================================
// Creates:
// [ 2/1  4/1 | 8/1 ]
// [ 3/1  7/1 | 15/1]
// ============================================================================
init_matrix:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    load_addr x19, matrix

    // Row 1, Col 1: 2/1
    mov     x0, #2
    mov     x1, #1
    mov     x2, x19
    bl      rat_new
    add     x19, x19, #16

    // Row 1, Col 2: 4/1
    mov     x0, #4
    mov     x1, #1
    mov     x2, x19
    bl      rat_new
    add     x19, x19, #16

    // Row 1, Col 3: 8/1
    mov     x0, #8
    mov     x1, #1
    mov     x2, x19
    bl      rat_new
    add     x19, x19, #16

    // Row 2, Col 1: 3/1
    mov     x0, #3
    mov     x1, #1
    mov     x2, x19
    bl      rat_new
    add     x19, x19, #16

    // Row 2, Col 2: 7/1
    mov     x0, #7
    mov     x1, #1
    mov     x2, x19
    bl      rat_new
    add     x19, x19, #16

    // Row 2, Col 3: 15/1
    mov     x0, #15
    mov     x1, #1
    mov     x2, x19
    bl      rat_new

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// ============================================================================
// print_matrix: Print the current matrix
// ============================================================================
print_matrix:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    load_addr x19, matrix

    // Print Row 1
    load_addr x0, row1_label
    bl      print_string

    mov     x0, x19
    bl      rat_print
    add     x19, x19, #16

    load_addr x0, space
    bl      print_string

    mov     x0, x19
    bl      rat_print
    add     x19, x19, #16

    load_addr x0, pipe
    bl      print_string

    mov     x0, x19
    bl      rat_print
    add     x19, x19, #16

    load_addr x0, close_bracket
    bl      print_string

    // Print Row 2
    load_addr x0, row2_label
    bl      print_string

    mov     x0, x19
    bl      rat_print
    add     x19, x19, #16

    load_addr x0, space
    bl      print_string

    mov     x0, x19
    bl      rat_print
    add     x19, x19, #16

    load_addr x0, pipe
    bl      print_string

    mov     x0, x19
    bl      rat_print

    load_addr x0, close_bracket
    bl      print_string

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// ============================================================================
// perform_row_operation: R2 = R2 - (3/2) * R1
// ============================================================================
// This is a typical Gaussian elimination row operation:
// 1. Compute factor = M[2,1] / M[1,1] = 3/2
// 2. For each column j:
//    M[2,j] = M[2,j] - factor * M[1,j]
// ============================================================================
perform_row_operation:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    str     x21, [sp, #32]

    // Create factor 3/2
    mov     x0, #3
    mov     x1, #2
    load_addr x2, factor
    bl      rat_new

    load_addr x19, matrix      // R1 start
    add     x20, x19, #48       // R2 start (3 cells * 16 bytes = 48)

    // Process each column
    mov     x21, #0             // Column counter

.Lcol_loop:
    cmp     x21, #3
    b.ge    .Lrow_op_done

    // Compute: temp1 = factor * R1[j]
    load_addr x0, factor
    mov     x1, x19             // R1[j]
    load_addr x2, temp1
    bl      rat_mul

    // Compute: temp2 = R2[j] - temp1
    mov     x0, x20             // R2[j]
    load_addr x1, temp1
    load_addr x2, temp2
    bl      rat_sub

    // Copy result back to R2[j]
    load_addr x0, temp2
    ldp     x1, x2, [x0]        // Load numerator and denominator
    stp     x1, x2, [x20]       // Store to R2[j]

    // Advance pointers
    add     x19, x19, #16       // Next R1 element
    add     x20, x20, #16       // Next R2 element
    add     x21, x21, #1
    b       .Lcol_loop

.Lrow_op_done:
    ldr     x21, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// ============================================================================
// Helper functions (stubs that call external functions)
// ============================================================================
print_string:
    // This would normally be in rational.s, but we'll implement locally
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, x0

    // Calculate length
    mov     x1, x0
.Lstrlen:
    ldrb    w2, [x1]
    cbz     w2, .Lstrlen_done
    add     x1, x1, #1
    b       .Lstrlen

.Lstrlen_done:
    sub     x2, x1, x19

    // Write
    mov     x0, #1              // stdout
    mov     x1, x19
    mov     w16, #4
    orr     w16, w16, #0x2000000
    svc     #0x80

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret
