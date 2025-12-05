//==============================================================================
// Advent of Code 2024 Day 3: Mull It Over
// ARM64 Assembly for macOS (Apple Silicon)
//
// Part 1: Parse corrupted memory for valid mul(X,Y) instructions where X and Y
//         are 1-3 digit numbers. Sum all products.
//
// Part 2: Handle do() and don't() instructions that enable/disable mul().
//         mul() instructions start enabled.
//
// Compile: as -o solution.o solution.s && \
//          ld -o solution solution.o -lSystem \
//          -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64
//
// Run: ./solution (reads ../input.txt)
//==============================================================================

.global _start
.align 4

// macOS system calls
.equ SYS_READ, 0x2000003
.equ SYS_WRITE, 0x2000004
.equ SYS_OPEN, 0x2000005
.equ SYS_CLOSE, 0x2000006
.equ SYS_EXIT, 0x2000001

// File flags
.equ O_RDONLY, 0x0000

// File descriptor constants
.equ STDIN, 0
.equ STDOUT, 1
.equ STDERR, 2

.data
input_path:     .asciz "../input.txt"
part1_label:    .asciz "Part 1: "
part2_label:    .asciz "\nPart 2: "
newline:        .asciz "\n"

.bss
.align 4
buffer:         .skip 32768          // 32KB buffer for input file
output_buf:     .skip 32             // Buffer for number output

.text

// Main entry point
_start:
    // Open input file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0005
    adrp x0, input_path@PAGE
    add x0, x0, input_path@PAGEOFF
    mov x1, #O_RDONLY
    mov x2, #0
    svc #0x80

    cmp x0, #0
    b.lt exit_error
    mov x19, x0                      // Save file descriptor in x19

    // Read entire file into buffer
    movz x16, #0x2000, lsl #16
    movk x16, #0x0003
    mov x0, x19                      // File descriptor
    adrp x1, buffer@PAGE
    add x1, x1, buffer@PAGEOFF
    mov x2, #32768                   // Max bytes to read
    svc #0x80

    cmp x0, #0
    b.lt exit_error
    mov x20, x0                      // Save file size in x20

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // ===== PART 1 =====
    adrp x0, part1_label@PAGE
    add x0, x0, part1_label@PAGEOFF
    bl print_string

    adrp x0, buffer@PAGE
    add x0, x0, buffer@PAGEOFF
    mov x1, x20                      // Buffer size
    bl part1_solve
    mov x21, x0                      // Save part1 result

    bl print_number

    // ===== PART 2 =====
    adrp x0, part2_label@PAGE
    add x0, x0, part2_label@PAGEOFF
    bl print_string

    adrp x0, buffer@PAGE
    add x0, x0, buffer@PAGEOFF
    mov x1, x20                      // Buffer size
    bl part2_solve
    mov x22, x0                      // Save part2 result

    bl print_number

    // Print final newline
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    // Exit success
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    mov x0, #0
    svc #0x80

exit_error:
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    mov x0, #1
    svc #0x80

//==============================================================================
// Part 1: Find all mul(X,Y) and sum products
// Input: x0 = buffer pointer, x1 = buffer size
// Output: x0 = sum
//==============================================================================
part1_solve:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0                      // Buffer pointer
    mov x20, x1                      // Remaining bytes
    mov x21, #0                      // Sum accumulator

.part1_loop:
    cbz x20, .part1_done             // No more bytes

    // Look for 'mul(' pattern
    ldrb w0, [x19]
    cmp w0, #'m'
    b.ne .part1_next

    cmp x20, #8                      // Need at least "mul(X,Y)"
    b.lt .part1_done

    ldrb w0, [x19, #1]
    cmp w0, #'u'
    b.ne .part1_next

    ldrb w0, [x19, #2]
    cmp w0, #'l'
    b.ne .part1_next

    ldrb w0, [x19, #3]
    cmp w0, #'('
    b.ne .part1_next

    // Found "mul(" - parse first number
    add x0, x19, #4                  // Start after "mul("
    mov x1, x20
    sub x1, x1, #4
    bl parse_number

    cmp x0, #-1                      // Check if valid
    b.eq .part1_next
    cmp x0, #999                     // Max 3 digits (999)
    b.gt .part1_next

    mov x22, x0                      // Save first number
    mov x23, x1                      // Save position after number

    // Check for comma
    ldrb w0, [x23]
    cmp w0, #','
    b.ne .part1_next

    // Parse second number
    add x0, x23, #1                  // Start after comma
    mov x1, x20
    sub x1, x1, x23
    add x1, x1, x19
    bl parse_number

    cmp x0, #-1                      // Check if valid
    b.eq .part1_next
    cmp x0, #999                     // Max 3 digits
    b.gt .part1_next

    mov x24, x0                      // Save second number
    mov x23, x1                      // Save position after number

    // Check for closing paren
    ldrb w0, [x23]
    cmp w0, #')'
    b.ne .part1_next

    // Valid mul instruction - multiply and add to sum
    mul x0, x22, x24
    add x21, x21, x0

.part1_next:
    add x19, x19, #1
    sub x20, x20, #1
    b .part1_loop

.part1_done:
    mov x0, x21                      // Return sum

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

//==============================================================================
// Part 2: Handle do() and don't() toggles
// Input: x0 = buffer pointer, x1 = buffer size
// Output: x0 = sum
//==============================================================================
part2_solve:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0                      // Buffer pointer
    mov x20, x1                      // Remaining bytes
    mov x21, #0                      // Sum accumulator
    mov x25, #1                      // Enabled flag (1=enabled, 0=disabled)

.part2_loop:
    cbz x20, .part2_done             // No more bytes

    ldrb w0, [x19]

    // Check for 'do()' or 'don't()'
    cmp w0, #'d'
    b.ne .part2_check_mul

    cmp x20, #4                      // Need at least "do()"
    b.lt .part2_check_mul

    ldrb w0, [x19, #1]
    cmp w0, #'o'
    b.ne .part2_check_mul

    ldrb w0, [x19, #2]

    // Check for "do()"
    cmp w0, #'('
    b.ne .part2_check_dont

    cmp x20, #4
    b.lt .part2_check_mul

    ldrb w0, [x19, #3]
    cmp w0, #')'
    b.ne .part2_check_mul

    // Found "do()" - enable
    mov x25, #1
    add x19, x19, #4
    sub x20, x20, #4
    b .part2_loop

.part2_check_dont:
    // Check for "don't()"
    cmp w0, #'n'
    b.ne .part2_check_mul

    cmp x20, #7                      // Need "don't()"
    b.lt .part2_check_mul

    ldrb w0, [x19, #3]
    cmp w0, #'\''
    b.ne .part2_check_mul

    ldrb w0, [x19, #4]
    cmp w0, #'t'
    b.ne .part2_check_mul

    ldrb w0, [x19, #5]
    cmp w0, #'('
    b.ne .part2_check_mul

    ldrb w0, [x19, #6]
    cmp w0, #')'
    b.ne .part2_check_mul

    // Found "don't()" - disable
    mov x25, #0
    add x19, x19, #7
    sub x20, x20, #7
    b .part2_loop

.part2_check_mul:
    // Look for 'mul(' pattern
    ldrb w0, [x19]
    cmp w0, #'m'
    b.ne .part2_next

    cmp x20, #8                      // Need at least "mul(X,Y)"
    b.lt .part2_done

    ldrb w0, [x19, #1]
    cmp w0, #'u'
    b.ne .part2_next

    ldrb w0, [x19, #2]
    cmp w0, #'l'
    b.ne .part2_next

    ldrb w0, [x19, #3]
    cmp w0, #'('
    b.ne .part2_next

    // Found "mul(" - parse first number
    add x0, x19, #4                  // Start after "mul("
    mov x1, x20
    sub x1, x1, #4
    bl parse_number

    cmp x0, #-1                      // Check if valid
    b.eq .part2_next
    cmp x0, #999                     // Max 3 digits
    b.gt .part2_next

    mov x22, x0                      // Save first number
    mov x23, x1                      // Save position after number

    // Check for comma
    ldrb w0, [x23]
    cmp w0, #','
    b.ne .part2_next

    // Parse second number
    add x0, x23, #1                  // Start after comma
    mov x1, x20
    sub x1, x1, x23
    add x1, x1, x19
    bl parse_number

    cmp x0, #-1                      // Check if valid
    b.eq .part2_next
    cmp x0, #999                     // Max 3 digits
    b.gt .part2_next

    mov x24, x0                      // Save second number
    mov x23, x1                      // Save position after number

    // Check for closing paren
    ldrb w0, [x23]
    cmp w0, #')'
    b.ne .part2_next

    // Valid mul instruction - check if enabled
    cbz x25, .part2_next             // Skip if disabled

    // Multiply and add to sum
    mul x0, x22, x24
    add x21, x21, x0

.part2_next:
    add x19, x19, #1
    sub x20, x20, #1
    b .part2_loop

.part2_done:
    mov x0, x21                      // Return sum

    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

//==============================================================================
// Parse a decimal number from string (1-3 digits)
// Input: x0 = string pointer, x1 = max bytes
// Output: x0 = number (-1 if invalid), x1 = pointer after number
//==============================================================================
parse_number:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x0                      // String pointer
    mov x20, #0                      // Accumulator
    mov x2, #0                       // Digit count

.parse_loop:
    cbz x1, .parse_check_valid       // No more bytes

    ldrb w3, [x19]                   // Load character

    // Check if digit
    cmp w3, #'0'
    b.lt .parse_check_valid
    cmp w3, #'9'
    b.gt .parse_check_valid

    // It's a digit - convert and accumulate
    sub w3, w3, #'0'
    mov x4, #10
    mul x20, x20, x4
    add x20, x20, x3

    add x2, x2, #1                   // Increment digit count
    cmp x2, #3                       // Max 3 digits
    b.gt .parse_invalid

    add x19, x19, #1                 // Next character
    sub x1, x1, #1
    b .parse_loop

.parse_check_valid:
    cbz x2, .parse_invalid           // No digits found
    mov x0, x20                      // Return number
    mov x1, x19                      // Return position

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.parse_invalid:
    mov x0, #-1                      // Return -1 for invalid
    mov x1, x19

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

//==============================================================================
// Print a null-terminated string
// Input: x0 = string pointer
//==============================================================================
print_string:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x0                      // Save string pointer

    // Calculate string length
    mov x20, #0
.strlen_loop:
    ldrb w1, [x19, x20]
    cbz w1, .strlen_done
    add x20, x20, #1
    b .strlen_loop

.strlen_done:
    // Write to stdout
    movz x16, #0x2000, lsl #16
    movk x16, #0x0004
    mov x0, #STDOUT
    mov x1, x19
    mov x2, x20
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

//==============================================================================
// Print a number in decimal
// Input: x0 = number to print
//==============================================================================
print_number:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0                      // Number to print
    adrp x20, output_buf@PAGE
    add x20, x20, output_buf@PAGEOFF
    add x20, x20, #31                // Point to end of buffer
    mov w21, #0
    strb w21, [x20]                  // Null terminator

    mov x22, #10                     // Divisor

    // Handle zero special case
    cbnz x19, .print_convert_loop
    sub x20, x20, #1
    mov w21, #'0'
    strb w21, [x20]
    b .print_output

.print_convert_loop:
    cbz x19, .print_output

    udiv x1, x19, x22                // Divide by 10
    msub x2, x1, x22, x19            // Get remainder (digit)

    add w2, w2, #'0'                 // Convert to ASCII
    sub x20, x20, #1
    strb w2, [x20]

    mov x19, x1                      // Continue with quotient
    b .print_convert_loop

.print_output:
    mov x0, x20
    bl print_string

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
