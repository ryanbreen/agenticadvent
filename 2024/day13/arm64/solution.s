// Day 13: Claw Contraption - ARM64 Assembly (macOS)
// Solve systems of linear equations using Cramer's rule
//
// System of equations:
//   a*ax + b*bx = px
//   a*ay + b*by = py
//
// Solution (Cramer's rule):
//   det = ax*by - ay*bx
//   a = (px*by - py*bx) / det
//   b = (ax*py - ay*px) / det
//
// Part 1: Max 100 presses per button
// Part 2: Add 10^13 to prize coordinates, no press limit

.global _start
.align 4

// Constants
.equ MAX_INPUT_SIZE, 32768
.equ MAX_MACHINES, 512

// Macro for loading addresses
.macro LOAD_ADDR reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// ============================================================================
// Data Section
// ============================================================================
.data

input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"
error_msg:      .asciz "Error\n"

.align 3
file_buffer:    .space MAX_INPUT_SIZE

// Machine data: ax, ay, bx, by, px, py (6 values per machine)
machines:       .space MAX_MACHINES * 8 * 6
num_machines:   .quad 0

// ============================================================================
// Code Section
// ============================================================================
.text

_start:
    // Open and read input file
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

    // Parse input
    bl      parse_input

    // Solve Part 1
    bl      solve_part1
    mov     x21, x0                         // Save result

    // Solve Part 2
    bl      solve_part2
    mov     x22, x0                         // Save result

    // Print Part 1
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x21
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Print Part 2
    LOAD_ADDR x0, part2_msg
    bl      print_str
    mov     x0, x22
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Exit
    mov     x0, #0
    mov     x16, #1
    svc     #0x80

error_exit:
    LOAD_ADDR x0, error_msg
    bl      print_str
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// ============================================================================
// parse_input: Parse machine configurations
// ============================================================================
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x19, file_buffer              // Input pointer
    LOAD_ADDR x20, machines                 // Output array
    mov     x21, #0                         // Machine count

parse_loop:
    // Check if we've reached the end
    ldrb    w0, [x19]
    cbz     w0, parse_done

    // Skip "Button A: X+"
    bl      skip_to_digit

    // Parse ax
    bl      parse_number
    str     x0, [x20], #8                   // Store ax
    mov     x22, x0

    // Skip to next number (after ", Y+")
    bl      skip_to_digit

    // Parse ay
    bl      parse_number
    str     x0, [x20], #8                   // Store ay

    // Skip to "Button B: X+"
    bl      skip_to_digit

    // Parse bx
    bl      parse_number
    str     x0, [x20], #8                   // Store bx

    // Skip to ", Y+"
    bl      skip_to_digit

    // Parse by
    bl      parse_number
    str     x0, [x20], #8                   // Store by

    // Skip to "Prize: X="
    bl      skip_to_digit

    // Parse px
    bl      parse_number
    str     x0, [x20], #8                   // Store px

    // Skip to ", Y="
    bl      skip_to_digit

    // Parse py
    bl      parse_number
    str     x0, [x20], #8                   // Store py

    // Increment machine count
    add     x21, x21, #1

    // Skip any whitespace/newlines to next machine
    b       parse_loop

parse_done:
    LOAD_ADDR x0, num_machines
    str     x21, [x0]

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// skip_to_digit: Advance x19 to next digit
// ============================================================================
skip_to_digit:
skip_loop:
    ldrb    w0, [x19]
    cbz     w0, skip_done
    cmp     w0, #'0'
    b.lt    skip_next
    cmp     w0, #'9'
    b.le    skip_done
skip_next:
    add     x19, x19, #1
    b       skip_loop
skip_done:
    ret

// ============================================================================
// parse_number: Parse decimal number from x19
// Output: x0 = number, x19 advanced past number
// ============================================================================
parse_number:
    mov     x0, #0                          // Result
parse_num_loop:
    ldrb    w1, [x19]
    cmp     w1, #'0'
    b.lt    parse_num_done
    cmp     w1, #'9'
    b.gt    parse_num_done

    // result = result * 10 + (digit - '0')
    mov     x2, #10
    mul     x0, x0, x2
    sub     w1, w1, #'0'
    add     x0, x0, x1

    add     x19, x19, #1
    b       parse_num_loop
parse_num_done:
    ret

// ============================================================================
// solve_part1: Solve with max 100 presses
// Output: x0 = total tokens
// ============================================================================
solve_part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x19, machines
    LOAD_ADDR x20, num_machines
    ldr     x20, [x20]                      // Number of machines
    mov     x21, #0                         // Total cost

solve1_loop:
    cbz     x20, solve1_done

    // Load machine data
    ldp     x0, x1, [x19], #16              // ax, ay
    ldp     x2, x3, [x19], #16              // bx, by
    ldp     x4, x5, [x19], #16              // px, py

    // Save for next iteration
    sub     x20, x20, #1

    // Solve this machine
    mov     x6, #100                        // max_presses
    bl      solve_machine

    // Add to total if valid (x0 != -1)
    cmn     x0, #1
    b.eq    solve1_next
    add     x21, x21, x0

solve1_next:
    b       solve1_loop

solve1_done:
    mov     x0, x21

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part2: Solve with offset and no press limit
// Output: x0 = total tokens
// ============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x19, machines
    LOAD_ADDR x20, num_machines
    ldr     x20, [x20]
    mov     x21, #0                         // Total cost

    // Load offset: 10^13 = 10000000000000 = 0x9184e72a000
    movz    x23, #0xa000
    movk    x23, #0x4e72, lsl #16
    movk    x23, #0x0918, lsl #32

solve2_loop:
    cbz     x20, solve2_done

    // Load machine data
    ldp     x0, x1, [x19], #16              // ax, ay
    ldp     x2, x3, [x19], #16              // bx, by
    ldp     x4, x5, [x19], #16              // px, py

    // Add offset to prize coordinates
    add     x4, x4, x23                     // px += offset
    add     x5, x5, x23                     // py += offset

    sub     x20, x20, #1

    // Solve with no max presses
    mov     x6, #-1                         // No limit
    bl      solve_machine

    // Add to total if valid
    cmn     x0, #1
    b.eq    solve2_next
    add     x21, x21, x0

solve2_next:
    b       solve2_loop

solve2_done:
    mov     x0, x21

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_machine: Solve one machine using Cramer's rule
// Input: x0=ax, x1=ay, x2=bx, x3=by, x4=px, x5=py, x6=max_presses (-1 = no limit)
// Output: x0 = cost (3*a + b) or -1 if no solution
// ============================================================================
solve_machine:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0                         // ax
    mov     x20, x1                         // ay
    mov     x21, x2                         // bx
    mov     x22, x3                         // by
    mov     x23, x4                         // px
    mov     x24, x5                         // py
    mov     x25, x6                         // max_presses

    // Calculate determinant: det = ax*by - ay*bx
    // Use signed multiply for proper handling
    smull   x0, w19, w22                    // ax * by (extend to 64-bit)
    smull   x1, w20, w21                    // ay * bx (extend to 64-bit)
    sub     x26, x0, x1                     // det

    // Check if det == 0
    cbz     x26, solve_fail

    // Calculate a_num = px*by - py*bx
    mul     x0, x23, x22                    // px * by
    mul     x1, x24, x21                    // py * bx
    sub     x27, x0, x1                     // a_num

    // Check if a_num % det == 0
    sdiv    x0, x27, x26                    // a = a_num / det
    msub    x1, x0, x26, x27                // remainder = a_num - a*det
    cbnz    x1, solve_fail                  // Not integer solution

    mov     x10, x0                         // Save a

    // Calculate b_num = ax*py - ay*px
    mul     x0, x19, x24                    // ax * py
    mul     x1, x20, x23                    // ay * px
    sub     x27, x0, x1                     // b_num

    // Check if b_num % det == 0
    sdiv    x0, x27, x26                    // b = b_num / det
    msub    x1, x0, x26, x27                // remainder = b_num - b*det
    cbnz    x1, solve_fail

    mov     x11, x0                         // Save b

    // Check if a >= 0 and b >= 0
    cmp     x10, #0
    b.lt    solve_fail
    cmp     x11, #0
    b.lt    solve_fail

    // Check max_presses constraint if applicable
    cmn     x25, #1                         // Check if max_presses == -1
    b.eq    solve_calc_cost                 // No limit, skip check

    cmp     x10, x25
    b.gt    solve_fail
    cmp     x11, x25
    b.gt    solve_fail

solve_calc_cost:
    // Calculate cost = 3*a + b
    mov     x0, #3
    mul     x0, x0, x10
    add     x0, x0, x11

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

solve_fail:
    mov     x0, #-1

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print null-terminated string
// Input: x0 = string pointer
// ============================================================================
print_str:
    stp     x29, x30, [sp, #-16]!
    mov     x19, x0

    // Calculate string length
    mov     x1, x0
strlen_loop:
    ldrb    w2, [x1], #1
    cbnz    w2, strlen_loop
    sub     x2, x1, x0
    sub     x2, x2, #1

    // Write to stdout
    mov     x0, #1                          // stdout
    mov     x1, x19
    mov     x16, #4                         // write() syscall
    svc     #0x80

    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print unsigned 64-bit number
// Input: x0 = number to print
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32                     // Buffer for digits

    mov     x19, x0                         // Number to print
    mov     x20, sp                         // Buffer pointer
    add     x20, x20, #31                   // Point to end of buffer
    mov     w1, #0
    strb    w1, [x20]                       // Null terminator

    // Handle zero specially
    cbnz    x19, print_num_loop
    sub     x20, x20, #1
    mov     w1, #'0'
    strb    w1, [x20]
    b       print_num_done

print_num_loop:
    cbz     x19, print_num_done

    // Divide by 10
    mov     x1, #10
    udiv    x2, x19, x1                     // quotient
    mul     x3, x2, x1                      // quotient * 10
    sub     x1, x19, x3                     // remainder

    // Convert digit to ASCII and store
    add     w1, w1, #'0'
    sub     x20, x20, #1
    strb    w1, [x20]

    mov     x19, x2                         // quotient for next iteration
    b       print_num_loop

print_num_done:
    mov     x0, x20
    bl      print_str

    add     sp, sp, #32
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
