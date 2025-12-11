// Advent of Code 2024 Day 7: Bridge Repair
// ARM64 Assembly for macOS
//
// Algorithm:
// - Parse equations: target and list of numbers
// - Part 1: Try all combinations of + and * operators (2^(n-1) combinations)
// - Part 2: Also try || (concatenation) operator (3^(n-1) combinations)
// - Operators evaluated left-to-right

.global _start
.align 4

// Constants
.equ SYS_OPEN, 0x2000005
.equ SYS_READ, 0x2000003
.equ SYS_CLOSE, 0x2000006
.equ SYS_WRITE, 0x2000004
.equ SYS_EXIT, 0x2000001
.equ O_RDONLY, 0
.equ STDOUT, 1

// Buffer sizes
.equ MAX_NUMS, 16          // Max numbers per equation
.equ MAX_EQUATIONS, 1024   // Max equations in input
.equ FILE_BUFFER_SIZE, 65536

.data
input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "\nPart 2: "
newline:        .asciz "\n"

.align 8
// Equation storage: [target (8 bytes), count (8 bytes), nums[MAX_NUMS] (8 bytes each)]
// Total size per equation: 8 + 8 + 16*8 = 144 bytes
equations:      .space 144 * MAX_EQUATIONS
equation_count: .quad 0

// File buffer
file_buffer:    .space FILE_BUFFER_SIZE

.text

// Utility: Count digits in a number
// Input: x0 = number
// Output: x0 = digit count
count_digits:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    mov x1, x0              // Save number
    mov x0, #0              // Digit count
    cbz x1, count_digits_zero

count_digits_loop:
    cbz x1, count_digits_done
    add x0, x0, #1
    mov x2, #10
    udiv x1, x1, x2
    b count_digits_loop

count_digits_zero:
    mov x0, #1

count_digits_done:
    ldp x29, x30, [sp], #16
    ret

// Utility: Concatenate two numbers (implements || operator)
// Input: x0 = left number, x1 = right number
// Output: x0 = concatenated result
concatenate:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    stp x19, x20, [sp, #16]

    mov x19, x0             // Save left
    mov x20, x1             // Save right

    // Count digits in right number
    mov x0, x20
    bl count_digits
    mov x2, x0              // x2 = digit count

    // Multiply left by 10^digit_count
    mov x0, x19
    mov x3, #1              // Multiplier

concat_power_loop:
    cbz x2, concat_power_done
    mov x4, #10
    mul x3, x3, x4
    sub x2, x2, #1
    b concat_power_loop

concat_power_done:
    mul x0, x0, x3
    add x0, x0, x20

    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

// Evaluate expression with given operator choices
// Input: x0 = pointer to nums array
//        x1 = count of numbers
//        x2 = operator choices (encoded as base-n number)
//        x3 = num_operators (base: 2 or 3)
// Output: x0 = result
evaluate:
    stp x29, x30, [sp, #-80]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]

    mov x19, x0             // nums pointer
    mov x20, x1             // count
    mov x21, x2             // operators
    mov x25, x3             // num_operators (base)

    // Load first number
    ldr x22, [x19]          // result
    mov x23, #1             // index

eval_loop:
    cmp x23, x20
    b.ge eval_done

    // Get operator: operators % base
    mov x0, x21
    udiv x1, x0, x25        // quotient
    msub x26, x1, x25, x0   // remainder = operators - (quotient * base)
    mov x21, x1             // Update operators for next iteration

    // Load next number
    lsl x1, x23, #3         // index * 8
    ldr x24, [x19, x1]      // next number

    // Apply operator
    cmp x26, #0
    b.eq eval_add
    cmp x26, #1
    b.eq eval_mul
    // Otherwise concatenate

eval_concat:
    mov x0, x22
    mov x1, x24
    bl concatenate
    mov x22, x0
    b eval_next

eval_add:
    add x22, x22, x24
    b eval_next

eval_mul:
    mul x22, x22, x24

eval_next:
    add x23, x23, #1
    b eval_loop

eval_done:
    mov x0, x22

    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #80
    ret

// Check if target can be made with operators
// Input: x0 = target
//        x1 = pointer to nums
//        x2 = count
//        x3 = num_operators (2 for part1, 3 for part2)
// Output: x0 = 1 if possible, 0 if not
can_make_target:
    stp x29, x30, [sp, #-80]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]

    mov x19, x0             // target
    mov x20, x1             // nums pointer
    mov x21, x2             // count
    mov x22, x3             // num_operators

    sub x23, x21, #1        // num_positions = count - 1

    // Calculate total combinations
    mov x24, #1             // total = 1
    mov x25, x23            // counter

calc_combinations:
    cbz x25, calc_done
    mul x24, x24, x22       // total *= num_operators
    sub x25, x25, #1
    b calc_combinations

calc_done:
    mov x25, #0             // current combination

try_combination:
    cmp x25, x24
    b.ge no_match

    // Evaluate with current operator combination
    mov x0, x20
    mov x1, x21
    mov x2, x25
    mov x3, x22             // Pass num_operators
    bl evaluate

    // Check if result matches target
    cmp x0, x19
    b.eq found_match

    add x25, x25, #1
    b try_combination

found_match:
    mov x0, #1
    b can_make_done

no_match:
    mov x0, #0

can_make_done:
    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #80
    ret

// Parse a number from string
// Input: x0 = pointer to string (will be updated)
//        x1 = pointer to result
// Output: x0 = updated pointer (after number and delimiter)
parse_number:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    stp x19, x20, [sp, #16]

    mov x19, x0             // String pointer
    mov x20, #0             // Result

parse_num_loop:
    ldrb w2, [x19]
    cmp w2, #'0'
    b.lt parse_num_done
    cmp w2, #'9'
    b.gt parse_num_done

    // result = result * 10 + (digit - '0')
    mov x3, #10
    mul x20, x20, x3
    sub w2, w2, #'0'
    add x20, x20, x2
    add x19, x19, #1
    b parse_num_loop

parse_num_done:
    str x20, [x1]

    // Skip delimiter (space, colon, newline)
parse_skip_delim:
    ldrb w2, [x19]
    cmp w2, #' '
    b.eq parse_skip_space
    cmp w2, #':'
    b.eq parse_skip_space
    cmp w2, #'\n'
    b.eq parse_num_end
    cbz w2, parse_num_end
    b parse_num_end

parse_skip_space:
    add x19, x19, #1
    b parse_skip_delim

parse_num_end:
    mov x0, x19
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

// Parse all equations from input buffer
// Input: x0 = buffer pointer
//        x1 = buffer size
parse_equations:
    stp x29, x30, [sp, #-80]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #64]

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // End pointer
    adrp x21, equations@PAGE
    add x21, x21, equations@PAGEOFF
    mov x22, #0             // Equation count

parse_eq_loop:
    cmp x19, x20
    b.ge parse_eq_done
    ldrb w0, [x19]
    cbz w0, parse_eq_done
    cmp w0, #'\n'
    b.eq parse_eq_skip_newline

    // Parse target
    mov x0, x19
    mov x1, x21             // Store in equation struct
    bl parse_number
    mov x19, x0

    // Count and parse numbers
    add x23, x21, #8        // Number count location
    add x24, x21, #16       // First number location
    mov x25, #0             // Number count (use callee-saved register!)

parse_nums_loop:
    ldrb w0, [x19]
    cmp w0, #'\n'
    b.eq parse_nums_done
    cbz w0, parse_nums_done

    // Parse number
    mov x0, x19
    mov x1, x24
    bl parse_number
    mov x19, x0

    add x25, x25, #1
    add x24, x24, #8
    b parse_nums_loop

parse_nums_done:
    str x25, [x23]          // Store count

    add x22, x22, #1        // Increment equation count
    add x21, x21, #144      // Next equation struct

parse_eq_skip_newline:
    ldrb w0, [x19]
    cbz w0, parse_eq_done
    cmp w0, #'\n'
    b.ne parse_eq_cont
    add x19, x19, #1

parse_eq_cont:
    b parse_eq_loop

parse_eq_done:
    adrp x0, equation_count@PAGE
    add x0, x0, equation_count@PAGEOFF
    str x22, [x0]

    ldp x25, x26, [sp, #64]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #80
    ret

// Solve part 1
// Output: x0 = sum of valid targets
solve_part1:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]

    mov x19, #0             // Sum
    mov x20, #0             // Index

    adrp x21, equation_count@PAGE
    add x21, x21, equation_count@PAGEOFF
    ldr x21, [x21]          // Total equations

    adrp x22, equations@PAGE
    add x22, x22, equations@PAGEOFF

part1_loop:
    cmp x20, x21
    b.ge part1_done

    // Get equation
    mov x23, #144
    mul x23, x20, x23
    add x23, x22, x23       // Equation pointer

    ldr x0, [x23]           // Target
    ldr x2, [x23, #8]       // Count
    add x1, x23, #16        // Nums pointer
    mov x3, #2              // 2 operators for part 1

    mov x24, x0             // Save target
    bl can_make_target

    cbz x0, part1_next
    add x19, x19, x24       // Add target to sum

part1_next:
    add x20, x20, #1
    b part1_loop

part1_done:
    mov x0, x19

    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// Solve part 2
// Output: x0 = sum of valid targets
solve_part2:
    stp x29, x30, [sp, #-64]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    stp x21, x22, [sp, #32]
    stp x23, x24, [sp, #48]

    mov x19, #0             // Sum
    mov x20, #0             // Index

    adrp x21, equation_count@PAGE
    add x21, x21, equation_count@PAGEOFF
    ldr x21, [x21]          // Total equations

    adrp x22, equations@PAGE
    add x22, x22, equations@PAGEOFF

part2_loop:
    cmp x20, x21
    b.ge part2_done

    // Get equation
    mov x23, #144
    mul x23, x20, x23
    add x23, x22, x23       // Equation pointer

    ldr x0, [x23]           // Target
    ldr x2, [x23, #8]       // Count
    add x1, x23, #16        // Nums pointer
    mov x3, #3              // 3 operators for part 2

    mov x24, x0             // Save target
    bl can_make_target

    cbz x0, part2_next
    add x19, x19, x24       // Add target to sum

part2_next:
    add x20, x20, #1
    b part2_loop

part2_done:
    mov x0, x19

    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #64
    ret

// Print a 64-bit number
// Input: x0 = number to print
print_number:
    stp x29, x30, [sp, #-48]!
    mov x29, sp
    stp x19, x20, [sp, #16]
    str x21, [sp, #32]

    mov x19, x0
    add x20, sp, #47        // Buffer on stack (end)
    strb wzr, [x20]         // Null terminator
    sub x20, x20, #1

    cbz x19, print_zero

print_digit_loop:
    cbz x19, print_digits_done
    mov x1, #10
    udiv x2, x19, x1        // quotient
    msub x3, x2, x1, x19    // remainder = x19 - (quotient * 10)
    add w3, w3, #'0'
    strb w3, [x20]
    sub x20, x20, #1
    mov x19, x2
    b print_digit_loop

print_zero:
    mov w0, #'0'
    strb w0, [x20]
    sub x20, x20, #1

print_digits_done:
    add x20, x20, #1        // Point to first digit

    // Calculate length
    add x2, sp, #47
    sub x2, x2, x20         // length

    // Write syscall
    movz x16, #0x2000, lsl #16
    movk x16, #0x0004
    mov x0, #STDOUT
    mov x1, x20
    svc #0

    ldr x21, [sp, #32]
    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #48
    ret

// Print string
// Input: x0 = string pointer
print_string:
    stp x29, x30, [sp, #-32]!
    mov x29, sp
    stp x19, x20, [sp, #16]

    mov x19, x0

    // Calculate length
    mov x20, x0
strlen_loop:
    ldrb w1, [x20]
    cbz w1, strlen_done
    add x20, x20, #1
    b strlen_loop

strlen_done:
    sub x2, x20, x19        // length

    // Write syscall
    movz x16, #0x2000, lsl #16
    movk x16, #0x0004
    mov x0, #STDOUT
    mov x1, x19
    svc #0

    ldp x19, x20, [sp, #16]
    ldp x29, x30, [sp], #32
    ret

_start:
    // Open file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0005
    adrp x0, input_path@PAGE
    add x0, x0, input_path@PAGEOFF
    mov x1, #O_RDONLY
    mov x2, #0
    svc #0

    mov x19, x0             // Save fd

    // Read file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0003
    mov x0, x19             // fd
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #FILE_BUFFER_SIZE
    svc #0

    mov x20, x0             // Save bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0

    // Parse equations
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl parse_equations

    // Print "Part 1: "
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_string

    // Solve part 1
    bl solve_part1
    bl print_number

    // Print "Part 2: "
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_string

    // Solve part 2
    bl solve_part2
    bl print_number

    // Print final newline
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    // Exit
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    mov x0, #0
    svc #0
