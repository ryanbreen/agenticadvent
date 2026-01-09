// Day 21: Monkey Math - ARM64 Assembly (macOS)
//
// Part 1: Evaluate expression tree starting from 'root' monkey
// Part 2: Find what 'humn' value makes root's two children equal (symbolic equation solving)
//
// Algorithm:
// 1. Parse monkeys - each has 4-char name, either number or (op, left, right)
// 2. Part 1: Recursive evaluation from 'root'
// 3. Part 2: Find path to 'humn', then backtrack solving for humn value

.global _main
.align 4

// Constants
.equ MAX_MONKEYS, 2048
.equ MAX_INPUT, 65536
.equ NAME_SIZE, 4
.equ MONKEY_SIZE, 32         // 4 name + 4 pad + 8 value + 4 op + 4 left_idx + 4 right_idx + 4 flags

// Monkey struct offsets
.equ M_NAME, 0               // 4 bytes - name as 32-bit int
.equ M_VALUE, 8              // 8 bytes - value (if number or computed)
.equ M_OP, 16                // 4 bytes - operator char ('+', '-', '*', '/') or 0 if number
.equ M_LEFT, 20              // 4 bytes - index of left monkey
.equ M_RIGHT, 24             // 4 bytes - index of right monkey
.equ M_FLAGS, 28             // 4 bytes - flags (bit 0: has_humn in subtree)

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

.align 3
num_monkeys:    .quad 0
root_index:     .quad 0
humn_index:     .quad 0

// Monkey array - each monkey is 32 bytes
.align 3
monkeys:        .space MAX_MONKEYS * MONKEY_SIZE

// File buffer
file_buffer:    .space MAX_INPUT

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_main:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Open input file
    LOAD_ADDR x0, input_path
    mov     x1, #0
    mov     x2, #0
    mov     x16, #5
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit
    mov     x19, x0

    // Read file
    mov     x0, x19
    LOAD_ADDR x1, file_buffer
    mov     x2, #MAX_INPUT
    mov     x16, #3
    svc     #0x80

    // Close file
    mov     x0, x19
    mov     x16, #6
    svc     #0x80

    // Parse input
    bl      parse_input

    // Resolve name references to indices
    bl      resolve_names

    // Part 1: Evaluate root
    LOAD_ADDR x0, root_index
    ldr     x0, [x0]
    bl      evaluate
    mov     x21, x0

    // Part 2: Solve for humn
    bl      solve_part2
    mov     x22, x0

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
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// ============================================================================
// parse_input: Parse monkey definitions
// Format: "name: number" or "name: left op right"
// ============================================================================
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    LOAD_ADDR x19, file_buffer      // Input pointer
    LOAD_ADDR x20, monkeys          // Monkey array base
    mov     x21, #0                  // Monkey count

parse_line:
    ldrb    w0, [x19]
    cbz     w0, parse_done
    cmp     w0, #'\n'
    b.eq    skip_empty

    // Read 4-char name as 32-bit int
    ldr     w22, [x19]               // Load 4 bytes (name)
    add     x19, x19, #4             // Skip name

    // Check for special names
    // "root" = 0x746f6f72 in little-endian
    movz    w0, #0x6f72              // 'ro'
    movk    w0, #0x746f, lsl #16     // 'ot'
    cmp     w22, w0
    b.ne    check_humn
    LOAD_ADDR x0, root_index
    str     x21, [x0]
    b       store_name

check_humn:
    // "humn" = 0x6e6d7568 in little-endian
    movz    w0, #0x7568              // 'hu'
    movk    w0, #0x6e6d, lsl #16     // 'mn'
    cmp     w22, w0
    b.ne    store_name
    LOAD_ADDR x0, humn_index
    str     x21, [x0]

store_name:
    // Calculate monkey struct address
    mov     x23, #MONKEY_SIZE
    mul     x23, x21, x23
    add     x23, x20, x23            // x23 = &monkeys[count]

    // Store name
    str     w22, [x23, #M_NAME]

    // Skip ": "
    add     x19, x19, #2

    // Check if next is digit (number) or letter (expression)
    ldrb    w0, [x19]
    cmp     w0, #'0'
    b.lt    parse_expression
    cmp     w0, #'9'
    b.gt    parse_expression

    // Parse number
    mov     x24, #0                  // Value accumulator
parse_number:
    ldrb    w0, [x19]
    cmp     w0, #'0'
    b.lt    store_number
    cmp     w0, #'9'
    b.gt    store_number
    sub     w0, w0, #'0'
    mov     x1, #10
    mul     x24, x24, x1
    add     x24, x24, x0
    add     x19, x19, #1
    b       parse_number

store_number:
    str     x24, [x23, #M_VALUE]
    str     wzr, [x23, #M_OP]        // Op = 0 means it's a number
    b       next_line

parse_expression:
    // Read left operand name (4 chars)
    ldr     w24, [x19]               // Left name
    add     x19, x19, #4

    // Skip space
    add     x19, x19, #1

    // Read operator
    ldrb    w25, [x19]               // Operator
    add     x19, x19, #1

    // Skip space
    add     x19, x19, #1

    // Read right operand name (4 chars)
    ldr     w26, [x19]               // Right name
    add     x19, x19, #4

    // Store expression
    str     w25, [x23, #M_OP]        // Operator
    str     w24, [x23, #M_LEFT]      // Left name (to be resolved later)
    str     w26, [x23, #M_RIGHT]     // Right name (to be resolved later)

next_line:
    // Skip to next line
skip_to_newline:
    ldrb    w0, [x19]
    cbz     w0, next_monkey
    cmp     w0, #'\n'
    b.eq    found_newline
    add     x19, x19, #1
    b       skip_to_newline

found_newline:
    add     x19, x19, #1

next_monkey:
    add     x21, x21, #1
    b       parse_line

skip_empty:
    add     x19, x19, #1
    b       parse_line

parse_done:
    LOAD_ADDR x0, num_monkeys
    str     x21, [x0]

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// resolve_names: Convert name references to indices
// ============================================================================
resolve_names:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x0, num_monkeys
    ldr     x19, [x0]
    LOAD_ADDR x20, monkeys

    mov     x21, #0                  // Current monkey index

resolve_loop:
    cmp     x21, x19
    b.ge    resolve_done

    // Get monkey struct address
    mov     x0, #MONKEY_SIZE
    mul     x0, x21, x0
    add     x22, x20, x0             // x22 = &monkeys[i]

    // Check if it's an expression
    ldr     w0, [x22, #M_OP]
    cbz     w0, next_resolve         // Skip if it's a number

    // Resolve left name
    ldr     w23, [x22, #M_LEFT]      // Left name
    mov     x0, x23
    bl      find_monkey
    str     w0, [x22, #M_LEFT]       // Store index

    // Resolve right name
    ldr     w23, [x22, #M_RIGHT]     // Right name
    mov     x0, x23
    bl      find_monkey
    str     w0, [x22, #M_RIGHT]      // Store index

next_resolve:
    add     x21, x21, #1
    b       resolve_loop

resolve_done:
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// find_monkey: Find monkey index by name
// x0 = name (32-bit), returns x0 = index
// ============================================================================
find_monkey:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     w19, w0                  // Name to find
    LOAD_ADDR x0, num_monkeys
    ldr     x20, [x0]
    LOAD_ADDR x21, monkeys
    mov     x22, #0                  // Index

find_loop:
    cmp     x22, x20
    b.ge    find_not_found

    mov     x0, #MONKEY_SIZE
    mul     x0, x22, x0
    add     x0, x21, x0
    ldr     w1, [x0, #M_NAME]
    cmp     w1, w19
    b.eq    find_found

    add     x22, x22, #1
    b       find_loop

find_found:
    mov     x0, x22
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

find_not_found:
    mov     x0, #-1
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// evaluate: Recursively evaluate a monkey's value
// x0 = monkey index, returns x0 = value
// ============================================================================
evaluate:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    mov     x19, x0                  // Monkey index

    // Get monkey struct address
    LOAD_ADDR x20, monkeys
    mov     x0, #MONKEY_SIZE
    mul     x0, x19, x0
    add     x20, x20, x0             // x20 = &monkeys[index]

    // Check if it's a number
    ldr     w21, [x20, #M_OP]
    cbz     w21, return_value

    // It's an expression - evaluate children
    ldr     w0, [x20, #M_LEFT]
    bl      evaluate
    mov     x22, x0                  // Left value

    ldr     w0, [x20, #M_RIGHT]
    bl      evaluate
    mov     x23, x0                  // Right value

    // Apply operator
    cmp     w21, #'+'
    b.eq    do_add
    cmp     w21, #'-'
    b.eq    do_sub
    cmp     w21, #'*'
    b.eq    do_mul
    cmp     w21, #'/'
    b.eq    do_div
    b       eval_done

do_add:
    add     x0, x22, x23
    b       eval_done

do_sub:
    sub     x0, x22, x23
    b       eval_done

do_mul:
    mul     x0, x22, x23
    b       eval_done

do_div:
    sdiv    x0, x22, x23
    b       eval_done

return_value:
    ldr     x0, [x20, #M_VALUE]

eval_done:
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// contains_humn: Check if subtree contains 'humn'
// x0 = monkey index, returns x0 = 1 if contains humn, 0 otherwise
// ============================================================================
contains_humn:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0                  // Monkey index

    // Check if this is humn
    LOAD_ADDR x0, humn_index
    ldr     x0, [x0]
    cmp     x19, x0
    b.eq    is_humn

    // Get monkey struct address
    LOAD_ADDR x20, monkeys
    mov     x0, #MONKEY_SIZE
    mul     x0, x19, x0
    add     x20, x20, x0             // x20 = &monkeys[index]

    // Check if it's a number (leaf)
    ldr     w21, [x20, #M_OP]
    cbz     w21, not_humn

    // Check left subtree
    ldr     w0, [x20, #M_LEFT]
    bl      contains_humn
    cbnz    x0, is_humn

    // Check right subtree
    ldr     w0, [x20, #M_RIGHT]
    bl      contains_humn
    cbnz    x0, is_humn

not_humn:
    mov     x0, #0
    b       contains_done

is_humn:
    mov     x0, #1

contains_done:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_for_humn: Given that monkey at index should equal target, find humn value
// x0 = monkey index, x1 = target value, returns x0 = humn value
// ============================================================================
solve_for_humn:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0                  // Monkey index
    mov     x20, x1                  // Target value

    // Check if this is humn
    LOAD_ADDR x0, humn_index
    ldr     x0, [x0]
    cmp     x19, x0
    b.eq    return_target

    // Get monkey struct address
    LOAD_ADDR x21, monkeys
    mov     x0, #MONKEY_SIZE
    mul     x0, x19, x0
    add     x21, x21, x0             // x21 = &monkeys[index]

    // Get operator
    ldr     w22, [x21, #M_OP]

    // Get left and right indices
    ldr     w23, [x21, #M_LEFT]      // Left index
    ldr     w24, [x21, #M_RIGHT]     // Right index

    // Check which side contains humn
    mov     x0, x23
    bl      contains_humn
    mov     x25, x0                  // x25 = left_has_humn

    cbnz    x25, left_has_humn_case

    // Right side has humn - evaluate left side
    mov     x0, x23
    bl      evaluate
    mov     x26, x0                  // x26 = left_val

    // Calculate new target based on operator
    // right = solve(target, left_val, op)
    cmp     w22, #'+'
    b.eq    right_add
    cmp     w22, #'-'
    b.eq    right_sub
    cmp     w22, #'*'
    b.eq    right_mul
    cmp     w22, #'/'
    b.eq    right_div
    b       solve_done

right_add:
    // left + right = target => right = target - left
    sub     x1, x20, x26
    b       recurse_right

right_sub:
    // left - right = target => right = left - target
    sub     x1, x26, x20
    b       recurse_right

right_mul:
    // left * right = target => right = target / left
    sdiv    x1, x20, x26
    b       recurse_right

right_div:
    // left / right = target => right = left / target
    sdiv    x1, x26, x20
    b       recurse_right

recurse_right:
    mov     x0, x24
    bl      solve_for_humn
    b       solve_done

left_has_humn_case:
    // Left side has humn - evaluate right side
    mov     x0, x24
    bl      evaluate
    mov     x27, x0                  // x27 = right_val

    // Calculate new target based on operator
    // left = solve(target, right_val, op)
    cmp     w22, #'+'
    b.eq    left_add
    cmp     w22, #'-'
    b.eq    left_sub
    cmp     w22, #'*'
    b.eq    left_mul
    cmp     w22, #'/'
    b.eq    left_div
    b       solve_done

left_add:
    // left + right = target => left = target - right
    sub     x1, x20, x27
    b       recurse_left

left_sub:
    // left - right = target => left = target + right
    add     x1, x20, x27
    b       recurse_left

left_mul:
    // left * right = target => left = target / right
    sdiv    x1, x20, x27
    b       recurse_left

left_div:
    // left / right = target => left = target * right
    mul     x1, x20, x27
    b       recurse_left

recurse_left:
    mov     x0, x23
    bl      solve_for_humn
    b       solve_done

return_target:
    mov     x0, x20

solve_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part2: Find what humn needs to yell for root's children to be equal
// ============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    // Get root's children
    LOAD_ADDR x0, root_index
    ldr     x19, [x0]

    LOAD_ADDR x20, monkeys
    mov     x0, #MONKEY_SIZE
    mul     x0, x19, x0
    add     x20, x20, x0             // x20 = &monkeys[root]

    ldr     w21, [x20, #M_LEFT]      // Left child index
    ldr     w22, [x20, #M_RIGHT]     // Right child index

    // Check which side contains humn
    mov     x0, x21
    bl      contains_humn
    mov     x23, x0                  // x23 = left_has_humn

    cbnz    x23, left_has_humn_p2

    // Right side has humn - evaluate left side for target
    mov     x0, x21
    bl      evaluate
    mov     x1, x0                   // target = left value
    mov     x0, x22                  // solve for right side
    bl      solve_for_humn
    b       part2_done

left_has_humn_p2:
    // Left side has humn - evaluate right side for target
    mov     x0, x22
    bl      evaluate
    mov     x1, x0                   // target = right value
    mov     x0, x21                  // solve for left side
    bl      solve_for_humn

part2_done:
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print null-terminated string
// ============================================================================
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    mov     x20, #0
1:  ldrb    w1, [x19, x20]
    cbz     w1, 2f
    add     x20, x20, #1
    b       1b

2:  mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print signed 64-bit number
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

    // Handle negative
    cmp     x19, #0
    b.ge    positive
    neg     x19, x19
    mov     x22, #1                  // Flag for negative
    b       convert_digits

positive:
    mov     x22, #0

convert_digits:
    cbnz    x19, 1f
    sub     x20, x20, #1
    mov     w0, #'0'
    strb    w0, [x20]
    b       check_negative

1:  cbz     x19, check_negative
    mov     x1, #10
    udiv    x21, x19, x1
    msub    x0, x21, x1, x19
    add     w0, w0, #'0'
    sub     x20, x20, #1
    strb    w0, [x20]
    mov     x19, x21
    b       1b

check_negative:
    cbz     x22, 2f
    sub     x20, x20, #1
    mov     w0, #'-'
    strb    w0, [x20]

2:  mov     x0, x20
    bl      print_str

    add     sp, sp, #32
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
