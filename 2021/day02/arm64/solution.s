// ARM64 Assembly solution for AoC 2021 Day 2 - Dive!
// macOS syscalls

.global _main
.align 2

.equ STDOUT, 1

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 16384
output_buffer: .skip 32

.text
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Open file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0005
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0              // O_RDONLY
    mov x2, #0
    svc #0x80
    cmp x0, #0
    b.lt exit_error
    mov x19, x0             // Save fd

    // Read file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0003
    mov x0, x19
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #16384
    svc #0x80
    mov x20, x0             // Save bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Process input for Part 1
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl part1
    mov x21, x0             // Save Part 1 result

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str
    mov x0, x21
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Process input for Part 2
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl part2
    mov x22, x0             // Save Part 2 result

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str
    mov x0, x22
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    mov x0, #0
    ldp x29, x30, [sp], #16
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    svc #0x80

exit_error:
    mov x0, #1
    ldp x29, x30, [sp], #16
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    svc #0x80

// Part 1: Track horizontal and depth
// forward X: horizontal += X
// down X: depth += X
// up X: depth -= X
// Return horizontal * depth
// x0 = buffer, x1 = length
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end
    mov x21, #0             // horizontal
    mov x22, #0             // depth

part1_loop:
    cmp x19, x20
    b.ge part1_done

    // Skip whitespace/newlines
    ldrb w23, [x19]
    cmp w23, #'\n'
    b.eq part1_skip
    cmp w23, #' '
    b.eq part1_skip
    cmp w23, #'\r'
    b.eq part1_skip
    cmp w23, #0
    b.eq part1_done

    // Identify command by first character
    // 'f' = forward, 'd' = down, 'u' = up
    cmp w23, #'f'
    b.eq part1_forward
    cmp w23, #'d'
    b.eq part1_down
    cmp w23, #'u'
    b.eq part1_up
    b part1_skip

part1_forward:
    // Skip to the number (after "forward ")
    add x19, x19, #8        // "forward " is 8 chars
    mov x0, x19
    bl parse_number
    mov x19, x0
    add x21, x21, x1        // horizontal += value
    b part1_loop

part1_down:
    // Skip to the number (after "down ")
    add x19, x19, #5        // "down " is 5 chars
    mov x0, x19
    bl parse_number
    mov x19, x0
    add x22, x22, x1        // depth += value
    b part1_loop

part1_up:
    // Skip to the number (after "up ")
    add x19, x19, #3        // "up " is 3 chars
    mov x0, x19
    bl parse_number
    mov x19, x0
    sub x22, x22, x1        // depth -= value
    b part1_loop

part1_skip:
    add x19, x19, #1
    b part1_loop

part1_done:
    mul x0, x21, x22        // Return horizontal * depth

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2: Track horizontal, depth, and aim
// down X: aim += X
// up X: aim -= X
// forward X: horizontal += X, depth += aim * X
// Return horizontal * depth
// x0 = buffer, x1 = length
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end
    mov x21, #0             // horizontal
    mov x22, #0             // depth
    mov x24, #0             // aim

part2_loop:
    cmp x19, x20
    b.ge part2_done

    // Skip whitespace/newlines
    ldrb w23, [x19]
    cmp w23, #'\n'
    b.eq part2_skip
    cmp w23, #' '
    b.eq part2_skip
    cmp w23, #'\r'
    b.eq part2_skip
    cmp w23, #0
    b.eq part2_done

    // Identify command by first character
    cmp w23, #'f'
    b.eq part2_forward
    cmp w23, #'d'
    b.eq part2_down
    cmp w23, #'u'
    b.eq part2_up
    b part2_skip

part2_forward:
    // Skip to the number (after "forward ")
    add x19, x19, #8
    mov x0, x19
    bl parse_number
    mov x19, x0
    add x21, x21, x1        // horizontal += value
    mul x25, x24, x1        // aim * value
    add x22, x22, x25       // depth += aim * value
    b part2_loop

part2_down:
    // Skip to the number (after "down ")
    add x19, x19, #5
    mov x0, x19
    bl parse_number
    mov x19, x0
    add x24, x24, x1        // aim += value
    b part2_loop

part2_up:
    // Skip to the number (after "up ")
    add x19, x19, #3
    mov x0, x19
    bl parse_number
    mov x19, x0
    sub x24, x24, x1        // aim -= value
    b part2_loop

part2_skip:
    add x19, x19, #1
    b part2_loop

part2_done:
    mul x0, x21, x22        // Return horizontal * depth

    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Parse a number from string
// x0 = pointer, returns x0 = new pointer, x1 = number
parse_number:
    mov x1, #0              // Result
    mov x2, #10             // Base
parse_num_loop:
    ldrb w3, [x0]
    cmp w3, #'0'
    b.lt parse_num_done
    cmp w3, #'9'
    b.gt parse_num_done
    sub w3, w3, #'0'
    mul x1, x1, x2
    add x1, x1, x3
    add x0, x0, #1
    b parse_num_loop
parse_num_done:
    ret

// Print string
// x0 = string pointer
print_str:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    mov x20, #0
ps_len_loop:
    ldrb w1, [x19, x20]
    cbz w1, ps_write
    add x20, x20, #1
    b ps_len_loop
ps_write:
    movz x16, #0x2000, lsl #16
    movk x16, #0x0004
    mov x0, #STDOUT
    mov x1, x19
    mov x2, x20
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print number
// x0 = number to print
print_number:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    adrp x19, output_buffer@PAGE
    add x19, x19, output_buffer@PAGEOFF
    add x19, x19, #31
    mov w1, #0
    strb w1, [x19]
    mov x20, x0
    mov x2, #10

pn_loop:
    udiv x3, x20, x2
    msub x4, x3, x2, x20
    add w4, w4, #'0'
    sub x19, x19, #1
    strb w4, [x19]
    mov x20, x3
    cbnz x20, pn_loop

    mov x0, x19
    bl print_str

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
