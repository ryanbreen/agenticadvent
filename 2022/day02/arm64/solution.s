// ARM64 Assembly solution for AoC 2022 Day 2 - Rock Paper Scissors
// macOS syscalls

.global _start
.align 2

.equ STDOUT, 1

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

// Part 1 lookup table: score for (opponent, me) pair
// Index: (opponent - 'A') * 3 + (me - 'X')
// Score = shape_score + outcome_score
// shape_score: X=1, Y=2, Z=3
// outcome: 0=loss, 3=draw, 6=win
.align 3
part1_table:
    .byte 4     // A,X: Rock vs Rock = draw (1+3)
    .byte 8     // A,Y: Rock vs Paper = win (2+6)
    .byte 3     // A,Z: Rock vs Scissors = loss (3+0)
    .byte 1     // B,X: Paper vs Rock = loss (1+0)
    .byte 5     // B,Y: Paper vs Paper = draw (2+3)
    .byte 9     // B,Z: Paper vs Scissors = win (3+6)
    .byte 7     // C,X: Scissors vs Rock = win (1+6)
    .byte 2     // C,Y: Scissors vs Paper = loss (2+0)
    .byte 6     // C,Z: Scissors vs Scissors = draw (3+3)

// Part 2 lookup table: score for (opponent, desired_outcome) pair
// Index: (opponent - 'A') * 3 + (outcome - 'X')
// X=lose, Y=draw, Z=win
// Score = shape_we_play + outcome_score
.align 3
part2_table:
    .byte 3     // A,X: vs Rock, need lose -> Scissors (3+0)
    .byte 4     // A,Y: vs Rock, need draw -> Rock (1+3)
    .byte 8     // A,Z: vs Rock, need win -> Paper (2+6)
    .byte 1     // B,X: vs Paper, need lose -> Rock (1+0)
    .byte 5     // B,Y: vs Paper, need draw -> Paper (2+3)
    .byte 9     // B,Z: vs Paper, need win -> Scissors (3+6)
    .byte 2     // C,X: vs Scissors, need lose -> Paper (2+0)
    .byte 6     // C,Y: vs Scissors, need draw -> Scissors (3+3)
    .byte 7     // C,Z: vs Scissors, need win -> Rock (1+6)

.align 3
file_buffer: .skip 16384
output_buffer: .skip 32

.text
_start:
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

    // Parse and compute both parts
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl solve
    mov x21, x0             // Part 1 result
    mov x22, x1             // Part 2 result

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str
    mov x0, x21
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

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

// Solve both parts in single pass
// x0 = buffer, x1 = length
// Returns x0 = part1 total, x1 = part2 total
solve:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end
    mov x21, #0             // Part 1 total
    mov x22, #0             // Part 2 total

    // Load table addresses
    adrp x23, part1_table@PAGE
    add x23, x23, part1_table@PAGEOFF
    adrp x24, part2_table@PAGE
    add x24, x24, part2_table@PAGEOFF

solve_loop:
    cmp x19, x20
    b.ge solve_done

    // Read opponent char (A, B, or C)
    ldrb w25, [x19]

    // Skip if not A/B/C (handle trailing newlines)
    cmp w25, #'A'
    b.lt skip_line
    cmp w25, #'C'
    b.gt skip_line

    // Read my char (X, Y, or Z) - at position +2
    ldrb w26, [x19, #2]

    // Calculate table index: (opponent - 'A') * 3 + (me - 'X')
    sub w25, w25, #'A'      // opponent index: 0, 1, or 2
    sub w26, w26, #'X'      // my index: 0, 1, or 2

    // Calculate combined index
    mov w0, #3
    mul w25, w25, w0        // opponent * 3
    add w25, w25, w26       // + my index

    // Look up Part 1 score
    ldrb w0, [x23, x25]
    add x21, x21, x0        // Add to Part 1 total

    // Look up Part 2 score
    ldrb w0, [x24, x25]
    add x22, x22, x0        // Add to Part 2 total

skip_line:
    // Skip to next line
    add x19, x19, #1
skip_to_nl:
    cmp x19, x20
    b.ge solve_done
    ldrb w0, [x19]
    add x19, x19, #1
    cmp w0, #'\n'
    b.ne skip_to_nl
    b solve_loop

solve_done:
    mov x0, x21             // Return Part 1
    mov x1, x22             // Return Part 2

    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
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
