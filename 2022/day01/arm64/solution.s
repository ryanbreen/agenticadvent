// ARM64 Assembly solution for AoC 2022 Day 1 - Calorie Counting
// macOS syscalls

.global _start
.align 2

.equ STDOUT, 1
.equ MAX_ELVES, 300

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 32768
elf_totals: .skip MAX_ELVES * 8    // Array of elf calorie totals
elf_count: .skip 8                  // Number of elves
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
    mov x2, #32768
    svc #0x80
    mov x20, x0             // Save bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Parse input into elf totals
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl parse_input

    // Part 1: Find maximum elf total
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

    // Part 2: Sum of top 3 totals
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

// Parse input into array of elf totals
// x0 = buffer, x1 = length
// Groups are separated by blank lines
parse_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end
    adrp x21, elf_totals@PAGE
    add x21, x21, elf_totals@PAGEOFF
    mov x22, #0             // Elf count
    mov x23, #0             // Current elf total

parse_loop:
    cmp x19, x20
    b.ge parse_finish_last

    // Check for newline at start of line (blank line)
    ldrb w24, [x19]
    cmp w24, #'\n'
    b.eq blank_line

    // Check for end of buffer
    cmp w24, #0
    b.eq parse_finish_last

    // Parse a number
    mov x0, x19
    bl parse_number
    mov x19, x0             // Update pointer
    add x23, x23, x1        // Add to current elf total

    // Skip to next line
    bl skip_to_newline
    mov x19, x0
    b parse_loop

blank_line:
    // Store current elf total if non-zero
    cbz x23, skip_store
    str x23, [x21, x22, lsl #3]
    add x22, x22, #1
    mov x23, #0

skip_store:
    add x19, x19, #1        // Skip the blank line
    b parse_loop

parse_finish_last:
    // Store final elf total if non-zero
    cbz x23, parse_done
    str x23, [x21, x22, lsl #3]
    add x22, x22, #1

parse_done:
    adrp x0, elf_count@PAGE
    add x0, x0, elf_count@PAGEOFF
    str x22, [x0]

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

// Skip to newline
// x0 = pointer, returns x0 = new pointer (after newline)
skip_to_newline:
skip_nl_loop:
    ldrb w1, [x0]
    cmp w1, #'\n'
    b.eq skip_nl_found
    cmp w1, #0
    b.eq skip_nl_end
    add x0, x0, #1
    b skip_nl_loop
skip_nl_found:
    add x0, x0, #1
skip_nl_end:
    ret

// Part 1: Find maximum elf total
// Returns max in x0
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    adrp x1, elf_totals@PAGE
    add x1, x1, elf_totals@PAGEOFF
    adrp x2, elf_count@PAGE
    add x2, x2, elf_count@PAGEOFF
    ldr x2, [x2]
    mov x0, #0              // Max value

part1_loop:
    cbz x2, part1_done
    ldr x3, [x1], #8
    cmp x3, x0
    csel x0, x3, x0, gt     // x0 = max(x0, x3)
    sub x2, x2, #1
    b part1_loop

part1_done:
    ldp x29, x30, [sp], #16
    ret

// Part 2: Sum of top 3 elf totals
// We maintain top3[0] >= top3[1] >= top3[2]
// Returns sum in x0
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    adrp x19, elf_totals@PAGE
    add x19, x19, elf_totals@PAGEOFF
    adrp x20, elf_count@PAGE
    add x20, x20, elf_count@PAGEOFF
    ldr x20, [x20]          // Count

    // Initialize top 3 to 0
    mov x21, #0             // top1 (largest)
    mov x22, #0             // top2
    mov x23, #0             // top3 (smallest of top 3)

part2_loop:
    cbz x20, part2_sum

    ldr x24, [x19], #8      // Current value
    sub x20, x20, #1

    // Check if current > top1
    cmp x24, x21
    b.le check_top2
    // Shift: top3 = top2, top2 = top1, top1 = current
    mov x23, x22
    mov x22, x21
    mov x21, x24
    b part2_loop

check_top2:
    // Check if current > top2
    cmp x24, x22
    b.le check_top3
    // Shift: top3 = top2, top2 = current
    mov x23, x22
    mov x22, x24
    b part2_loop

check_top3:
    // Check if current > top3
    cmp x24, x23
    b.le part2_loop
    // top3 = current
    mov x23, x24
    b part2_loop

part2_sum:
    // Sum top 3
    add x0, x21, x22
    add x0, x0, x23

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
