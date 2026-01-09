// Day 20: Grove Positioning System - ARM64 Assembly Solution
// Circular list mixing with original order tracking

.global _start
.align 4

// Constants
.equ MAX_NUMBERS, 5100
.equ BUFFER_SIZE, 65536

.data
.align 4
filename:       .asciz "../input.txt"
fmt_part1:      .asciz "Part 1: "
fmt_part2:      .asciz "Part 2: "
newline:        .asciz "\n"
minus:          .asciz "-"

.bss
.align 4
buffer:         .skip BUFFER_SIZE
// Each entry: 16 bytes (8 for original_index, 8 for value)
indexed:        .skip MAX_NUMBERS * 16
num_count:      .skip 8
zero_orig_idx:  .skip 8

.text
.align 4

// ============================================================
// _start - Entry point
// ============================================================
_start:
    // Read input file
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    bl read_file

    // Parse numbers
    adrp x0, buffer@PAGE
    add x0, x0, buffer@PAGEOFF
    bl parse_numbers

    // Part 1
    bl solve_part1
    mov x19, x0             // save part1 result
    mov x20, x1             // save sign

    // Print Part 1
    adrp x0, fmt_part1@PAGE
    add x0, x0, fmt_part1@PAGEOFF
    bl print_string
    mov x0, x19
    mov x1, x20
    bl print_signed_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    // Part 2
    bl solve_part2
    mov x21, x0             // save part2 result
    mov x22, x1             // save sign

    // Print Part 2
    adrp x0, fmt_part2@PAGE
    add x0, x0, fmt_part2@PAGEOFF
    bl print_string
    mov x0, x21
    mov x1, x22
    bl print_signed_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    // Exit
    mov x0, #0
    mov x16, #1
    svc #0x80

// ============================================================
// read_file - Read entire file into buffer
// x0 = filename
// ============================================================
read_file:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    // Open file
    mov x16, #5             // open syscall
    mov x1, #0              // O_RDONLY
    svc #0x80

    mov x19, x0             // save fd

    // Read file
    mov x16, #3             // read syscall
    adrp x1, buffer@PAGE
    add x1, x1, buffer@PAGEOFF
    mov x2, #BUFFER_SIZE
    svc #0x80

    mov x20, x0             // bytes read

    // Null terminate
    adrp x1, buffer@PAGE
    add x1, x1, buffer@PAGEOFF
    strb wzr, [x1, x20]

    // Close file
    mov x0, x19
    mov x16, #6             // close syscall
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// parse_numbers - Parse signed integers from buffer
// x0 = buffer pointer
// ============================================================
parse_numbers:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0             // buffer ptr
    adrp x20, indexed@PAGE
    add x20, x20, indexed@PAGEOFF  // indexed array
    mov x21, #0             // count

.parse_loop:
    // Skip whitespace
    ldrb w0, [x19]
    cbz w0, .parse_done
    cmp w0, #'\n'
    beq .skip_ws
    cmp w0, #' '
    beq .skip_ws
    cmp w0, #'\t'
    beq .skip_ws
    cmp w0, #'\r'
    beq .skip_ws
    b .parse_number

.skip_ws:
    add x19, x19, #1
    b .parse_loop

.parse_number:
    // Check for minus sign
    mov x22, #0             // negative flag
    ldrb w0, [x19]
    cmp w0, #'-'
    bne .parse_digits
    mov x22, #1
    add x19, x19, #1

.parse_digits:
    mov x0, #0              // value
.digit_loop:
    ldrb w1, [x19]
    cmp w1, #'0'
    blt .digit_done
    cmp w1, #'9'
    bgt .digit_done
    sub w1, w1, #'0'
    mov x2, #10
    mul x0, x0, x2
    add x0, x0, x1
    add x19, x19, #1
    b .digit_loop

.digit_done:
    // Apply negative if needed
    cbz x22, .store_number
    neg x0, x0

.store_number:
    // Store (original_index, value)
    str x21, [x20]          // original_index
    str x0, [x20, #8]       // value

    // Track zero's original index
    cbnz x0, .not_zero
    adrp x1, zero_orig_idx@PAGE
    add x1, x1, zero_orig_idx@PAGEOFF
    str x21, [x1]

.not_zero:
    add x20, x20, #16
    add x21, x21, #1
    b .parse_loop

.parse_done:
    adrp x0, num_count@PAGE
    add x0, x0, num_count@PAGEOFF
    str x21, [x0]

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// copy_and_init - Reset indexed array from original values
// x0 = multiplier (1 for part1, decryption_key for part2)
// ============================================================
copy_and_init:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0             // multiplier

    adrp x20, num_count@PAGE
    add x20, x20, num_count@PAGEOFF
    ldr x20, [x20]          // n

    adrp x21, indexed@PAGE
    add x21, x21, indexed@PAGEOFF

    // Re-read input and reinitialize
    adrp x0, buffer@PAGE
    add x0, x0, buffer@PAGEOFF
    mov x22, #0             // index

.copy_loop:
    cmp x22, x20
    bge .copy_done

    // Skip whitespace
    ldrb w1, [x0]
    cbz w1, .copy_done
    cmp w1, #'\n'
    beq .copy_skip
    cmp w1, #' '
    beq .copy_skip
    cmp w1, #'\t'
    beq .copy_skip
    cmp w1, #'\r'
    beq .copy_skip
    b .copy_parse

.copy_skip:
    add x0, x0, #1
    b .copy_loop

.copy_parse:
    // Check for minus
    mov x2, #0              // negative flag
    ldrb w1, [x0]
    cmp w1, #'-'
    bne .copy_digits
    mov x2, #1
    add x0, x0, #1

.copy_digits:
    mov x3, #0              // value
.copy_digit_loop:
    ldrb w1, [x0]
    cmp w1, #'0'
    blt .copy_digit_done
    cmp w1, #'9'
    bgt .copy_digit_done
    sub w1, w1, #'0'
    mov x4, #10
    mul x3, x3, x4
    add x3, x3, x1
    add x0, x0, #1
    b .copy_digit_loop

.copy_digit_done:
    cbz x2, .copy_store
    neg x3, x3

.copy_store:
    // Multiply by multiplier
    mul x3, x3, x19

    // Store (original_index, value)
    str x22, [x21]          // original_index
    str x3, [x21, #8]       // value * multiplier

    add x21, x21, #16
    add x22, x22, #1
    b .copy_loop

.copy_done:
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// mix - Mix the list once
// ============================================================
mix:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    adrp x19, num_count@PAGE
    add x19, x19, num_count@PAGEOFF
    ldr x19, [x19]          // n

    adrp x20, indexed@PAGE
    add x20, x20, indexed@PAGEOFF

    sub x21, x19, #1        // n - 1 (for modulo)
    mov x22, #0             // orig_idx to process

.mix_outer:
    cmp x22, x19
    bge .mix_done

    // Find current position of element with original index == x22
    mov x23, #0             // curr_pos
.find_loop:
    cmp x23, x19
    bge .mix_next           // Should not happen

    // Calculate address of indexed[curr_pos]
    lsl x24, x23, #4        // curr_pos * 16
    add x24, x20, x24       // &indexed[curr_pos]
    ldr x25, [x24]          // original_index
    cmp x25, x22
    beq .found_pos
    add x23, x23, #1
    b .find_loop

.found_pos:
    // x23 = curr_pos, x24 = &indexed[curr_pos]
    ldr x25, [x24, #8]      // value

    // Calculate new position: (curr_pos + value) % (n - 1)
    // Need signed modulo that always gives non-negative result
    add x26, x23, x25       // curr_pos + value

    // Signed modulo: new_pos = ((curr_pos + value) % (n-1) + (n-1)) % (n-1)
    sdiv x27, x26, x21      // (curr_pos + value) / (n-1)
    msub x26, x27, x21, x26 // remainder (can be negative)

    // Make positive if negative
    cmp x26, #0
    bge .pos_ok
    add x26, x26, x21
.pos_ok:
    // x26 = new_pos

    // If new_pos == curr_pos, no move needed
    cmp x26, x23
    beq .mix_next

    // Save the element to move
    ldr x27, [x24]          // orig_idx
    ldr x28, [x24, #8]      // value

    // Shift elements
    cmp x26, x23
    bgt .shift_left
    b .shift_right

.shift_left:
    // new_pos > curr_pos: shift elements [curr_pos+1..new_pos] left by 1
    mov x0, x23             // i = curr_pos
.shift_left_loop:
    cmp x0, x26
    bge .shift_left_done

    add x1, x0, #1          // i + 1
    lsl x2, x1, #4          // (i+1) * 16
    add x2, x20, x2         // &indexed[i+1]
    ldr x3, [x2]            // orig_idx
    ldr x4, [x2, #8]        // value

    lsl x5, x0, #4          // i * 16
    add x5, x20, x5         // &indexed[i]
    str x3, [x5]
    str x4, [x5, #8]

    add x0, x0, #1
    b .shift_left_loop

.shift_left_done:
    // Insert at new_pos
    lsl x0, x26, #4
    add x0, x20, x0
    str x27, [x0]
    str x28, [x0, #8]
    b .mix_next

.shift_right:
    // new_pos < curr_pos: shift elements [new_pos..curr_pos-1] right by 1
    mov x0, x23             // i = curr_pos
.shift_right_loop:
    cmp x0, x26
    ble .shift_right_done

    sub x1, x0, #1          // i - 1
    lsl x2, x1, #4          // (i-1) * 16
    add x2, x20, x2         // &indexed[i-1]
    ldr x3, [x2]            // orig_idx
    ldr x4, [x2, #8]        // value

    lsl x5, x0, #4          // i * 16
    add x5, x20, x5         // &indexed[i]
    str x3, [x5]
    str x4, [x5, #8]

    sub x0, x0, #1
    b .shift_right_loop

.shift_right_done:
    // Insert at new_pos
    lsl x0, x26, #4
    add x0, x20, x0
    str x27, [x0]
    str x28, [x0, #8]

.mix_next:
    add x22, x22, #1
    b .mix_outer

.mix_done:
    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// grove_coordinates - Find sum of values at 1000, 2000, 3000 after 0
// Returns: x0 = absolute value, x1 = sign (0=positive, 1=negative)
// ============================================================
grove_coordinates:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    adrp x19, num_count@PAGE
    add x19, x19, num_count@PAGEOFF
    ldr x19, [x19]          // n

    adrp x20, indexed@PAGE
    add x20, x20, indexed@PAGEOFF

    adrp x21, zero_orig_idx@PAGE
    add x21, x21, zero_orig_idx@PAGEOFF
    ldr x21, [x21]          // zero's original index

    // Find current position of zero
    mov x22, #0             // pos
.find_zero:
    cmp x22, x19
    bge .zero_not_found     // Should not happen

    lsl x0, x22, #4
    add x0, x20, x0
    ldr x1, [x0]            // orig_idx
    cmp x1, x21
    beq .zero_found
    add x22, x22, #1
    b .find_zero

.zero_not_found:
    mov x0, #0
    mov x1, #0
    b .gc_done

.zero_found:
    // x22 = zero_pos
    mov x23, #0             // sum

    // Get value at (zero_pos + 1000) % n
    mov x0, #1000
    add x0, x22, x0
    udiv x1, x0, x19
    msub x0, x1, x19, x0    // (zero_pos + 1000) % n
    lsl x0, x0, #4
    add x0, x20, x0
    ldr x24, [x0, #8]       // value
    add x23, x23, x24

    // Get value at (zero_pos + 2000) % n
    mov x0, #2000
    add x0, x22, x0
    udiv x1, x0, x19
    msub x0, x1, x19, x0
    lsl x0, x0, #4
    add x0, x20, x0
    ldr x24, [x0, #8]
    add x23, x23, x24

    // Get value at (zero_pos + 3000) % n
    mov x0, #3000
    add x0, x22, x0
    udiv x1, x0, x19
    msub x0, x1, x19, x0
    lsl x0, x0, #4
    add x0, x20, x0
    ldr x24, [x0, #8]
    add x23, x23, x24

    // Return sum with sign
    mov x0, x23
    cmp x23, #0
    bge .gc_positive
    neg x0, x23
    mov x1, #1
    b .gc_done

.gc_positive:
    mov x1, #0

.gc_done:
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// solve_part1 - Mix once, find grove coordinates
// Returns: x0 = value, x1 = sign
// ============================================================
solve_part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Initialize with multiplier 1
    mov x0, #1
    bl copy_and_init

    // Mix once
    bl mix

    // Get coordinates
    bl grove_coordinates

    ldp x29, x30, [sp], #16
    ret

// ============================================================
// solve_part2 - Multiply by key, mix 10 times
// Returns: x0 = value, x1 = sign
// ============================================================
solve_part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    // Initialize with decryption key 811589153
    mov x0, #811589153 & 0xFFFF
    movk x0, #(811589153 >> 16) & 0xFFFF, lsl #16
    bl copy_and_init

    // Mix 10 times
    mov x19, #10
.mix_loop:
    cbz x19, .mix_loop_done
    bl mix
    sub x19, x19, #1
    b .mix_loop

.mix_loop_done:
    // Get coordinates
    bl grove_coordinates

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// print_string - Print null-terminated string
// x0 = string ptr
// ============================================================
print_string:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    // Find length
    mov x1, #0
.ps_len:
    ldrb w2, [x19, x1]
    cbz w2, .ps_write
    add x1, x1, #1
    b .ps_len

.ps_write:
    mov x2, x1              // length
    mov x0, #1              // stdout
    mov x1, x19             // buffer
    mov x16, #4             // write syscall
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// print_signed_number - Print signed 64-bit integer
// x0 = absolute value, x1 = sign (0=positive, 1=negative)
// ============================================================
print_signed_number:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    sub sp, sp, #32

    mov x19, x0             // value
    mov x20, x1             // sign

    // Print minus if negative
    cbz x20, .psn_print_num
    adrp x0, minus@PAGE
    add x0, x0, minus@PAGEOFF
    bl print_string

.psn_print_num:
    add x2, sp, #31         // end of buffer
    strb wzr, [x2]          // null terminator

    mov x1, x19             // value

    // Handle zero
    cbnz x1, .psn_loop
    sub x2, x2, #1
    mov w3, #'0'
    strb w3, [x2]
    b .psn_print

.psn_loop:
    cbz x1, .psn_print
    mov x3, #10
    udiv x4, x1, x3
    msub x5, x4, x3, x1     // remainder
    add w5, w5, #'0'
    sub x2, x2, #1
    strb w5, [x2]
    mov x1, x4
    b .psn_loop

.psn_print:
    mov x0, x2
    bl print_string

    add sp, sp, #32
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
