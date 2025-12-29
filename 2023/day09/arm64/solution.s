// Day 9: Mirage Maintenance - ARM64 Assembly for macOS
// Reads sequences of integers and extrapolates next/previous values

.global _start
.align 4

// Constants
.equ MAX_LINES, 210          // Maximum number of lines
.equ MAX_NUMS_PER_LINE, 30   // Maximum numbers per line
.equ MAX_DEPTH, 30           // Maximum recursion depth for differences
.equ BUFFER_SIZE, 32768      // File read buffer size

// BSD syscall numbers for macOS
.equ SYS_EXIT, 1
.equ SYS_READ, 3
.equ SYS_WRITE, 4
.equ SYS_OPEN, 5
.equ SYS_CLOSE, 6

.text
_start:
    // Open file "../input.txt"
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0                  // O_RDONLY
    mov x16, #SYS_OPEN
    svc #0x80
    cmp x0, #0
    b.lt exit_error
    mov x19, x0                 // Save file descriptor

    // Read file into buffer
    mov x0, x19                 // fd
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #BUFFER_SIZE
    mov x16, #SYS_READ
    svc #0x80
    cmp x0, #0
    b.le exit_error
    mov x20, x0                 // Save bytes read

    // Close file
    mov x0, x19
    mov x16, #SYS_CLOSE
    svc #0x80

    // Parse the input file
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20                 // Bytes read
    bl parse_input
    // x0 now contains number of lines parsed

    mov x19, x0                 // Save line count

    // Part 1: Sum of extrapolated next values
    mov x20, #0                 // part1_sum
    mov x21, #0                 // line index

part1_loop:
    cmp x21, x19
    b.ge part1_done

    mov x0, x21                 // line index
    bl extrapolate_next
    add x20, x20, x0            // Add to sum

    add x21, x21, #1
    b part1_loop

part1_done:
    // Part 2: Sum of extrapolated previous values
    mov x22, #0                 // part2_sum
    mov x21, #0                 // line index

part2_loop:
    cmp x21, x19
    b.ge part2_done

    mov x0, x21                 // line index
    bl extrapolate_prev
    add x22, x22, x0            // Add to sum

    add x21, x21, #1
    b part2_loop

part2_done:
    // Print Part 1 result
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    mov x1, #8
    bl print_str
    mov x0, x20
    bl print_number
    bl print_newline

    // Print Part 2 result
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    mov x1, #8
    bl print_str
    mov x0, x22
    bl print_number
    bl print_newline

    // Exit successfully
    mov x0, #0
    mov x16, #SYS_EXIT
    svc #0x80

exit_error:
    mov x0, #1
    mov x16, #SYS_EXIT
    svc #0x80

// ============================================================
// parse_input: Parse the input buffer into sequences
// Input: x0 = buffer pointer, x1 = length
// Output: x0 = number of lines parsed
// ============================================================
parse_input:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0                 // buffer ptr
    add x20, x0, x1             // buffer end
    mov x21, #0                 // line index

    adrp x22, sequences@PAGE
    add x22, x22, sequences@PAGEOFF

    adrp x23, seq_lengths@PAGE
    add x23, x23, seq_lengths@PAGEOFF

parse_line_loop:
    cmp x19, x20
    b.ge parse_done

    // Parse numbers for current line
    mov x24, #0                 // count of numbers in this line

    // Calculate pointer to current line's sequence storage
    mov x25, #MAX_NUMS_PER_LINE
    mul x25, x21, x25           // offset = line_idx * MAX_NUMS_PER_LINE
    lsl x25, x25, #3            // * 8 bytes per int64
    add x25, x22, x25           // x25 = address of sequences[line_idx][0]

parse_num_loop:
    cmp x19, x20
    b.ge save_line_length

    // Skip whitespace (but not newlines)
    ldrb w0, [x19]
    cmp w0, #' '
    b.ne check_tab
    add x19, x19, #1
    b parse_num_loop

check_tab:
    cmp w0, #'\t'
    b.ne check_newline
    add x19, x19, #1
    b parse_num_loop

check_newline:
    cmp w0, #'\n'
    b.ne parse_one_number
    add x19, x19, #1
    b save_line_length

parse_one_number:
    // Check if we have a digit or minus sign
    cmp w0, #'-'
    b.eq do_parse_number
    cmp w0, #'0'
    b.lt save_line_length       // Not a number character
    cmp w0, #'9'
    b.gt save_line_length

do_parse_number:
    // Parse one number
    mov x0, x19                 // current position
    mov x1, x20                 // buffer end
    bl parse_int64
    // x0 = parsed number, x1 = new position
    mov x19, x1                 // Update position

    // Store the number
    str x0, [x25, x24, lsl #3]
    add x24, x24, #1
    b parse_num_loop

save_line_length:
    // Only save if we have numbers
    cmp x24, #0
    b.eq parse_line_loop

    // Store length for this line
    str x24, [x23, x21, lsl #3]

    add x21, x21, #1
    b parse_line_loop

parse_done:
    mov x0, x21                 // Return line count

    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// parse_int64: Parse a signed 64-bit integer
// Input: x0 = string pointer, x1 = buffer end
// Output: x0 = parsed number, x1 = new position
// ============================================================
parse_int64:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0                 // current position
    mov x20, x1                 // buffer end
    mov x21, #0                 // accumulator
    mov x22, #0                 // negative flag

    // Check for minus sign
    ldrb w0, [x19]
    cmp w0, #'-'
    b.ne parse_digits
    mov x22, #1                 // Set negative flag
    add x19, x19, #1

parse_digits:
    cmp x19, x20
    b.ge parse_int_done

    ldrb w0, [x19]
    cmp w0, #'0'
    b.lt parse_int_done
    cmp w0, #'9'
    b.gt parse_int_done

    // accumulator = accumulator * 10 + digit
    mov x1, #10
    mul x21, x21, x1
    sub w0, w0, #'0'
    add x21, x21, x0

    add x19, x19, #1
    b parse_digits

parse_int_done:
    // Apply negative if needed
    cmp x22, #0
    b.eq no_negate
    neg x21, x21

no_negate:
    mov x0, x21                 // Return value
    mov x1, x19                 // Return new position

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// extrapolate_next: Compute next value for a sequence
// Input: x0 = line index
// Output: x0 = extrapolated next value
// ============================================================
extrapolate_next:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    mov x19, x0                 // line index

    // Get sequence length
    adrp x20, seq_lengths@PAGE
    add x20, x20, seq_lengths@PAGEOFF
    ldr x21, [x20, x19, lsl #3] // x21 = length of this sequence

    // Get pointer to sequence
    adrp x22, sequences@PAGE
    add x22, x22, sequences@PAGEOFF
    mov x0, #MAX_NUMS_PER_LINE
    mul x0, x19, x0
    lsl x0, x0, #3
    add x22, x22, x0            // x22 = &sequences[line_idx][0]

    // Copy sequence to work buffer (diff_work)
    adrp x23, diff_work@PAGE
    add x23, x23, diff_work@PAGEOFF
    mov x24, #0
copy_seq_next:
    cmp x24, x21
    b.ge copy_done_next
    ldr x0, [x22, x24, lsl #3]
    str x0, [x23, x24, lsl #3]
    add x24, x24, #1
    b copy_seq_next

copy_done_next:
    // Store initial length
    adrp x25, depth_lengths@PAGE
    add x25, x25, depth_lengths@PAGEOFF
    str x21, [x25]              // depth_lengths[0] = original length

    mov x26, #0                 // depth counter
    mov x27, x21                // current length

compute_diffs_next:
    // Check if all zeros
    mov x24, #0                 // index
    mov x28, #1                 // all_zeros flag

    // Calculate offset for current depth
    mov x0, #MAX_NUMS_PER_LINE
    mul x0, x26, x0
    lsl x0, x0, #3
    add x1, x23, x0             // x1 = current level ptr

check_zeros_next:
    cmp x24, x27
    b.ge check_zeros_done_next
    ldr x0, [x1, x24, lsl #3]
    cmp x0, #0
    b.eq next_zero_check
    mov x28, #0
    b check_zeros_done_next
next_zero_check:
    add x24, x24, #1
    b check_zeros_next

check_zeros_done_next:
    cmp x28, #1
    b.eq extrapolate_back_next

    // Compute differences
    sub x27, x27, #1            // new length = len - 1
    cmp x27, #0
    b.le extrapolate_back_next

    add x26, x26, #1            // depth++

    // Store new length at this depth
    str x27, [x25, x26, lsl #3]

    // Calculate pointers for prev and current level
    mov x0, #MAX_NUMS_PER_LINE
    sub x2, x26, #1
    mul x2, x2, x0
    lsl x2, x2, #3
    add x3, x23, x2             // x3 = prev level ptr

    mul x0, x26, x0
    lsl x0, x0, #3
    add x4, x23, x0             // x4 = current level ptr

    mov x24, #0                 // index
diff_loop_next:
    cmp x24, x27
    b.ge compute_diffs_next

    add x5, x24, #1
    ldr x0, [x3, x5, lsl #3]    // seq[i+1]
    ldr x1, [x3, x24, lsl #3]   // seq[i]
    sub x0, x0, x1              // diff
    str x0, [x4, x24, lsl #3]

    add x24, x24, #1
    b diff_loop_next

extrapolate_back_next:
    // Now work back up, adding last values
    // x26 = current depth (the zero level)
    mov x24, #0                 // This will hold the extrapolated value from below

extrap_loop_next:
    cmp x26, #0
    b.lt extrap_done_next

    // Get pointer to this level
    mov x0, #MAX_NUMS_PER_LINE
    mul x0, x26, x0
    lsl x0, x0, #3
    add x1, x23, x0             // x1 = current level ptr

    // Get length at this level
    ldr x2, [x25, x26, lsl #3]

    // Get last value at this level
    sub x3, x2, #1
    ldr x0, [x1, x3, lsl #3]    // last value at this level

    // Add extrapolated value from below
    add x24, x24, x0

    sub x26, x26, #1
    b extrap_loop_next

extrap_done_next:
    mov x0, x24                 // Return extrapolated value

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// extrapolate_prev: Compute previous value for a sequence
// Input: x0 = line index
// Output: x0 = extrapolated previous value
// ============================================================
extrapolate_prev:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    mov x19, x0                 // line index

    // Get sequence length
    adrp x20, seq_lengths@PAGE
    add x20, x20, seq_lengths@PAGEOFF
    ldr x21, [x20, x19, lsl #3] // x21 = length of this sequence

    // Get pointer to sequence
    adrp x22, sequences@PAGE
    add x22, x22, sequences@PAGEOFF
    mov x0, #MAX_NUMS_PER_LINE
    mul x0, x19, x0
    lsl x0, x0, #3
    add x22, x22, x0            // x22 = &sequences[line_idx][0]

    // Copy sequence to work buffer (diff_work)
    adrp x23, diff_work@PAGE
    add x23, x23, diff_work@PAGEOFF
    mov x24, #0
copy_seq_prev:
    cmp x24, x21
    b.ge copy_done_prev
    ldr x0, [x22, x24, lsl #3]
    str x0, [x23, x24, lsl #3]
    add x24, x24, #1
    b copy_seq_prev

copy_done_prev:
    // Store initial length
    adrp x25, depth_lengths@PAGE
    add x25, x25, depth_lengths@PAGEOFF
    str x21, [x25]              // depth_lengths[0] = original length

    // Also store first values for each level
    adrp x28, first_values@PAGE
    add x28, x28, first_values@PAGEOFF
    ldr x0, [x23]               // First value of original sequence
    str x0, [x28]

    mov x26, #0                 // depth counter
    mov x27, x21                // current length

compute_diffs_prev:
    // Check if all zeros
    mov x24, #0                 // index
    mov x6, #1                  // all_zeros flag (use x6 to avoid x28 conflict)

    // Calculate offset for current depth
    mov x0, #MAX_NUMS_PER_LINE
    mul x0, x26, x0
    lsl x0, x0, #3
    add x1, x23, x0             // x1 = current level ptr

check_zeros_prev:
    cmp x24, x27
    b.ge check_zeros_done_prev
    ldr x0, [x1, x24, lsl #3]
    cmp x0, #0
    b.eq next_zero_check_prev
    mov x6, #0
    b check_zeros_done_prev
next_zero_check_prev:
    add x24, x24, #1
    b check_zeros_prev

check_zeros_done_prev:
    cmp x6, #1
    b.eq extrapolate_back_prev

    // Compute differences
    sub x27, x27, #1            // new length = len - 1
    cmp x27, #0
    b.le extrapolate_back_prev

    add x26, x26, #1            // depth++

    // Store new length at this depth
    str x27, [x25, x26, lsl #3]

    // Calculate pointers for prev and current level
    mov x0, #MAX_NUMS_PER_LINE
    sub x2, x26, #1
    mul x2, x2, x0
    lsl x2, x2, #3
    add x3, x23, x2             // x3 = prev level ptr

    mul x0, x26, x0
    lsl x0, x0, #3
    add x4, x23, x0             // x4 = current level ptr

    mov x24, #0                 // index
diff_loop_prev:
    cmp x24, x27
    b.ge store_first_val

    add x5, x24, #1
    ldr x0, [x3, x5, lsl #3]    // seq[i+1]
    ldr x1, [x3, x24, lsl #3]   // seq[i]
    sub x0, x0, x1              // diff
    str x0, [x4, x24, lsl #3]

    add x24, x24, #1
    b diff_loop_prev

store_first_val:
    // Store first value for this depth level
    ldr x0, [x4]                // First value at this level
    str x0, [x28, x26, lsl #3]
    b compute_diffs_prev

extrapolate_back_prev:
    // Now work back up, subtracting from first values
    // x26 = current depth (the zero level)
    // Store 0 as the first value for the zero level
    str xzr, [x28, x26, lsl #3]

    mov x24, #0                 // This will hold the extrapolated value from below

extrap_loop_prev:
    cmp x26, #0
    b.lt extrap_done_prev

    // Get first value at this level
    ldr x0, [x28, x26, lsl #3]

    // prev_extrapolated = first_val - below_extrapolated
    sub x24, x0, x24

    sub x26, x26, #1
    b extrap_loop_prev

extrap_done_prev:
    mov x0, x24                 // Return extrapolated value

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// print_str: Print a string
// Input: x0 = string pointer, x1 = length
// ============================================================
print_str:
    stp x29, x30, [sp, #-16]!

    mov x2, x1                  // length
    mov x1, x0                  // buffer
    mov x0, #1                  // stdout
    mov x16, #SYS_WRITE
    svc #0x80

    ldp x29, x30, [sp], #16
    ret

// ============================================================
// print_number: Print a signed 64-bit number
// Input: x0 = number
// ============================================================
print_number:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0                 // number
    adrp x20, num_buffer@PAGE
    add x20, x20, num_buffer@PAGEOFF
    add x20, x20, #20           // End of buffer
    mov x21, x20                // Save end position
    mov x22, #0                 // negative flag

    // Handle negative
    cmp x19, #0
    b.ge positive_num
    neg x19, x19
    mov x22, #1

positive_num:
    // Handle zero specially
    cmp x19, #0
    b.ne conv_loop
    mov w0, #'0'
    sub x20, x20, #1
    strb w0, [x20]
    b print_done

conv_loop:
    cmp x19, #0
    b.eq conv_done

    mov x0, #10
    udiv x1, x19, x0            // quotient
    msub x2, x1, x0, x19        // remainder
    mov x19, x1

    add w2, w2, #'0'
    sub x20, x20, #1
    strb w2, [x20]
    b conv_loop

conv_done:
    // Add minus sign if negative
    cmp x22, #0
    b.eq print_done
    mov w0, #'-'
    sub x20, x20, #1
    strb w0, [x20]

print_done:
    // Calculate length and print
    sub x1, x21, x20            // length
    mov x2, x1
    mov x1, x20
    mov x0, #1
    mov x16, #SYS_WRITE
    svc #0x80

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// print_newline: Print a newline character
// ============================================================
print_newline:
    stp x29, x30, [sp, #-16]!

    adrp x1, newline@PAGE
    add x1, x1, newline@PAGEOFF
    mov x0, #1
    mov x2, #1
    mov x16, #SYS_WRITE
    svc #0x80

    ldp x29, x30, [sp], #16
    ret

// ============================================================
// Data Section
// ============================================================
.data
filename:
    .asciz "../input.txt"
part1_msg:
    .ascii "Part 1: "
part2_msg:
    .ascii "Part 2: "
newline:
    .ascii "\n"

.bss
.align 4
file_buffer:
    .space BUFFER_SIZE
num_buffer:
    .space 32
sequences:
    .space MAX_LINES * MAX_NUMS_PER_LINE * 8
seq_lengths:
    .space MAX_LINES * 8
diff_work:
    .space MAX_DEPTH * MAX_NUMS_PER_LINE * 8
depth_lengths:
    .space MAX_DEPTH * 8
first_values:
    .space MAX_DEPTH * 8
