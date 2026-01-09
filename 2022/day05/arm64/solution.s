// ARM64 Assembly solution for AoC 2022 Day 5 - Supply Stacks
// macOS syscalls

.global _start
.align 2

.equ STDOUT, 1
.equ MAX_STACKS, 10
.equ MAX_STACK_HEIGHT, 256
.equ MAX_MOVES, 1024

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 65536
buffer_len: .quad 0

// Each stack is a byte array of up to MAX_STACK_HEIGHT chars
// stacks[i] = array of bytes, stacks_len[i] = length
.align 3
stacks1: .skip MAX_STACKS * MAX_STACK_HEIGHT
stacks1_len: .skip MAX_STACKS * 8

.align 3
stacks2: .skip MAX_STACKS * MAX_STACK_HEIGHT
stacks2_len: .skip MAX_STACKS * 8

num_stacks: .quad 0

// Moves stored as (count, from, to) triples
.align 3
moves: .skip MAX_MOVES * 24
num_moves: .quad 0

output_buffer: .skip 64
temp_stack: .skip MAX_STACK_HEIGHT

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
    mov x2, #65536
    svc #0x80

    adrp x1, buffer_len@PAGE
    add x1, x1, buffer_len@PAGEOFF
    str x0, [x1]            // Save bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Parse input
    bl parse_input

    // Copy original stacks to stacks2 BEFORE running part1
    bl copy_stacks_to_2

    // Part 1: Move crates one at a time
    bl part1

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str
    bl print_top_crates_1
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2: Move multiple crates at once (stacks2 already has original stacks)
    bl part2

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str
    bl print_top_crates_2
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

// Parse input file into stacks and moves
parse_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    adrp x19, file_buffer@PAGE
    add x19, x19, file_buffer@PAGEOFF
    adrp x20, buffer_len@PAGE
    add x20, x20, buffer_len@PAGEOFF
    ldr x20, [x20]
    add x20, x19, x20       // End of buffer

    // First, find the blank line that separates stacks from moves
    mov x21, x19            // Current position
find_blank_line:
    cmp x21, x20
    b.ge parse_done
    ldrb w22, [x21]
    cmp w22, #'\n'
    b.ne not_blank_check
    // Check if next char is also newline (blank line)
    add x23, x21, #1
    cmp x23, x20
    b.ge not_blank_check
    ldrb w22, [x23]
    cmp w22, #'\n'
    b.eq found_blank_line
not_blank_check:
    add x21, x21, #1
    b find_blank_line

found_blank_line:
    // x21 points to first \n of blank line
    // Stack section ends at x21
    // Move section starts at x21+2 (after \n\n)

    // First, find the number row (last line before blank) to count stacks
    // Go back from x21 to find start of number row
    mov x22, x21
    sub x22, x22, #1        // Skip the newline
find_number_row_start:
    cmp x22, x19
    b.le at_start
    sub x23, x22, #1
    ldrb w24, [x23]
    cmp w24, #'\n'
    b.eq found_number_row_start
    sub x22, x22, #1
    b find_number_row_start
at_start:
    mov x22, x19
    b count_stacks_in_row
found_number_row_start:
    // x22 points to first char of number row

count_stacks_in_row:
    // Count the digits to find number of stacks (last digit is count)
    mov x25, #0             // Stack count
count_digits:
    cmp x22, x21
    b.ge done_counting
    ldrb w24, [x22]
    cmp w24, #'1'
    b.lt skip_non_digit
    cmp w24, #'9'
    b.gt skip_non_digit
    sub w25, w24, #'0'      // Convert to number (use last one found)
skip_non_digit:
    add x22, x22, #1
    b count_digits

done_counting:
    adrp x0, num_stacks@PAGE
    add x0, x0, num_stacks@PAGEOFF
    str x25, [x0]

    // Initialize stack lengths to 0
    adrp x0, stacks1_len@PAGE
    add x0, x0, stacks1_len@PAGEOFF
    mov x1, #0
init_lens:
    cmp x1, x25
    b.ge parse_stack_lines
    str xzr, [x0, x1, lsl #3]
    add x1, x1, #1
    b init_lens

parse_stack_lines:
    // Parse each line of the stack diagram
    // Crates are at positions 1, 5, 9, ... (1 + 4*i)
    // We read top to bottom, but need to store bottom to top
    // So first, collect all crate lines, then process bottom-up

    // Find how many stack lines there are (lines before the number row)
    mov x22, x19            // Start of buffer
    mov x26, #0             // Line count

    // Find number row start again
    mov x23, x21
    sub x23, x23, #1
find_num_row_again:
    cmp x23, x19
    b.le found_num_row_start_again
    sub x24, x23, #1
    ldrb w27, [x24]
    cmp w27, #'\n'
    b.eq found_num_row_start_again
    sub x23, x23, #1
    b find_num_row_again
found_num_row_start_again:
    // x23 is start of number row
    // Stack lines are from x19 to x23-1

    // Process stack lines from bottom to top (bottom = closest to number row)
    // Find end of stack diagram (line before number row)
    mov x26, x23            // End of crate section
    cmp x26, x19
    b.le no_crates
    sub x26, x26, #1        // Go back before \n

    // Find start of this line (last crate line)
process_crate_lines:
    cmp x26, x19
    b.lt done_parsing_crates

    // Find start of current line
    mov x27, x26
find_line_start:
    cmp x27, x19
    b.le at_line_start
    sub x28, x27, #1
    ldrb w0, [x28]
    cmp w0, #'\n'
    b.eq at_line_start
    sub x27, x27, #1
    b find_line_start
at_line_start:
    // x27 = start of line, x26 = end of line (before \n)
    // Parse crates at positions 1, 5, 9, ...
    mov x0, #0              // Stack index
parse_crates_in_line:
    adrp x1, num_stacks@PAGE
    add x1, x1, num_stacks@PAGEOFF
    ldr x1, [x1]
    cmp x0, x1
    b.ge done_line

    // Position in line = 1 + 4*stack_index
    mov x2, #4
    mul x2, x0, x2
    add x2, x2, #1
    add x2, x2, x27         // Absolute position

    cmp x2, x26
    b.gt next_stack

    ldrb w3, [x2]
    cmp w3, #'A'
    b.lt next_stack
    cmp w3, #'Z'
    b.gt next_stack

    // Found a crate, add to stack
    // stacks1[stack_index][stacks1_len[stack_index]++] = crate
    adrp x4, stacks1@PAGE
    add x4, x4, stacks1@PAGEOFF
    adrp x5, stacks1_len@PAGE
    add x5, x5, stacks1_len@PAGEOFF

    ldr x6, [x5, x0, lsl #3]  // Current length
    mov x7, #MAX_STACK_HEIGHT
    mul x7, x0, x7
    add x7, x7, x6          // Offset into stacks1
    strb w3, [x4, x7]
    add x6, x6, #1
    str x6, [x5, x0, lsl #3]

next_stack:
    add x0, x0, #1
    b parse_crates_in_line

done_line:
    // Move to previous line
    cmp x27, x19
    b.le done_parsing_crates
    sub x26, x27, #2        // Skip newline and go to end of previous line
    b process_crate_lines

no_crates:
done_parsing_crates:
    // Now parse the moves
    // Moves start at x21 + 2 (after blank line)
    add x22, x21, #2        // Start of moves
    mov x23, #0             // Move count
    adrp x24, moves@PAGE
    add x24, x24, moves@PAGEOFF

parse_moves_loop:
    cmp x22, x20
    b.ge done_moves

    // Skip any leading whitespace/newlines
    ldrb w25, [x22]
    cmp w25, #' '
    b.eq skip_ws
    cmp w25, #'\n'
    b.eq skip_ws
    cmp w25, #'\r'
    b.eq skip_ws
    cmp w25, #0
    b.eq done_moves
    b parse_move_line
skip_ws:
    add x22, x22, #1
    b parse_moves_loop

parse_move_line:
    // Expect "move N from X to Y"
    // Skip "move "
    add x22, x22, #5

    // Parse count
    mov x0, x22
    bl parse_number
    mov x22, x0             // Updated pointer
    mov x25, x1             // Count

    // Skip " from "
    add x22, x22, #6

    // Parse from stack (1-indexed, convert to 0-indexed)
    mov x0, x22
    bl parse_number
    mov x22, x0
    sub x26, x1, #1         // From stack (0-indexed)

    // Skip " to "
    add x22, x22, #4

    // Parse to stack (1-indexed, convert to 0-indexed)
    mov x0, x22
    bl parse_number
    mov x22, x0
    sub x27, x1, #1         // To stack (0-indexed)

    // Store move (count, from, to)
    str x25, [x24], #8
    str x26, [x24], #8
    str x27, [x24], #8
    add x23, x23, #1

    // Skip to end of line
skip_to_eol:
    cmp x22, x20
    b.ge done_moves
    ldrb w25, [x22]
    cmp w25, #'\n'
    b.eq found_eol
    cmp w25, #0
    b.eq done_moves
    add x22, x22, #1
    b skip_to_eol
found_eol:
    add x22, x22, #1
    b parse_moves_loop

done_moves:
    adrp x0, num_moves@PAGE
    add x0, x0, num_moves@PAGEOFF
    str x23, [x0]

parse_done:
    ldp x27, x28, [sp], #16
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

// Copy stacks1 to stacks2 (for part2 to use original stacks)
copy_stacks_to_2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    adrp x19, num_stacks@PAGE
    add x19, x19, num_stacks@PAGEOFF
    ldr x19, [x19]

    mov x20, #0             // Stack index
copy_stack_loop:
    cmp x20, x19
    b.ge copy_done

    // Copy length
    adrp x21, stacks1_len@PAGE
    add x21, x21, stacks1_len@PAGEOFF
    ldr x22, [x21, x20, lsl #3]

    adrp x21, stacks2_len@PAGE
    add x21, x21, stacks2_len@PAGEOFF
    str x22, [x21, x20, lsl #3]

    // Copy stack contents
    mov x0, #MAX_STACK_HEIGHT
    mul x0, x20, x0         // Offset

    adrp x1, stacks1@PAGE
    add x1, x1, stacks1@PAGEOFF
    add x1, x1, x0

    adrp x2, stacks2@PAGE
    add x2, x2, stacks2@PAGEOFF
    add x2, x2, x0

    mov x3, #0
copy_bytes:
    cmp x3, x22
    b.ge next_stack_copy
    ldrb w4, [x1, x3]
    strb w4, [x2, x3]
    add x3, x3, #1
    b copy_bytes

next_stack_copy:
    add x20, x20, #1
    b copy_stack_loop

copy_done:
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 1: Move crates one at a time (using stacks1)
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    adrp x19, moves@PAGE
    add x19, x19, moves@PAGEOFF
    adrp x20, num_moves@PAGE
    add x20, x20, num_moves@PAGEOFF
    ldr x20, [x20]

    mov x21, #0             // Move index
part1_move_loop:
    cmp x21, x20
    b.ge part1_done

    // Load move (count, from, to)
    mov x0, #24
    mul x0, x21, x0
    add x0, x0, x19
    ldr x22, [x0]           // Count
    ldr x23, [x0, #8]       // From
    ldr x24, [x0, #16]      // To

    // Move crates one at a time
part1_crate_loop:
    cbz x22, part1_next_move

    // Pop from source stack
    adrp x0, stacks1_len@PAGE
    add x0, x0, stacks1_len@PAGEOFF
    ldr x25, [x0, x23, lsl #3]
    sub x25, x25, #1
    str x25, [x0, x23, lsl #3]  // Decrement source length

    // Get crate
    adrp x1, stacks1@PAGE
    add x1, x1, stacks1@PAGEOFF
    mov x2, #MAX_STACK_HEIGHT
    mul x2, x23, x2
    add x2, x2, x25
    ldrb w26, [x1, x2]      // Crate char

    // Push to destination stack
    ldr x25, [x0, x24, lsl #3]
    mov x2, #MAX_STACK_HEIGHT
    mul x2, x24, x2
    add x2, x2, x25
    strb w26, [x1, x2]
    add x25, x25, #1
    str x25, [x0, x24, lsl #3]  // Increment dest length

    sub x22, x22, #1
    b part1_crate_loop

part1_next_move:
    add x21, x21, #1
    b part1_move_loop

part1_done:
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2: Move multiple crates at once (preserves order, using stacks2)
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    adrp x19, moves@PAGE
    add x19, x19, moves@PAGEOFF
    adrp x20, num_moves@PAGE
    add x20, x20, num_moves@PAGEOFF
    ldr x20, [x20]

    mov x21, #0             // Move index
part2_move_loop:
    cmp x21, x20
    b.ge part2_done

    // Load move (count, from, to)
    mov x0, #24
    mul x0, x21, x0
    add x0, x0, x19
    ldr x22, [x0]           // Count
    ldr x23, [x0, #8]       // From
    ldr x24, [x0, #16]      // To

    // Get source stack info
    adrp x0, stacks2_len@PAGE
    add x0, x0, stacks2_len@PAGEOFF
    ldr x25, [x0, x23, lsl #3]  // Source length

    adrp x1, stacks2@PAGE
    add x1, x1, stacks2@PAGEOFF

    // Copy crates to temp (preserving order)
    adrp x2, temp_stack@PAGE
    add x2, x2, temp_stack@PAGEOFF

    mov x26, #0             // Index in crates to copy
copy_to_temp:
    cmp x26, x22
    b.ge move_from_temp

    // Source position: stacks2[from][src_len - count + i]
    mov x3, #MAX_STACK_HEIGHT
    mul x3, x23, x3
    sub x4, x25, x22        // Start position
    add x4, x4, x26
    add x3, x3, x4
    ldrb w5, [x1, x3]
    strb w5, [x2, x26]

    add x26, x26, #1
    b copy_to_temp

move_from_temp:
    // Update source length
    sub x25, x25, x22
    str x25, [x0, x23, lsl #3]

    // Get dest stack info
    ldr x27, [x0, x24, lsl #3]  // Dest length

    // Copy from temp to dest (preserving order)
    mov x26, #0
copy_to_dest:
    cmp x26, x22
    b.ge part2_next_move

    ldrb w5, [x2, x26]

    // Dest position: stacks2[to][dest_len + i]
    mov x3, #MAX_STACK_HEIGHT
    mul x3, x24, x3
    add x4, x27, x26
    add x3, x3, x4
    strb w5, [x1, x3]

    add x26, x26, #1
    b copy_to_dest

part2_next_move:
    // Update dest length
    add x27, x27, x22
    str x27, [x0, x24, lsl #3]

    add x21, x21, #1
    b part2_move_loop

part2_done:
    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print top crates from stacks1
print_top_crates_1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    adrp x19, num_stacks@PAGE
    add x19, x19, num_stacks@PAGEOFF
    ldr x19, [x19]

    adrp x20, output_buffer@PAGE
    add x20, x20, output_buffer@PAGEOFF

    mov x21, #0             // Stack index
collect_tops_1:
    cmp x21, x19
    b.ge print_collected_1

    adrp x0, stacks1_len@PAGE
    add x0, x0, stacks1_len@PAGEOFF
    ldr x22, [x0, x21, lsl #3]

    cbz x22, skip_empty_1   // Skip empty stacks

    adrp x1, stacks1@PAGE
    add x1, x1, stacks1@PAGEOFF
    mov x2, #MAX_STACK_HEIGHT
    mul x2, x21, x2
    sub x3, x22, #1
    add x2, x2, x3
    ldrb w4, [x1, x2]
    strb w4, [x20, x21]

skip_empty_1:
    add x21, x21, #1
    b collect_tops_1

print_collected_1:
    strb wzr, [x20, x19]    // Null terminate
    mov x0, x20
    bl print_str

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print top crates from stacks2
print_top_crates_2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    adrp x19, num_stacks@PAGE
    add x19, x19, num_stacks@PAGEOFF
    ldr x19, [x19]

    adrp x20, output_buffer@PAGE
    add x20, x20, output_buffer@PAGEOFF

    mov x21, #0             // Stack index
collect_tops_2:
    cmp x21, x19
    b.ge print_collected_2

    adrp x0, stacks2_len@PAGE
    add x0, x0, stacks2_len@PAGEOFF
    ldr x22, [x0, x21, lsl #3]

    cbz x22, skip_empty_2   // Skip empty stacks

    adrp x1, stacks2@PAGE
    add x1, x1, stacks2@PAGEOFF
    mov x2, #MAX_STACK_HEIGHT
    mul x2, x21, x2
    sub x3, x22, #1
    add x2, x2, x3
    ldrb w4, [x1, x2]
    strb w4, [x20, x21]

skip_empty_2:
    add x21, x21, #1
    b collect_tops_2

print_collected_2:
    strb wzr, [x20, x19]    // Null terminate
    mov x0, x20
    bl print_str

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
