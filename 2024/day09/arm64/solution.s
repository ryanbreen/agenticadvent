// Day 9: Disk Fragmenter - ARM64 Assembly (macOS)
//
// Compact a fragmented disk by moving file blocks to fill gaps.
// Part 1: Move blocks one at a time from end to leftmost free space
// Part 2: Move whole files (highest ID first) to leftmost span that fits
//
// Algorithm:
//   1. Parse disk map: alternating digits = file length, free space length
//   2. Expand to block array: file ID or -1 for free space
//   3. Part 1: Two-pointer compaction (left finds free, right finds file)
//   4. Part 2: For each file (high ID to low), find leftmost fitting span
//   5. Calculate checksum: sum of position * file_id

.global _start
.align 4

// Constants
.equ MAX_INPUT_SIZE, 20480         // 20KB input buffer
.equ MAX_BLOCKS, 100000            // Max expanded blocks (~95KB)
.equ EMPTY, -1                     // Marker for free space

// Macro for loading addresses from data section
.macro LOAD_ADDR reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// ============================================================================
// Data Section
// ============================================================================
.data

// String constants
input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"
error_msg:      .asciz "Error reading file\n"

.align 3
// File I/O buffer
file_buffer:    .space MAX_INPUT_SIZE

// Block arrays - using 32-bit signed ints (file IDs can be large)
// Need two separate arrays since Part 1 and Part 2 operate independently
blocks_p1:      .space MAX_BLOCKS * 4
blocks_p2:      .space MAX_BLOCKS * 4
block_count:    .quad 0                 // Number of blocks after expansion

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_start:
    // Open and read input file
    LOAD_ADDR x0, input_path
    mov     x1, #0                          // O_RDONLY
    mov     x2, #0                          // mode (not used for O_RDONLY)
    mov     x16, #5                         // open() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    mov     x19, x0                         // Save fd in x19

    // Read file
    mov     x0, x19
    LOAD_ADDR x1, file_buffer
    mov     x2, #MAX_INPUT_SIZE
    mov     x16, #3                         // read() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    mov     x20, x0                         // Save bytes read in x20

    // Close file
    mov     x0, x19
    mov     x16, #6                         // close() syscall
    svc     #0x80

    // Parse disk map and expand to blocks
    bl      parse_and_expand

    // Solve Part 1
    bl      solve_part1
    mov     x21, x0                         // Save part1 result

    // Solve Part 2
    bl      solve_part2
    mov     x22, x0                         // Save part2 result

    // Print results
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x21
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    LOAD_ADDR x0, part2_msg
    bl      print_str
    mov     x0, x22
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Exit
    mov     x0, #0
    mov     x16, #1                         // exit() syscall
    svc     #0x80

error_exit:
    LOAD_ADDR x0, error_msg
    bl      print_str
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// ============================================================================
// parse_and_expand: Parse disk map and expand to block arrays
// Input: file_buffer contains disk map (alternating file/free digits)
// Output: blocks_p1 and blocks_p2 filled, block_count set
// ============================================================================
parse_and_expand:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x19, file_buffer              // Input pointer
    LOAD_ADDR x20, blocks_p1                // Output pointer (p1)
    LOAD_ADDR x21, blocks_p2                // Output pointer (p2)
    mov     x22, #0                         // File ID counter
    mov     x23, #1                         // is_file flag (start with file)
    mov     x24, #0                         // Block count

parse_loop:
    ldrb    w0, [x19], #1                   // Load next character

    // Check for end of input (newline, null, or non-digit)
    cbz     w0, parse_done
    cmp     w0, #'\n'
    b.eq    parse_done
    cmp     w0, #'0'
    b.lt    parse_done
    cmp     w0, #'9'
    b.gt    parse_done

    // Convert ASCII digit to number
    sub     w0, w0, #'0'

    // Determine what to write (file_id or -1)
    cmp     x23, #1
    b.eq    write_file

    // Write free space
    mvn     w1, wzr                         // w1 = -1 (all bits set)
    b       write_blocks

write_file:
    mov     w1, w22                         // w1 = file_id

write_blocks:
    // Write w0 blocks of value w1
    cbz     w0, next_section                // Skip if length is 0

write_loop:
    str     w1, [x20], #4                   // Store to blocks_p1
    str     w1, [x21], #4                   // Store to blocks_p2
    add     x24, x24, #1                    // Increment block count
    subs    w0, w0, #1
    b.ne    write_loop

next_section:
    // Toggle is_file flag
    eor     x23, x23, #1

    // If we just finished writing free space, increment file_id
    cmp     x23, #1
    b.eq    parse_loop                      // Just wrote free, now file
    add     x22, x22, #1                    // Just wrote file, increment ID
    b       parse_loop

parse_done:
    // Save block count
    LOAD_ADDR x0, block_count
    str     x24, [x0]

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part1: Compact by moving blocks one at a time
// Uses two-pointer technique: left finds free space, right finds file blocks
// Returns: checksum in x0
// ============================================================================
solve_part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x0, block_count
    ldr     x19, [x0]                       // x19 = block_count
    LOAD_ADDR x20, blocks_p1                // x20 = blocks array

    mov     x21, #0                         // left pointer
    sub     x22, x19, #1                    // right pointer = count - 1

compact_loop:
    cmp     x21, x22
    b.ge    compact_done

    // Find leftmost free space
find_left:
    cmp     x21, x22
    b.ge    compact_done
    ldr     w0, [x20, x21, lsl #2]
    cmn     w0, #1                          // Compare with -1
    b.eq    find_right                      // Found free space
    add     x21, x21, #1
    b       find_left

find_right:
    // Find rightmost file block
    cmp     x21, x22
    b.ge    compact_done
    ldr     w1, [x20, x22, lsl #2]
    cmn     w1, #1                          // Compare with -1
    b.ne    do_swap                         // Found file block
    sub     x22, x22, #1
    b       find_right

do_swap:
    // Swap blocks[left] and blocks[right]
    str     w1, [x20, x21, lsl #2]          // blocks[left] = blocks[right]
    mvn     w2, wzr                         // w2 = -1
    str     w2, [x20, x22, lsl #2]          // blocks[right] = -1
    add     x21, x21, #1
    sub     x22, x22, #1
    b       compact_loop

compact_done:
    // Calculate checksum
    bl      calculate_checksum_p1

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// solve_part2: Compact by moving whole files (highest ID first)
// Returns: checksum in x0
// ============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    LOAD_ADDR x0, block_count
    ldr     x19, [x0]                       // x19 = block_count
    LOAD_ADDR x20, blocks_p2                // x20 = blocks array

    // Find max file ID
    mov     x21, #0                         // max_file_id
    mov     x0, #0                          // index
find_max_id:
    cmp     x0, x19
    b.ge    found_max_id
    ldr     w1, [x20, x0, lsl #2]
    cmn     w1, #1                          // Skip -1
    b.eq    1f
    cmp     x1, x21
    b.le    1f
    mov     x21, x1                         // Update max
1:  add     x0, x0, #1
    b       find_max_id

found_max_id:
    // Process files from max_id down to 0
    mov     x22, x21                        // x22 = current file_id to process

process_file_loop:
    cmp     x22, #0
    b.lt    p2_done

    // Find this file's position and length
    mov     x23, #0                         // search index
find_file:
    cmp     x23, x19
    b.ge    next_file                       // File not found (shouldn't happen)
    ldr     w0, [x20, x23, lsl #2]
    sxtw    x0, w0                          // Sign extend for comparison
    cmp     x0, x22
    b.eq    found_file_start
    add     x23, x23, #1
    b       find_file

found_file_start:
    // x23 = file start position
    // Count file length
    mov     x24, x23                        // x24 = end index
count_file_len:
    cmp     x24, x19
    b.ge    found_file_end
    ldr     w0, [x20, x24, lsl #2]
    sxtw    x0, w0
    cmp     x0, x22
    b.ne    found_file_end
    add     x24, x24, #1
    b       count_file_len

found_file_end:
    // x23 = file_start, x24 = file_end (exclusive)
    // x25 = file_length
    sub     x25, x24, x23

    // Find leftmost free span that fits, before current position
    mov     x26, #0                         // search position
find_free_span:
    cmp     x26, x23                        // Must be left of current position
    b.ge    next_file                       // No suitable span found

    // Check if current position is free
    ldr     w0, [x20, x26, lsl #2]
    cmn     w0, #1
    b.ne    skip_to_next_free               // Not free, skip ahead

    // Count consecutive free blocks
    mov     x1, x26                         // span_start
    mov     x2, #0                          // span_length
count_free:
    cmp     x1, x23                         // Don't go past file position
    b.ge    check_span_fits
    ldr     w0, [x20, x1, lsl #2]
    cmn     w0, #1
    b.ne    check_span_fits                 // End of free span
    add     x2, x2, #1
    add     x1, x1, #1
    b       count_free

check_span_fits:
    // x2 = span_length, x25 = file_length
    cmp     x2, x25
    b.ge    move_file                       // Found suitable span at x26

    // Continue searching
    mov     x26, x1                         // Jump to end of this free span
    b       find_free_span

skip_to_next_free:
    add     x26, x26, #1
    b       find_free_span

move_file:
    // Move file from [x23, x24) to [x26, x26+length)
    // x26 = new_start, x23 = old_start, x24 = old_end

    // Clear old position
    mov     x0, x23
clear_old:
    cmp     x0, x24
    b.ge    write_new
    mvn     w1, wzr                         // -1
    str     w1, [x20, x0, lsl #2]
    add     x0, x0, #1
    b       clear_old

write_new:
    // Write to new position
    mov     x0, x26
    mov     x1, #0                          // offset counter
write_new_loop:
    cmp     x1, x25
    b.ge    next_file
    str     w22, [x20, x0, lsl #2]          // Write file_id
    add     x0, x0, #1
    add     x1, x1, #1
    b       write_new_loop

next_file:
    sub     x22, x22, #1
    b       process_file_loop

p2_done:
    // Calculate checksum
    bl      calculate_checksum_p2

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// calculate_checksum_p1: Calculate checksum for Part 1
// Sum of position * file_id for each non-empty block
// Returns: checksum in x0
// ============================================================================
calculate_checksum_p1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x0, block_count
    ldr     x19, [x0]                       // x19 = block_count
    LOAD_ADDR x20, blocks_p1                // x20 = blocks array

    mov     x21, #0                         // checksum accumulator
    mov     x22, #0                         // position counter

checksum_loop_p1:
    cmp     x22, x19
    b.ge    checksum_done_p1

    ldr     w0, [x20, x22, lsl #2]
    cmn     w0, #1                          // Skip if -1 (free space)
    b.eq    next_position_p1

    // Add position * file_id to checksum
    sxtw    x0, w0                          // Sign extend file_id to 64-bit
    mul     x1, x22, x0                     // position * file_id
    add     x21, x21, x1

next_position_p1:
    add     x22, x22, #1
    b       checksum_loop_p1

checksum_done_p1:
    mov     x0, x21                         // Return checksum

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// calculate_checksum_p2: Calculate checksum for Part 2
// Sum of position * file_id for each non-empty block
// Returns: checksum in x0
// ============================================================================
calculate_checksum_p2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x0, block_count
    ldr     x19, [x0]                       // x19 = block_count
    LOAD_ADDR x20, blocks_p2                // x20 = blocks array

    mov     x21, #0                         // checksum accumulator
    mov     x22, #0                         // position counter

checksum_loop_p2:
    cmp     x22, x19
    b.ge    checksum_done_p2

    ldr     w0, [x20, x22, lsl #2]
    cmn     w0, #1                          // Skip if -1 (free space)
    b.eq    next_position_p2

    // Add position * file_id to checksum
    sxtw    x0, w0                          // Sign extend file_id to 64-bit
    mul     x1, x22, x0                     // position * file_id
    add     x21, x21, x1

next_position_p2:
    add     x22, x22, #1
    b       checksum_loop_p2

checksum_done_p2:
    mov     x0, x21                         // Return checksum

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print a null-terminated string
// Input: x0 = address of string
// ============================================================================
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Find length
    mov     x20, #0
1:  ldrb    w1, [x19, x20]
    cbz     w1, 2f
    add     x20, x20, #1
    b       1b

2:  // Write to stdout
    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print a number
// Input: x0 = number to print
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

    // Handle zero case
    cbnz    x19, 1f
    sub     x20, x20, #1
    mov     w0, #'0'
    strb    w0, [x20]
    b       2f

1:  cbz     x19, 2f
    mov     x1, #10
    udiv    x2, x19, x1
    msub    x3, x2, x1, x19
    add     w3, w3, #'0'
    sub     x20, x20, #1
    strb    w3, [x20]
    mov     x19, x2
    b       1b

2:  mov     x0, x20
    bl      print_str

    add     sp, sp, #32
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
