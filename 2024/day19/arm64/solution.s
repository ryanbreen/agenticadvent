// Day 19: Linen Layout - ARM64 Assembly (macOS)
//
// Part 1: Count designs that CAN be formed by concatenating towel patterns
// Part 2: Sum the NUMBER OF WAYS each design can be formed
// Algorithm: Bottom-up DP - dp[i] = ways to form design[i:]

.global _start
.align 4

// Constants
.equ MAX_INPUT_SIZE, 65536
.equ MAX_PATTERNS, 512
.equ MAX_PATTERN_LEN, 16
.equ MAX_DESIGNS, 512
.equ MAX_DESIGN_LEN, 128
.equ MAX_DP_SIZE, 256

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

// Patterns storage: array of pointers to pattern strings
pattern_ptrs:   .space MAX_PATTERNS * 8
pattern_lens:   .space MAX_PATTERNS * 4
num_patterns:   .quad 0

// Designs storage: array of pointers to design strings
design_ptrs:    .space MAX_DESIGNS * 8
design_lens:    .space MAX_DESIGNS * 4
num_designs:    .quad 0

// DP array for count_ways (64-bit integers for Part 2)
dp_array:       .space MAX_DP_SIZE * 8

// Results
part1_result:   .quad 0
part2_result:   .quad 0

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_start:
    // Open input file
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

    // Null-terminate the buffer
    LOAD_ADDR x1, file_buffer
    strb    wzr, [x1, x0]

    // Close file
    mov     x0, x19
    mov     x16, #6                         // close() syscall
    svc     #0x80

    // Parse input
    bl      parse_input

    // Solve both parts
    bl      solve

    // Print Part 1 result
    LOAD_ADDR x0, part1_msg
    bl      print_str
    LOAD_ADDR x0, part1_result
    ldr     x0, [x0]
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Print Part 2 result
    LOAD_ADDR x0, part2_msg
    bl      print_str
    LOAD_ADDR x0, part2_result
    ldr     x0, [x0]
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Exit successfully
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
// parse_input: Parse patterns and designs from input
// Patterns are comma-separated on line 1, designs follow blank line
// ============================================================================
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x19, file_buffer              // Input pointer
    LOAD_ADDR x20, pattern_ptrs             // Pattern pointers array
    LOAD_ADDR x21, pattern_lens             // Pattern lengths array
    mov     x22, #0                         // Pattern count

    // Parse patterns (comma and space separated)
parse_patterns:
    // Skip leading whitespace
1:  ldrb    w0, [x19]
    cbz     w0, patterns_done
    cmp     w0, #' '
    b.eq    2f
    cmp     w0, #','
    b.eq    2f
    cmp     w0, #'\n'
    b.eq    patterns_done
    b       3f
2:  add     x19, x19, #1
    b       1b

3:  // Start of pattern - save pointer
    str     x19, [x20, x22, lsl #3]
    mov     x23, #0                         // Pattern length

    // Find end of pattern
4:  ldrb    w0, [x19]
    cbz     w0, save_pattern
    cmp     w0, #','
    b.eq    save_pattern
    cmp     w0, #' '
    b.eq    save_pattern
    cmp     w0, #'\n'
    b.eq    save_pattern
    add     x19, x19, #1
    add     x23, x23, #1
    b       4b

save_pattern:
    // Null-terminate the pattern in place
    // We'll mark the end by storing the length
    str     w23, [x21, x22, lsl #2]
    add     x22, x22, #1

    // Check if we hit newline (end of patterns line)
    cmp     w0, #'\n'
    b.eq    patterns_done
    b       parse_patterns

patterns_done:
    // Save pattern count
    LOAD_ADDR x0, num_patterns
    str     x22, [x0]

    // Skip blank line(s)
skip_blank:
    ldrb    w0, [x19]
    cbz     w0, parse_done
    cmp     w0, #'\n'
    b.ne    start_designs
    add     x19, x19, #1
    b       skip_blank

start_designs:
    // Parse designs
    LOAD_ADDR x20, design_ptrs
    LOAD_ADDR x21, design_lens
    mov     x22, #0                         // Design count

parse_designs:
    // Check for end of input
    ldrb    w0, [x19]
    cbz     w0, parse_done

    // Skip any leading newlines
    cmp     w0, #'\n'
    b.ne    5f
    add     x19, x19, #1
    b       parse_designs

5:  // Start of design - save pointer
    str     x19, [x20, x22, lsl #3]
    mov     x23, #0                         // Design length

    // Find end of design (newline or null)
6:  ldrb    w0, [x19]
    cbz     w0, save_design
    cmp     w0, #'\n'
    b.eq    save_design
    cmp     w0, #'\r'
    b.eq    save_design
    add     x19, x19, #1
    add     x23, x23, #1
    b       6b

save_design:
    // Only save if length > 0
    cbz     x23, skip_empty_design
    str     w23, [x21, x22, lsl #2]
    add     x22, x22, #1

skip_empty_design:
    // Move past newline if present
    ldrb    w0, [x19]
    cbz     w0, parse_done
    add     x19, x19, #1
    b       parse_designs

parse_done:
    // Save design count
    LOAD_ADDR x0, num_designs
    str     x22, [x0]

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// count_ways: Count ways to form a design from patterns
// Input: x0 = design pointer, x1 = design length
// Output: x0 = number of ways (0 if impossible)
// Uses bottom-up DP: dp[i] = ways to form design[i:]
// ============================================================================
count_ways:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0                         // design pointer
    mov     x20, x1                         // design length

    // Clear DP array
    LOAD_ADDR x21, dp_array
    mov     x0, x20
    add     x0, x0, #1                      // Need length + 1 slots
clear_dp:
    str     xzr, [x21, x0, lsl #3]
    subs    x0, x0, #1
    b.ge    clear_dp

    // Base case: dp[length] = 1 (empty suffix = one way)
    mov     x0, #1
    str     x0, [x21, x20, lsl #3]

    // Load pattern info
    LOAD_ADDR x22, pattern_ptrs
    LOAD_ADDR x23, pattern_lens
    LOAD_ADDR x0, num_patterns
    ldr     x24, [x0]

    // Fill DP from right to left: for pos = length-1 down to 0
    sub     x25, x20, #1                    // pos = length - 1

dp_outer_loop:
    cmp     x25, #0
    b.lt    dp_done

    // remaining = design_len - pos
    sub     x26, x20, x25

    // dp[pos] = 0 (already cleared)
    // Check each pattern
    mov     x27, #0                         // pattern index

dp_pattern_loop:
    cmp     x27, x24
    b.ge    dp_next_pos

    // Get pattern length
    ldr     w28, [x23, x27, lsl #2]

    // Skip if pattern too long
    cmp     x28, x26
    b.gt    dp_next_pattern

    // Get pattern pointer
    ldr     x0, [x22, x27, lsl #3]

    // Compare pattern with design[pos:pos+plen]
    add     x1, x19, x25                    // design + pos
    mov     x2, x28                         // pattern length
    bl      strncmp_impl

    // If match (result == 0), add dp[pos + plen] to dp[pos]
    cbnz    x0, dp_next_pattern

    // dp[pos] += dp[pos + plen]
    add     x0, x25, x28                    // pos + plen
    ldr     x1, [x21, x0, lsl #3]           // dp[pos + plen]
    ldr     x2, [x21, x25, lsl #3]          // dp[pos]
    add     x2, x2, x1
    str     x2, [x21, x25, lsl #3]

dp_next_pattern:
    add     x27, x27, #1
    b       dp_pattern_loop

dp_next_pos:
    sub     x25, x25, #1
    b       dp_outer_loop

dp_done:
    // Return dp[0]
    ldr     x0, [x21]

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// strncmp_impl: Compare n bytes of two strings
// Input: x0 = str1, x1 = str2, x2 = n
// Output: x0 = 0 if equal, non-zero otherwise
// ============================================================================
strncmp_impl:
    cbz     x2, strcmp_equal
strcmp_loop:
    ldrb    w3, [x0], #1
    ldrb    w4, [x1], #1
    subs    x2, x2, #1
    cbz     x2, strcmp_compare
    cmp     w3, w4
    b.ne    strcmp_diff
    b       strcmp_loop

strcmp_compare:
    sub     w0, w3, w4
    ret

strcmp_diff:
    sub     w0, w3, w4
    ret

strcmp_equal:
    mov     x0, #0
    ret

// ============================================================================
// solve: Solve both parts
// ============================================================================
solve:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x19, design_ptrs
    LOAD_ADDR x20, design_lens
    LOAD_ADDR x0, num_designs
    ldr     x21, [x0]

    mov     x22, #0                         // Part 1 count
    mov     x23, #0                         // Part 2 sum
    mov     x24, #0                         // design index

solve_loop:
    cmp     x24, x21
    b.ge    solve_done

    // Get design pointer and length
    ldr     x0, [x19, x24, lsl #3]
    ldr     w1, [x20, x24, lsl #2]

    // Count ways for this design
    bl      count_ways

    // Part 1: increment if ways > 0
    cbz     x0, no_ways
    add     x22, x22, #1

no_ways:
    // Part 2: add to total
    add     x23, x23, x0

    add     x24, x24, #1
    b       solve_loop

solve_done:
    // Store results
    LOAD_ADDR x0, part1_result
    str     x22, [x0]
    LOAD_ADDR x0, part2_result
    str     x23, [x0]

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print a null-terminated string
// Input: x0 = pointer to string
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
    mov     x16, #4                         // write() syscall
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print a 64-bit number
// Input: x0 = number
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
