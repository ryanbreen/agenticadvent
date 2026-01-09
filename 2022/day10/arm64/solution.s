// Day 10: Cathode-Ray Tube - ARM64 Assembly (macOS)
//
// Algorithm:
//   CPU has register X starting at 1
//   noop: 1 cycle, no effect
//   addx V: 2 cycles, then X += V
//   Part 1: Sum signal strengths (cycle * X) at cycles 20, 60, 100, 140, 180, 220
//   Part 2: Render 40x6 CRT. Each cycle draws pixel if sprite (3px wide at X) overlaps

.global _start
.align 4

// Constants
.equ BUFFER_SIZE, 16384         // Input file buffer size
.equ CRT_WIDTH, 40              // CRT width
.equ CRT_HEIGHT, 6              // CRT height
.equ CRT_SIZE, 240              // CRT_WIDTH * CRT_HEIGHT

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
part2_msg:      .asciz "Part 2:\n"
newline:        .asciz "\n"
error_msg:      .asciz "Error reading file\n"

.align 3
// File buffer for input
file_buffer:    .space BUFFER_SIZE
buffer_len:     .quad 0

// CRT display buffer (40x6 + newlines + null terminator)
// Each row is 40 chars + newline = 41 chars, 6 rows = 246 + null
.align 4
crt_buffer:     .space 256

// Global state
reg_x:          .quad 1         // X register
cycle_count:    .quad 0         // Current cycle
signal_sum:     .quad 0         // Part 1 sum

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
    mov     x2, #BUFFER_SIZE
    mov     x16, #3                         // read() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit

    // Save buffer length
    LOAD_ADDR x1, buffer_len
    str     x0, [x1]

    // Close file
    mov     x0, x19
    mov     x16, #6                         // close() syscall
    svc     #0x80

    // Initialize CRT buffer with dots
    LOAD_ADDR x0, crt_buffer
    mov     x1, #0
init_crt_loop:
    cmp     x1, #CRT_SIZE
    b.ge    init_crt_done

    // Calculate row position
    mov     x2, #CRT_WIDTH
    udiv    x3, x1, x2                      // row = i / 40
    msub    x4, x3, x2, x1                  // col = i % 40

    // Calculate buffer position (row * 41 + col for the char, or row * 41 + 40 for newline)
    mov     x5, #41
    mul     x5, x3, x5                      // row * 41
    add     x5, x5, x4                      // + col

    mov     w6, #'.'
    strb    w6, [x0, x5]

    // If this is end of row (col == 39), add newline
    cmp     x4, #39
    b.ne    init_crt_next
    add     x5, x5, #1
    mov     w6, #'\n'
    strb    w6, [x0, x5]

init_crt_next:
    add     x1, x1, #1
    b       init_crt_loop

init_crt_done:
    // Null terminate after 6 rows (6 * 41 = 246)
    mov     x1, #246
    strb    wzr, [x0, x1]

    // Initialize state
    LOAD_ADDR x0, reg_x
    mov     x1, #1
    str     x1, [x0]

    LOAD_ADDR x0, cycle_count
    str     xzr, [x0]

    LOAD_ADDR x0, signal_sum
    str     xzr, [x0]

    // Process instructions
    bl      process_instructions

    // Print Part 1
    LOAD_ADDR x0, part1_msg
    bl      print_str

    LOAD_ADDR x0, signal_sum
    ldr     x0, [x0]
    bl      print_num

    LOAD_ADDR x0, newline
    bl      print_str

    // Print Part 2
    LOAD_ADDR x0, part2_msg
    bl      print_str

    LOAD_ADDR x0, crt_buffer
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
// process_instructions: Parse and execute all instructions
// ============================================================================
process_instructions:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    // x19 = current position in buffer
    // x20 = end of buffer
    LOAD_ADDR x19, file_buffer
    LOAD_ADDR x0, buffer_len
    ldr     x20, [x0]
    add     x20, x19, x20

parse_loop:
    cmp     x19, x20
    b.ge    parse_done

    // Skip whitespace and newlines at start
    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.eq    skip_char
    cmp     w0, #'\r'
    b.eq    skip_char
    cmp     w0, #' '
    b.eq    skip_char
    cmp     w0, #0
    b.eq    parse_done

    // Check for 'n' (noop) or 'a' (addx)
    cmp     w0, #'n'
    b.eq    handle_noop
    cmp     w0, #'a'
    b.eq    handle_addx

    // Unknown instruction, skip to newline
    b       skip_to_newline

handle_noop:
    // Skip "noop" (4 chars)
    add     x19, x19, #4

    // Execute 1 cycle
    bl      execute_cycle

    b       parse_loop

handle_addx:
    // Skip "addx " (5 chars)
    add     x19, x19, #5

    // Parse the value (can be negative)
    mov     x21, #0                         // value
    mov     x22, #1                         // sign (1 = positive, -1 = negative)

    // Check for negative sign
    ldrb    w0, [x19]
    cmp     w0, #'-'
    b.ne    parse_value
    mov     x22, #-1
    add     x19, x19, #1

parse_value:
    cmp     x19, x20
    b.ge    have_value
    ldrb    w0, [x19]
    cmp     w0, #'0'
    b.lt    have_value
    cmp     w0, #'9'
    b.gt    have_value

    sub     w0, w0, #'0'
    and     x0, x0, #0xFF                   // Zero-extend to 64 bits
    mov     x1, #10
    mul     x21, x21, x1
    add     x21, x21, x0
    add     x19, x19, #1
    b       parse_value

have_value:
    // Apply sign
    mul     x21, x21, x22                   // x21 = final value

    // Execute cycle 1 of addx
    bl      execute_cycle

    // Execute cycle 2 of addx
    bl      execute_cycle

    // After both cycles, update X register
    LOAD_ADDR x0, reg_x
    ldr     x1, [x0]
    add     x1, x1, x21
    str     x1, [x0]

    b       parse_loop

skip_char:
    add     x19, x19, #1
    b       parse_loop

skip_to_newline:
    cmp     x19, x20
    b.ge    parse_done
    ldrb    w0, [x19]
    add     x19, x19, #1
    cmp     w0, #'\n'
    b.ne    skip_to_newline
    b       parse_loop

parse_done:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// execute_cycle: Execute one CPU cycle
// - Increments cycle counter
// - Checks if we're at a signal strength cycle (20, 60, 100, 140, 180, 220)
// - Draws pixel to CRT
// ============================================================================
execute_cycle:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    // Increment cycle
    LOAD_ADDR x0, cycle_count
    ldr     x19, [x0]
    add     x19, x19, #1
    str     x19, [x0]                       // x19 = new cycle number

    // Load X register value
    LOAD_ADDR x0, reg_x
    ldr     x20, [x0]                       // x20 = X register value

    // ===== Part 1: Check signal strength =====
    // Check if cycle is 20, 60, 100, 140, 180, or 220
    // These are 20 + 40*k for k = 0,1,2,3,4,5
    // So check if (cycle - 20) % 40 == 0 and cycle <= 220

    cmp     x19, #20
    b.lt    skip_signal
    cmp     x19, #220
    b.gt    skip_signal

    sub     x1, x19, #20                    // cycle - 20
    mov     x2, #40
    udiv    x3, x1, x2
    msub    x4, x3, x2, x1                  // (cycle - 20) % 40
    cbnz    x4, skip_signal

    // This is a target cycle, add signal strength
    mul     x1, x19, x20                    // cycle * X
    LOAD_ADDR x2, signal_sum
    ldr     x3, [x2]
    add     x3, x3, x1
    str     x3, [x2]

skip_signal:
    // ===== Part 2: Draw CRT pixel =====
    // CRT position = (cycle - 1) % 240
    // Row position = crt_pos / 40
    // Column position = crt_pos % 40
    // Draw '#' if abs(col_pos - X) <= 1

    sub     x1, x19, #1                     // cycle - 1
    mov     x2, #CRT_SIZE
    udiv    x3, x1, x2
    msub    x1, x3, x2, x1                  // crt_pos = (cycle - 1) % 240

    mov     x2, #CRT_WIDTH
    udiv    x3, x1, x2                      // row = crt_pos / 40
    msub    x4, x3, x2, x1                  // col = crt_pos % 40 (x4 = column position)

    // Check if sprite overlaps: abs(col - X) <= 1
    // Sprite is 3 pixels wide centered at X
    sub     x5, x4, x20                     // col - X

    // abs(x5)
    cmp     x5, #0
    cneg    x5, x5, lt                      // x5 = abs(col - X)

    cmp     x5, #1
    b.gt    cycle_done                      // No overlap, pixel stays '.'

    // Draw '#' at this position
    // Buffer position = row * 41 + col (41 because of newlines)
    mov     x6, #41
    mul     x6, x3, x6                      // row * 41
    add     x6, x6, x4                      // + col

    LOAD_ADDR x7, crt_buffer
    mov     w8, #'#'
    strb    w8, [x7, x6]

cycle_done:
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
// print_num: Print a number (handles positive numbers)
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
