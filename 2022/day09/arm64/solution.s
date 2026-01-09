// Day 9: Rope Bridge - ARM64 Assembly (macOS)
//
// Algorithm:
//   Parse moves (direction + count)
//   Simulate rope physics: head moves, each knot follows the one before it
//   If a knot is more than 1 step away from its leader, move toward it using sign(delta)
//   Part 1: rope length 2, count unique tail positions
//   Part 2: rope length 10, count unique tail positions
//
// Hash set is used to track unique (x, y) positions for the tail
// Coordinates can be negative, so we encode them as (x+500)*1001 + (y+500) for hashing

.global _start
.align 4

// Constants
.equ BUFFER_SIZE, 16384         // Input file buffer size
.equ HASH_SIZE, 131072          // Hash table size (power of 2 for faster modulo)
.equ HASH_MASK, 131071          // HASH_SIZE - 1
.equ MAX_ROPE_LEN, 10           // Maximum rope length
.equ COORD_OFFSET, 500          // Offset to make coordinates positive

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
// File buffer for input
file_buffer:    .space BUFFER_SIZE
buffer_len:     .quad 0

// Rope positions: 10 knots, each with (x, y) as 64-bit signed integers
// Layout: knot[i].x at offset i*16, knot[i].y at offset i*16+8
.align 4
rope:           .space MAX_ROPE_LEN * 16

// Hash table for visited positions
// Each entry is 8 bytes: 0 = empty, non-zero = encoded position + 1
.align 4
hash_table:     .space HASH_SIZE * 8

// Counter for unique positions
unique_count:   .quad 0

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

    // ========== Part 1: Rope length 2 ==========
    mov     x0, #2
    bl      simulate_rope
    mov     x21, x0                         // Save part1 result

    // ========== Part 2: Rope length 10 ==========
    mov     x0, #10
    bl      simulate_rope
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
// simulate_rope: Simulate rope with given length
// Input: x0 = rope length
// Returns: x0 = number of unique tail positions
// ============================================================================
simulate_rope:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0                         // x19 = rope length

    // Initialize rope positions to (0, 0)
    LOAD_ADDR x0, rope
    mov     x1, #0
init_rope_loop:
    cmp     x1, #MAX_ROPE_LEN
    b.ge    init_rope_done
    lsl     x2, x1, #4                      // offset = i * 16
    str     xzr, [x0, x2]                   // knot[i].x = 0
    add     x2, x2, #8
    str     xzr, [x0, x2]                   // knot[i].y = 0
    add     x1, x1, #1
    b       init_rope_loop
init_rope_done:

    // Clear hash table
    LOAD_ADDR x0, hash_table
    mov     x1, #0
    mov     x2, #HASH_SIZE
clear_hash_loop:
    cmp     x1, x2
    b.ge    clear_hash_done
    str     xzr, [x0, x1, lsl #3]
    add     x1, x1, #1
    b       clear_hash_loop
clear_hash_done:

    // Reset unique count
    LOAD_ADDR x0, unique_count
    str     xzr, [x0]

    // Mark initial position (0, 0) as visited
    mov     x0, #0                          // x = 0
    mov     x1, #0                          // y = 0
    bl      add_visited

    // Parse and process moves
    LOAD_ADDR x20, file_buffer              // x20 = current position in buffer
    LOAD_ADDR x0, buffer_len
    ldr     x21, [x0]
    add     x21, x20, x21                   // x21 = end of buffer

parse_loop:
    cmp     x20, x21
    b.ge    simulate_done

    // Skip whitespace
    ldrb    w0, [x20]
    cmp     w0, #'\n'
    b.eq    skip_newline
    cmp     w0, #' '
    b.eq    skip_space
    cmp     w0, #'\r'
    b.eq    skip_space

    // Parse direction (R, L, U, D)
    ldrb    w22, [x20], #1                  // w22 = direction char

    // Skip space after direction
    ldrb    w0, [x20]
    cmp     w0, #' '
    b.ne    parse_count
    add     x20, x20, #1

parse_count:
    // Parse count (number)
    mov     x23, #0                         // x23 = count
parse_count_loop:
    cmp     x20, x21
    b.ge    have_move
    ldrb    w0, [x20]
    cmp     w0, #'0'
    b.lt    have_move
    cmp     w0, #'9'
    b.gt    have_move
    sub     w0, w0, #'0'
    mov     x1, #10
    mul     x23, x23, x1
    and     x0, x0, #0xFF                   // Zero-extend w0 to x0
    add     x23, x23, x0
    add     x20, x20, #1
    b       parse_count_loop

have_move:
    // Determine direction deltas
    // x24 = dx, x25 = dy
    mov     x24, #0
    mov     x25, #0

    cmp     w22, #'R'
    b.ne    check_left
    mov     x24, #1
    b       execute_move
check_left:
    cmp     w22, #'L'
    b.ne    check_up
    mov     x24, #-1
    b       execute_move
check_up:
    cmp     w22, #'U'
    b.ne    check_down
    mov     x25, #1
    b       execute_move
check_down:
    cmp     w22, #'D'
    b.ne    parse_loop                      // Unknown direction, skip
    mov     x25, #-1

execute_move:
    // Execute count steps
move_step_loop:
    cbz     x23, parse_loop                 // Done with this move

    // Move head
    LOAD_ADDR x26, rope                     // x26 = rope base
    ldr     x0, [x26]                       // head.x
    ldr     x1, [x26, #8]                   // head.y
    add     x0, x0, x24                     // head.x += dx
    add     x1, x1, x25                     // head.y += dy
    str     x0, [x26]
    str     x1, [x26, #8]

    // Update each following knot
    mov     x27, #1                         // knot index
update_knots_loop:
    cmp     x27, x19
    b.ge    knots_done

    // Get leader position (knot[i-1])
    sub     x0, x27, #1
    lsl     x0, x0, #4
    ldr     x1, [x26, x0]                   // leader.x
    add     x0, x0, #8
    ldr     x2, [x26, x0]                   // leader.y

    // Get current knot position (knot[i])
    lsl     x0, x27, #4
    ldr     x3, [x26, x0]                   // current.x
    add     x4, x0, #8
    ldr     x5, [x26, x4]                   // current.y

    // Calculate delta
    sub     x6, x1, x3                      // dx = leader.x - current.x
    sub     x7, x2, x5                      // dy = leader.y - current.y

    // Check if adjacent (abs(dx) <= 1 && abs(dy) <= 1)
    // Get absolute values
    cmp     x6, #0
    cneg    x8, x6, lt                      // abs(dx)
    cmp     x7, #0
    cneg    x9, x7, lt                      // abs(dy)

    cmp     x8, #1
    b.gt    need_move
    cmp     x9, #1
    b.le    next_knot                       // Adjacent, no move needed

need_move:
    // Move toward leader using sign(delta)
    // new.x = current.x + sign(dx)
    // new.y = current.y + sign(dy)

    // sign(dx)
    cmp     x6, #0
    mov     x8, #0
    b.eq    sign_dx_done
    mov     x8, #1
    b.gt    sign_dx_done
    mov     x8, #-1
sign_dx_done:

    // sign(dy)
    cmp     x7, #0
    mov     x9, #0
    b.eq    sign_dy_done
    mov     x9, #1
    b.gt    sign_dy_done
    mov     x9, #-1
sign_dy_done:

    // Update position
    add     x3, x3, x8
    add     x5, x5, x9
    str     x3, [x26, x0]
    str     x5, [x26, x4]

next_knot:
    add     x27, x27, #1
    b       update_knots_loop

knots_done:
    // Mark tail position as visited
    // tail is knot[rope_length - 1]
    sub     x0, x19, #1
    lsl     x0, x0, #4
    ldr     x0, [x26, x0]                   // tail.x
    sub     x1, x19, #1
    lsl     x1, x1, #4
    add     x1, x1, #8
    ldr     x1, [x26, x1]                   // tail.y
    bl      add_visited

    sub     x23, x23, #1
    b       move_step_loop

skip_newline:
skip_space:
    add     x20, x20, #1
    b       parse_loop

simulate_done:
    // Return unique count
    LOAD_ADDR x0, unique_count
    ldr     x0, [x0]

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// add_visited: Add position to visited set (if not already present)
// Input: x0 = x coordinate (can be negative)
//        x1 = y coordinate (can be negative)
// Clobbers: x0-x9
// ============================================================================
add_visited:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    // Encode position as (x + 500) * 1001 + (y + 500)
    // This creates a unique value for each (x, y) pair in reasonable range
    add     x2, x0, #COORD_OFFSET           // x + 500
    add     x3, x1, #COORD_OFFSET           // y + 500
    mov     x4, #1001
    mul     x2, x2, x4
    add     x2, x2, x3                      // encoded = (x+500)*1001 + (y+500)
    add     x19, x2, #1                     // stored value = encoded + 1 (0 means empty)

    // Hash: encoded & HASH_MASK
    and     x3, x2, #HASH_MASK              // hash index

    LOAD_ADDR x4, hash_table

    // Linear probing to find slot
probe_loop:
    ldr     x5, [x4, x3, lsl #3]            // Load entry
    cbz     x5, insert_new                  // Empty slot, insert
    cmp     x5, x19
    b.eq    already_visited                 // Already in set

    // Probe next slot
    add     x3, x3, #1
    and     x3, x3, #HASH_MASK
    b       probe_loop

insert_new:
    // Insert into hash table
    str     x19, [x4, x3, lsl #3]

    // Increment unique count
    LOAD_ADDR x0, unique_count
    ldr     x1, [x0]
    add     x1, x1, #1
    str     x1, [x0]

already_visited:
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
