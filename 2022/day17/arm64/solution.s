// Day 17: Pyroclastic Flow - ARM64 Assembly (macOS)
//
// Tetris-like simulation of falling rocks in a 7-wide chamber.
// - Five rock shapes cycle in order
// - Jets push rocks left/right, then rocks fall
// - Part 1: Simulate 2022 rocks and report tower height
// - Part 2: Simulate 1 trillion rocks using cycle detection
//
// Algorithm:
// 1. Parse jet pattern (< and > characters)
// 2. For each rock: start at x=2, y=height+3
// 3. Alternate: jet push (if valid), then fall (if valid)
// 4. When rock can't fall, it stops and update grid
// 5. Part 2: Track state (rock_type, jet_idx, surface_profile) to detect cycles

.global _start
.align 4

// Constants
.equ BUFFER_SIZE, 16384         // Input file buffer
.equ GRID_WIDTH, 7              // Chamber width
.equ GRID_HEIGHT, 8192          // Max tower height to track (much more than needed for cycle)
.equ MAX_JETS, 11000            // Max jet pattern length
.equ PROFILE_DEPTH, 50          // Depth of surface profile for cycle detection
.equ HASH_SIZE, 262144          // Hash table size (power of 2)

// Rock definitions: each rock is defined by offsets from bottom-left
// Rock 0: #### (horizontal line)
// Rock 1: .#. / ### / .#. (plus)
// Rock 2: ..# / ..# / ### (L-shape, bottom-left is origin)
// Rock 3: # / # / # / # (vertical line)
// Rock 4: ## / ## (square)

.macro LOAD_ADDR reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// ============================================================================
// Data Section
// ============================================================================
.data

input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

.align 3
file_buffer:    .space BUFFER_SIZE
jet_pattern:    .space MAX_JETS          // Store as 1 (right) or -1 (left) bytes
jet_len:        .quad 0
jet_idx:        .quad 0

// Grid: each row is 7 bits packed into a byte (bit 0 = col 0, etc.)
// Row 0 is the floor level
.align 4
grid:           .space GRID_HEIGHT

// Current tower height (number of rows with rocks)
tower_height:   .quad 0

// Rock shapes: each rock has up to 5 cells, stored as (dx, dy) pairs
// Format: count, then count pairs of (dx, dy)
.align 2
rock0:  .byte 4,  0,0, 1,0, 2,0, 3,0, 0,0      // ####
rock1:  .byte 5,  1,0, 0,1, 1,1, 2,1, 1,2      // plus
rock2:  .byte 5,  0,0, 1,0, 2,0, 2,1, 2,2      // L-shape
rock3:  .byte 4,  0,0, 0,1, 0,2, 0,3, 0,0      // vertical
rock4:  .byte 4,  0,0, 1,0, 0,1, 1,1, 0,0      // square

// Rock widths and heights for quick bounds checking
rock_widths:    .byte 4, 3, 3, 1, 2
rock_heights:   .byte 1, 3, 3, 4, 2

// For cycle detection in Part 2
.align 3
// Hash table: stores (rock_num, height) pairs for each state
// Format: valid flag (8 bytes), rock_num (8 bytes), height (8 bytes), jet_idx (8 bytes), rock_type (8 bytes), profile hash (8 bytes)
hash_table:     .space HASH_SIZE * 48     // 6 * 8 bytes per entry
heights_log:    .space 8192 * 8           // Log heights for first 8192 rocks

// ============================================================================
// Code Section
// ============================================================================
.text

// ============================================================================
// Main entry point
// ============================================================================
_start:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

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
    mov     x2, #BUFFER_SIZE
    mov     x16, #3                         // read() syscall
    svc     #0x80
    cmp     x0, #0
    b.le    error_exit
    mov     x20, x0                         // Save bytes read

    // Close file
    mov     x0, x19
    mov     x16, #6                         // close() syscall
    svc     #0x80

    // Parse jet pattern
    mov     x0, x20
    bl      parse_jets

    // Clear grid
    bl      clear_grid

    // Part 1: Simulate 2022 rocks
    mov     x0, #2022
    bl      simulate_rocks
    mov     x21, x0                         // Save part1 result

    // Print Part 1
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x21
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Clear grid and reset for Part 2
    bl      clear_grid
    LOAD_ADDR x0, jet_idx
    str     xzr, [x0]
    LOAD_ADDR x0, tower_height
    str     xzr, [x0]
    bl      clear_hash_table

    // Part 2: Simulate 1 trillion rocks with cycle detection
    // 1000000000000 = 0xE8D4A51000
    mov     x0, #0x1000            // bits 0-15
    movk    x0, #0xD4A5, lsl #16   // bits 16-31
    movk    x0, #0x00E8, lsl #32   // bits 32-47, now x0 = 0xE8D4A51000
    bl      simulate_rocks_with_cycle
    mov     x22, x0                         // Save part2 result

    // Print Part 2
    LOAD_ADDR x0, part2_msg
    bl      print_str
    mov     x0, x22
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Exit
    mov     x0, #0
    mov     x16, #1
    svc     #0x80

error_exit:
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// ============================================================================
// parse_jets: Parse jet pattern from input buffer
// Input: x0 = buffer length
// ============================================================================
parse_jets:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    LOAD_ADDR x19, file_buffer
    LOAD_ADDR x20, jet_pattern
    mov     x2, #0                          // Count

parse_jets_loop:
    ldrb    w1, [x19], #1
    cmp     w1, #'<'
    b.eq    jet_left
    cmp     w1, #'>'
    b.eq    jet_right
    b       parse_jets_done                 // End on newline or other char

jet_left:
    mov     w3, #-1
    strb    w3, [x20, x2]
    add     x2, x2, #1
    b       parse_jets_loop

jet_right:
    mov     w3, #1
    strb    w3, [x20, x2]
    add     x2, x2, #1
    b       parse_jets_loop

parse_jets_done:
    LOAD_ADDR x0, jet_len
    str     x2, [x0]

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// clear_grid: Clear the grid
// ============================================================================
clear_grid:
    LOAD_ADDR x0, grid
    mov     x1, #GRID_HEIGHT
clear_grid_loop:
    cbz     x1, clear_grid_done
    strb    wzr, [x0], #1
    sub     x1, x1, #1
    b       clear_grid_loop
clear_grid_done:
    LOAD_ADDR x0, tower_height
    str     xzr, [x0]
    ret

// ============================================================================
// clear_hash_table: Clear the hash table
// ============================================================================
clear_hash_table:
    LOAD_ADDR x0, hash_table
    mov     x1, #HASH_SIZE
    mov     x2, #48                         // Entry size
    mul     x1, x1, x2
clear_hash_loop:
    cbz     x1, clear_hash_done
    str     xzr, [x0], #8
    sub     x1, x1, #8
    b       clear_hash_loop
clear_hash_done:
    ret

// ============================================================================
// get_rock_ptr: Get pointer to rock data
// Input: x0 = rock type (0-4)
// Output: x0 = pointer to rock data
// ============================================================================
get_rock_ptr:
    LOAD_ADDR x1, rock0
    cmp     x0, #0
    b.eq    rock_ptr_done
    LOAD_ADDR x1, rock1
    cmp     x0, #1
    b.eq    rock_ptr_done
    LOAD_ADDR x1, rock2
    cmp     x0, #2
    b.eq    rock_ptr_done
    LOAD_ADDR x1, rock3
    cmp     x0, #3
    b.eq    rock_ptr_done
    LOAD_ADDR x1, rock4
rock_ptr_done:
    mov     x0, x1
    ret

// ============================================================================
// can_place_rock: Check if rock can be placed at position
// Input: x0 = rock type, x1 = x position, x2 = y position
// Output: x0 = 1 if can place, 0 if collision
// ============================================================================
can_place_rock:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    mov     x19, x0                         // rock type
    mov     x20, x1                         // x
    mov     x21, x2                         // y

    // Get rock pointer
    mov     x0, x19
    bl      get_rock_ptr
    mov     x22, x0                         // rock ptr

    // Get number of cells
    ldrb    w23, [x22], #1                  // count, advance ptr

    LOAD_ADDR x24, grid

check_cells:
    cbz     x23, can_place_yes

    // Load dx, dy
    ldrsb   w0, [x22], #1                   // dx
    ldrsb   w1, [x22], #1                   // dy

    // Calculate actual position
    add     x0, x20, x0                     // x + dx
    add     x1, x21, x1                     // y + dy

    // Check bounds
    cmp     x0, #0
    b.lt    can_place_no
    cmp     x0, #GRID_WIDTH
    b.ge    can_place_no
    cmp     x1, #0
    b.lt    can_place_no

    // Check grid collision (only if y < GRID_HEIGHT)
    cmp     x1, #GRID_HEIGHT
    b.ge    next_cell                       // Above grid tracking, assume empty

    ldrb    w2, [x24, x1]                   // Get row
    mov     w3, #1
    lsl     w3, w3, w0                      // Bit for column
    tst     w2, w3
    b.ne    can_place_no

next_cell:
    sub     x23, x23, #1
    b       check_cells

can_place_yes:
    mov     x0, #1
    b       can_place_done

can_place_no:
    mov     x0, #0

can_place_done:
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// place_rock: Place rock on grid and update height
// Input: x0 = rock type, x1 = x position, x2 = y position
// ============================================================================
place_rock:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0                         // rock type
    mov     x20, x1                         // x
    mov     x21, x2                         // y

    // Get rock pointer
    mov     x0, x19
    bl      get_rock_ptr
    mov     x22, x0

    // Get number of cells
    ldrb    w23, [x22], #1

    LOAD_ADDR x24, grid
    LOAD_ADDR x25, tower_height
    ldr     x26, [x25]                      // Current height

place_cells:
    cbz     x23, place_done

    ldrsb   w0, [x22], #1                   // dx
    ldrsb   w1, [x22], #1                   // dy

    add     x0, x20, x0                     // x + dx
    add     x1, x21, x1                     // y + dy

    // Only place if within grid tracking
    cmp     x1, #GRID_HEIGHT
    b.ge    skip_place_cell

    // Set bit in grid
    ldrb    w2, [x24, x1]
    mov     w3, #1
    lsl     w3, w3, w0
    orr     w2, w2, w3
    strb    w2, [x24, x1]

    // Update height
    add     x1, x1, #1                      // y + 1 = new potential height
    cmp     x1, x26
    csel    x26, x1, x26, gt

skip_place_cell:
    sub     x23, x23, #1
    b       place_cells

place_done:
    str     x26, [x25]                      // Store new height

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// simulate_one_rock: Simulate one rock falling
// Input: x0 = rock type
// ============================================================================
simulate_one_rock:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0                         // rock type

    // Initial position: x=2, y=height+3
    mov     x20, #2                         // x
    LOAD_ADDR x0, tower_height
    ldr     x21, [x0]
    add     x21, x21, #3                    // y = height + 3

    // Load jet state
    LOAD_ADDR x22, jet_pattern
    LOAD_ADDR x23, jet_idx
    ldr     x24, [x23]                      // Current jet index
    LOAD_ADDR x0, jet_len
    ldr     x25, [x0]                       // jet_len in callee-saved register

rock_fall_loop:
    // Apply jet push
    ldrsb   w0, [x22, x24]                  // Get jet direction (-1 or 1)
    sxtb    x0, w0
    add     x26, x20, x0                    // new_x = x + jet

    // Advance jet index
    add     x24, x24, #1
    cmp     x24, x25
    csel    x24, xzr, x24, ge               // jet_idx = (jet_idx + 1) % jet_len

    // Check if can move horizontally
    mov     x0, x19
    mov     x1, x26
    mov     x2, x21
    bl      can_place_rock
    cbz     x0, skip_horizontal
    mov     x20, x26                        // Accept horizontal move

skip_horizontal:
    // Try to move down
    sub     x26, x21, #1                    // new_y = y - 1

    mov     x0, x19
    mov     x1, x20
    mov     x2, x26
    bl      can_place_rock

    cbz     x0, rock_stopped
    mov     x21, x26                        // Accept downward move
    b       rock_fall_loop

rock_stopped:
    // Save jet index
    str     x24, [x23]

    // Place rock on grid
    mov     x0, x19
    mov     x1, x20
    mov     x2, x21
    bl      place_rock

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// simulate_rocks: Simulate n rocks (Part 1)
// Input: x0 = number of rocks
// Output: x0 = tower height
// ============================================================================
simulate_rocks:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0                         // Rock count
    mov     x20, #0                         // Current rock number

sim_loop:
    cmp     x20, x19
    b.ge    sim_done

    // Get rock type
    mov     x0, #5
    udiv    x1, x20, x0
    msub    x0, x1, x0, x20                 // rock_type = rock_num % 5

    bl      simulate_one_rock

    add     x20, x20, #1
    b       sim_loop

sim_done:
    LOAD_ADDR x0, tower_height
    ldr     x0, [x0]

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// compute_profile_hash: Compute hash of surface profile
// Output: x0 = hash value
// ============================================================================
compute_profile_hash:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, grid
    LOAD_ADDR x0, tower_height
    ldr     x20, [x0]                       // height

    mov     x21, #0                         // hash
    mov     x22, #PROFILE_DEPTH             // depth to hash

    // Start from top row and go down
profile_hash_loop:
    cbz     x22, profile_hash_done
    cbz     x20, profile_hash_done

    sub     x20, x20, #1
    ldrb    w0, [x19, x20]                  // Get row

    // Simple hash: rotate and xor
    lsl     x1, x21, #7
    lsr     x2, x21, #57
    orr     x21, x1, x2
    eor     x21, x21, x0

    sub     x22, x22, #1
    b       profile_hash_loop

profile_hash_done:
    mov     x0, x21

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// simulate_rocks_with_cycle: Simulate n rocks with cycle detection (Part 2)
// Input: x0 = number of rocks (can be very large)
// Output: x0 = tower height
// ============================================================================
simulate_rocks_with_cycle:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0                         // Total rocks needed
    mov     x20, #0                         // Current rock number
    mov     x27, #0                         // Height offset from cycles

sim_cycle_loop:
    cmp     x20, x19
    b.ge    sim_cycle_done

    // Get rock type
    mov     x0, #5
    udiv    x1, x20, x0
    msub    x21, x1, x0, x20                // rock_type = rock_num % 5

    // Simulate this rock
    mov     x0, x21
    bl      simulate_one_rock

    // Get current jet index AFTER simulating (matches Python)
    LOAD_ADDR x0, jet_idx
    ldr     x22, [x0]                       // jet_idx after rock placed

    // Get current height
    LOAD_ADDR x0, tower_height
    ldr     x23, [x0]                       // height after rock placed

    // Log height for cycle detection (only first 8192)
    cmp     x20, #8192
    b.ge    skip_height_log
    LOAD_ADDR x0, heights_log
    str     x23, [x0, x20, lsl #3]

skip_height_log:
    // Only start cycle detection after some rocks have fallen
    cmp     x20, #100
    b.lt    next_rock_cycle

    // Compute profile hash
    bl      compute_profile_hash
    mov     x24, x0                         // profile_hash

    // Compute state hash: combine rock_type, jet_idx, profile_hash
    mov     x0, x21
    lsl     x1, x22, #3
    eor     x0, x0, x1
    lsl     x1, x24, #16
    eor     x0, x0, x1
    mov     x1, #(HASH_SIZE - 1)
    and     x25, x0, x1                     // hash table index

    // Look up in hash table
    LOAD_ADDR x26, hash_table
    mov     x0, #48
    mul     x0, x25, x0
    add     x26, x26, x0                    // Entry pointer

    // Check if slot is valid and matches
    ldr     x0, [x26]                       // valid flag
    cbz     x0, store_state

    // Check if state matches
    ldr     x1, [x26, #24]                  // stored jet_idx
    cmp     x1, x22
    b.ne    store_state
    ldr     x1, [x26, #32]                  // stored rock_type
    cmp     x1, x21
    b.ne    store_state
    ldr     x1, [x26, #40]                  // stored profile_hash
    cmp     x1, x24
    b.ne    store_state

    // Found a cycle!
    ldr     x0, [x26, #8]                   // cycle_start_rock
    ldr     x1, [x26, #16]                  // cycle_start_height

    // cycle_len = current_rock - cycle_start_rock
    sub     x2, x20, x0                     // cycle_len
    // cycle_height = current_height - cycle_start_height
    sub     x3, x23, x1                     // cycle_height

    // remaining = total_rocks - current_rock - 1
    sub     x4, x19, x20
    sub     x4, x4, #1                      // remaining

    // full_cycles = remaining / cycle_len
    udiv    x5, x4, x2                      // full_cycles

    // leftover = remaining % cycle_len
    msub    x6, x5, x2, x4                  // leftover

    // final_height = current_height + full_cycles * cycle_height
    mul     x7, x5, x3
    add     x27, x23, x7                    // base height with full cycles

    // Add leftover height: heights[cycle_start + leftover] - heights[cycle_start]
    cbz     x6, no_leftover                 // if leftover == 0, skip

    // Look up heights[cycle_start + leftover]
    add     x7, x0, x6                      // cycle_start + leftover
    LOAD_ADDR x8, heights_log
    ldr     x9, [x8, x7, lsl #3]            // heights[cycle_start + leftover]
    // heights[cycle_start] = x1 (cycle_start_height)
    sub     x9, x9, x1                      // delta height
    add     x27, x27, x9

no_leftover:
    // Return the final height directly
    mov     x0, x27
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

store_state:
    // Store current state
    mov     x0, #1
    str     x0, [x26]                       // valid = 1
    str     x20, [x26, #8]                  // rock_num
    str     x23, [x26, #16]                 // height
    str     x22, [x26, #24]                 // jet_idx
    str     x21, [x26, #32]                 // rock_type
    str     x24, [x26, #40]                 // profile_hash

next_rock_cycle:
    add     x20, x20, #1
    b       sim_cycle_loop

sim_cycle_done:
    // Final height = tower_height + height_offset
    LOAD_ADDR x0, tower_height
    ldr     x0, [x0]
    add     x0, x0, x27

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print null-terminated string
// ============================================================================
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    mov     x20, #0
1:  ldrb    w1, [x19, x20]
    cbz     w1, 2f
    add     x20, x20, #1
    b       1b

2:  mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print 64-bit unsigned number
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

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
