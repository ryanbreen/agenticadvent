// ARM64 Assembly - Day 23: Unstable Diffusion
// Simulates elves spreading out on a grid
// Part 1: Count empty tiles in bounding box after 10 rounds
// Part 2: Find first round where no elf moves

.global _start
.align 4

// Constants
.equ MAX_ELVES, 3000
.equ HASH_SIZE, 8192      // Must be power of 2
.equ HASH_MASK, 8191
// EMPTY value: 0x80000000 (minimum signed 32-bit)
// Use this as sentinel since coordinates are never this extreme

// Syscall numbers (macOS)
.equ SYS_EXIT, 1
.equ SYS_READ, 3
.equ SYS_WRITE, 4
.equ SYS_OPEN, 5
.equ SYS_CLOSE, 6

.equ O_RDONLY, 0
.equ STDIN, 0
.equ STDOUT, 1

// Direction deltas: N, S, W, E (row, col pairs)
// N = (-1, 0), S = (1, 0), W = (0, -1), E = (0, 1)

.section __DATA,__data
.align 4

input_path: .asciz "../input.txt"

part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

// Direction check deltas for each direction
// N: check NW(-1,-1), N(-1,0), NE(-1,1)
// S: check SW(1,-1), S(1,0), SE(1,1)
// W: check NW(-1,-1), W(0,-1), SW(1,-1)
// E: check NE(-1,1), E(0,1), SE(1,1)

// N checks: (-1,-1), (-1,0), (-1,1), move: (-1,0)
// S checks: (1,-1), (1,0), (1,1), move: (1,0)
// W checks: (-1,-1), (0,-1), (1,-1), move: (0,-1)
// E checks: (-1,1), (0,1), (1,1), move: (0,1)

// All 8 neighbors: NW, N, NE, W, E, SW, S, SE
all_neighbors:
    .word -1, -1    // NW
    .word -1, 0     // N
    .word -1, 1     // NE
    .word 0, -1     // W
    .word 0, 1      // E
    .word 1, -1     // SW
    .word 1, 0      // S
    .word 1, 1      // SE

// Direction check positions (3 per direction)
dir_checks:
    // N: NW, N, NE
    .word -1, -1, -1, 0, -1, 1
    // S: SW, S, SE
    .word 1, -1, 1, 0, 1, 1
    // W: NW, W, SW
    .word -1, -1, 0, -1, 1, -1
    // E: NE, E, SE
    .word -1, 1, 0, 1, 1, 1

// Direction moves (dr, dc per direction: N, S, W, E)
dir_moves:
    .word -1, 0     // N
    .word 1, 0      // S
    .word 0, -1     // W
    .word 0, 1      // E

.section __DATA,__bss
.align 4

// Input buffer
input_buf: .space 65536

// Elf positions (row, col pairs)
elves: .space MAX_ELVES * 8
elf_count: .space 8

// New elf positions for next round
new_elves: .space MAX_ELVES * 8

// Hash set for current elf positions
// Each entry: row (4 bytes), col (4 bytes), or EMPTY if unused
hash_set: .space HASH_SIZE * 8

// Proposals: for each elf, store proposed position (row, col)
// If no proposal, store (EMPTY, EMPTY)
proposals: .space MAX_ELVES * 8

// Proposal count hash map: position -> count
// Each entry: row, col, count (12 bytes each, but use 16 for alignment)
proposal_counts: .space HASH_SIZE * 16

// Direction order (indices into dir_checks/moves: 0=N, 1=S, 2=W, 3=E)
dir_order: .space 4

// Number buffer for output
num_buf: .space 32

.section __TEXT,__text
.align 4

_start:
    // Initialize direction order: N, S, W, E = 0, 1, 2, 3
    adrp x0, dir_order@PAGE
    add x0, x0, dir_order@PAGEOFF
    mov w1, #0
    strb w1, [x0, #0]
    mov w1, #1
    strb w1, [x0, #1]
    mov w1, #2
    strb w1, [x0, #2]
    mov w1, #3
    strb w1, [x0, #3]

    // Open input file
    mov x16, #SYS_OPEN
    adrp x0, input_path@PAGE
    add x0, x0, input_path@PAGEOFF
    mov x1, #O_RDONLY
    mov x2, #0
    svc #0x80
    mov x19, x0             // Save fd

    // Read file
    mov x16, #SYS_READ
    mov x0, x19
    adrp x1, input_buf@PAGE
    add x1, x1, input_buf@PAGEOFF
    mov x2, #65536
    svc #0x80
    mov x20, x0             // Save bytes read

    // Close file
    mov x16, #SYS_CLOSE
    mov x0, x19
    svc #0x80

    // Parse input
    bl parse_input

    // Part 1: Run 10 rounds
    mov x21, #10            // Round counter

part1_loop:
    cbz x21, part1_done
    bl simulate_round
    bl rotate_directions
    sub x21, x21, #1
    b part1_loop

part1_done:
    // Calculate bounding box empty count
    bl calc_empty_tiles
    mov x22, x0             // Save Part 1 result

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_string
    mov x0, x22
    bl print_num
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    // Part 2: Continue until no moves
    // Re-parse and start fresh
    bl parse_input

    // Reset direction order
    adrp x0, dir_order@PAGE
    add x0, x0, dir_order@PAGEOFF
    mov w1, #0
    strb w1, [x0, #0]
    mov w1, #1
    strb w1, [x0, #1]
    mov w1, #2
    strb w1, [x0, #2]
    mov w1, #3
    strb w1, [x0, #3]

    mov x21, #0             // Round counter

part2_loop:
    add x21, x21, #1
    bl simulate_round
    // x0 = 1 if any elf moved, 0 if none moved
    cbz x0, part2_done
    bl rotate_directions
    b part2_loop

part2_done:
    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_string
    mov x0, x21
    bl print_num
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    // Exit
    mov x16, #SYS_EXIT
    mov x0, #0
    svc #0x80

// Parse input: find all '#' positions
// Input: x20 = bytes read (preserved from _start)
parse_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    adrp x10, input_buf@PAGE
    add x10, x10, input_buf@PAGEOFF
    adrp x11, elves@PAGE
    add x11, x11, elves@PAGEOFF

    mov x19, x20            // Save bytes read
    mov x12, #0             // row
    mov x13, #0             // col
    mov x14, #0             // elf count
    mov x15, #0             // buffer index

parse_loop:
    cmp x15, x19
    b.ge parse_done

    ldrb w16, [x10, x15]

    cmp w16, #'\n'
    b.eq parse_newline

    cmp w16, #'#'
    b.eq parse_elf

    // It's a '.' or other char
    add x13, x13, #1        // col++
    add x15, x15, #1
    b parse_loop

parse_newline:
    add x12, x12, #1        // row++
    mov x13, #0             // col = 0
    add x15, x15, #1
    b parse_loop

parse_elf:
    // Store elf position
    lsl x17, x14, #3        // offset = elf_count * 8
    add x17, x11, x17
    str w12, [x17, #0]      // row
    str w13, [x17, #4]      // col
    add x14, x14, #1
    add x13, x13, #1        // col++
    add x15, x15, #1
    b parse_loop

parse_done:
    adrp x0, elf_count@PAGE
    add x0, x0, elf_count@PAGEOFF
    str x14, [x0]

    // Build hash set
    bl build_hash_set

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Build hash set from elves array
build_hash_set:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    // Clear hash set
    adrp x10, hash_set@PAGE
    add x10, x10, hash_set@PAGEOFF
    mov x11, #HASH_SIZE
    mov w12, #0x8000
    lsl w12, w12, #16       // w12 = 0x80000000 (EMPTY sentinel)
clear_hash_loop:
    cbz x11, clear_done
    str w12, [x10], #8
    sub x11, x11, #1
    b clear_hash_loop

clear_done:
    // Insert all elves (use callee-saved regs since we call functions)
    adrp x19, elves@PAGE
    add x19, x19, elves@PAGEOFF
    adrp x20, elf_count@PAGE
    add x20, x20, elf_count@PAGEOFF
    ldr x20, [x20]
    mov x21, #0             // loop index

insert_loop:
    cmp x21, x20
    b.ge insert_done

    lsl x22, x21, #3
    add x22, x19, x22
    ldr w0, [x22, #0]       // row
    ldr w1, [x22, #4]       // col
    bl hash_set_insert

    add x21, x21, #1
    b insert_loop

insert_done:
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Hash function for (row, col) pair
// w0 = row, w1 = col
// Returns hash in x0
hash_pos:
    // Simple hash: ((row * 73856093) ^ (col * 83492791)) & HASH_MASK
    mov w2, #0x4660         // 73856093 & 0xFFFF
    movk w2, #0x467, lsl #16
    mul w3, w0, w2

    mov w2, #0xE817         // 83492791 & 0xFFFF
    movk w2, #0x4F9, lsl #16
    mul w4, w1, w2

    eor w0, w3, w4
    and w0, w0, #HASH_MASK
    ret

// Insert into hash set
// w0 = row, w1 = col
hash_set_insert:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov w19, w0             // Save row
    mov w20, w1             // Save col

    bl hash_pos

    adrp x10, hash_set@PAGE
    add x10, x10, hash_set@PAGEOFF

    mov x11, x0             // Starting index

probe_insert:
    lsl x12, x11, #3        // offset = index * 8
    add x12, x10, x12
    ldr w13, [x12]          // Check if slot empty

    mov w14, #0x8000
    lsl w14, w14, #16       // w14 = 0x80000000 (EMPTY sentinel)
    cmp w13, w14
    b.eq do_insert

    // Slot occupied, linear probe
    add x11, x11, #1
    and x11, x11, #HASH_MASK
    b probe_insert

do_insert:
    str w19, [x12, #0]      // Store row
    str w20, [x12, #4]      // Store col

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Check if position in hash set
// w0 = row, w1 = col
// Returns 1 if found, 0 otherwise
hash_set_contains:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov w19, w0
    mov w20, w1

    bl hash_pos

    adrp x10, hash_set@PAGE
    add x10, x10, hash_set@PAGEOFF

    mov x11, x0
    mov x12, #0             // Probe count

probe_contains:
    cmp x12, #HASH_SIZE
    b.ge not_found

    lsl x13, x11, #3
    add x13, x10, x13
    ldr w14, [x13]
    ldr w15, [x13, #4]

    mov w16, #0x8000
    lsl w16, w16, #16       // w16 = 0x80000000 (EMPTY sentinel)
    cmp w14, w16
    b.eq not_found

    cmp w14, w19
    b.ne continue_probe
    cmp w15, w20
    b.ne continue_probe

    // Found
    mov x0, #1
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

continue_probe:
    add x11, x11, #1
    and x11, x11, #HASH_MASK
    add x12, x12, #1
    b probe_contains

not_found:
    mov x0, #0
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Simulate one round
// Returns 1 if any elf moved, 0 otherwise
simulate_round:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    // Clear proposals
    adrp x10, proposals@PAGE
    add x10, x10, proposals@PAGEOFF
    adrp x11, elf_count@PAGE
    add x11, x11, elf_count@PAGEOFF
    ldr x11, [x11]
    mov w12, #0x8000
    lsl w12, w12, #16       // w12 = 0x80000000 (EMPTY sentinel)
    mov x13, #0

clear_proposals:
    cmp x13, x11
    b.ge clear_prop_done
    lsl x14, x13, #3
    add x14, x10, x14
    str w12, [x14, #0]
    str w12, [x14, #4]
    add x13, x13, #1
    b clear_proposals

clear_prop_done:
    // Clear proposal counts
    adrp x10, proposal_counts@PAGE
    add x10, x10, proposal_counts@PAGEOFF
    mov x11, #HASH_SIZE
    mov w12, #0x8000
    lsl w12, w12, #16       // w12 = 0x80000000 (EMPTY sentinel)

clear_counts:
    cbz x11, clear_counts_done
    str w12, [x10], #16
    sub x11, x11, #1
    b clear_counts

clear_counts_done:
    // Phase 1: Each elf proposes a move
    adrp x19, elves@PAGE
    add x19, x19, elves@PAGEOFF
    adrp x20, elf_count@PAGE
    add x20, x20, elf_count@PAGEOFF
    ldr x20, [x20]
    mov x21, #0             // elf index

phase1_loop:
    cmp x21, x20
    b.ge phase1_done

    lsl x22, x21, #3
    add x22, x19, x22
    ldr w23, [x22, #0]      // row
    ldr w24, [x22, #4]      // col

    // Check if any neighbors
    bl check_any_neighbors
    cbz x0, next_elf        // No neighbors, don't move

    // Try each direction in order
    adrp x25, dir_order@PAGE
    add x25, x25, dir_order@PAGEOFF
    mov x26, #0             // direction index

try_direction:
    cmp x26, #4
    b.ge next_elf           // No valid direction

    ldrb w27, [x25, x26]    // Get direction (0-3)

    mov w0, w23             // row
    mov w1, w24             // col
    mov w2, w27             // direction
    bl check_direction
    cbz x0, try_next_dir    // Direction blocked

    // Direction valid, propose move
    adrp x10, dir_moves@PAGE
    add x10, x10, dir_moves@PAGEOFF
    lsl w11, w27, #3        // offset = dir * 8
    add x10, x10, x11
    ldr w12, [x10, #0]      // dr
    ldr w13, [x10, #4]      // dc

    add w14, w23, w12       // new_row
    add w15, w24, w13       // new_col

    // Store proposal
    adrp x10, proposals@PAGE
    add x10, x10, proposals@PAGEOFF
    lsl x11, x21, #3
    add x10, x10, x11
    str w14, [x10, #0]
    str w15, [x10, #4]

    // Increment proposal count
    mov w0, w14
    mov w1, w15
    bl increment_proposal_count

    b next_elf

try_next_dir:
    add x26, x26, #1
    b try_direction

next_elf:
    add x21, x21, #1
    b phase1_loop

phase1_done:
    // Phase 2: Execute unique proposals
    mov x28, #0             // moved flag

    // Clear new_elves and rebuild
    adrp x19, elves@PAGE
    add x19, x19, elves@PAGEOFF
    adrp x20, elf_count@PAGE
    add x20, x20, elf_count@PAGEOFF
    ldr x20, [x20]
    adrp x21, new_elves@PAGE
    add x21, x21, new_elves@PAGEOFF
    adrp x22, proposals@PAGE
    add x22, x22, proposals@PAGEOFF

    mov x23, #0             // elf index

phase2_loop:
    cmp x23, x20
    b.ge phase2_done

    lsl x24, x23, #3
    add x25, x19, x24       // &elves[i]
    add x26, x21, x24       // &new_elves[i]
    add x27, x22, x24       // &proposals[i]

    ldr w0, [x27, #0]       // proposed row
    mov w1, #0x8000
    lsl w1, w1, #16         // w1 = 0x80000000 (EMPTY sentinel)
    cmp w0, w1
    b.eq keep_position      // No proposal

    ldr w1, [x27, #4]       // proposed col

    // Check proposal count
    stp x0, x1, [sp, #-16]!
    bl get_proposal_count
    ldp x2, x3, [sp], #16

    cmp x0, #1
    b.ne keep_position      // Not unique

    // Move elf
    str w2, [x26, #0]       // new row
    str w3, [x26, #4]       // new col
    mov x28, #1             // Set moved flag
    b next_elf2

keep_position:
    ldr w0, [x25, #0]
    ldr w1, [x25, #4]
    str w0, [x26, #0]
    str w1, [x26, #4]

next_elf2:
    add x23, x23, #1
    b phase2_loop

phase2_done:
    // Copy new_elves back to elves
    adrp x19, elves@PAGE
    add x19, x19, elves@PAGEOFF
    adrp x20, new_elves@PAGE
    add x20, x20, new_elves@PAGEOFF
    adrp x21, elf_count@PAGE
    add x21, x21, elf_count@PAGEOFF
    ldr x21, [x21]
    mov x22, #0

copy_loop:
    cmp x22, x21
    b.ge copy_done
    lsl x23, x22, #3
    ldr x24, [x20, x23]
    str x24, [x19, x23]
    add x22, x22, #1
    b copy_loop

copy_done:
    // Rebuild hash set
    bl build_hash_set

    mov x0, x28             // Return moved flag

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Check if any of the 8 neighbors is an elf
// w23 = row, w24 = col (preserved by caller)
// Returns 1 if any neighbor, 0 otherwise
check_any_neighbors:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov w19, w23            // Save row
    mov w20, w24            // Save col

    adrp x21, all_neighbors@PAGE
    add x21, x21, all_neighbors@PAGEOFF
    mov x22, #0             // neighbor index

check_neighbor_loop:
    cmp x22, #8
    b.ge no_neighbor

    lsl x10, x22, #3        // offset = index * 8
    add x10, x21, x10
    ldr w11, [x10, #0]      // dr
    ldr w12, [x10, #4]      // dc

    add w0, w19, w11        // row + dr
    add w1, w20, w12        // col + dc
    bl hash_set_contains
    cbnz x0, has_neighbor

    add x22, x22, #1
    b check_neighbor_loop

no_neighbor:
    mov x0, #0
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

has_neighbor:
    mov x0, #1
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Check if direction is valid (all 3 positions empty)
// w0 = row, w1 = col, w2 = direction (0-3)
// Returns 1 if valid, 0 otherwise
check_direction:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov w19, w0             // row
    mov w20, w1             // col
    mov w21, w2             // direction

    adrp x22, dir_checks@PAGE
    add x22, x22, dir_checks@PAGEOFF

    // Each direction has 6 words (3 positions * 2 coords)
    mov w23, #24            // 6 words * 4 bytes
    mul w23, w21, w23
    add x22, x22, x23       // Point to this direction's checks

    // Check first position
    ldr w10, [x22, #0]      // dr1
    ldr w11, [x22, #4]      // dc1
    add w0, w19, w10
    add w1, w20, w11
    bl hash_set_contains
    cbnz x0, dir_blocked

    // Check second position
    ldr w10, [x22, #8]      // dr2
    ldr w11, [x22, #12]     // dc2
    add w0, w19, w10
    add w1, w20, w11
    bl hash_set_contains
    cbnz x0, dir_blocked

    // Check third position
    ldr w10, [x22, #16]     // dr3
    ldr w11, [x22, #20]     // dc3
    add w0, w19, w10
    add w1, w20, w11
    bl hash_set_contains
    cbnz x0, dir_blocked

    // Direction valid
    mov x0, #1
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

dir_blocked:
    mov x0, #0
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Increment proposal count for position
// w0 = row, w1 = col
increment_proposal_count:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov w19, w0             // row
    mov w20, w1             // col

    bl hash_pos             // Get hash

    adrp x21, proposal_counts@PAGE
    add x21, x21, proposal_counts@PAGEOFF

    mov x22, x0             // Starting index

inc_probe:
    lsl x10, x22, #4        // offset = index * 16
    add x10, x21, x10
    ldr w11, [x10]          // Check row

    mov w12, #0x8000
    lsl w12, w12, #16       // w12 = 0x80000000 (EMPTY sentinel)
    cmp w11, w12
    b.eq inc_insert         // Empty slot

    // Check if same position
    ldr w13, [x10, #4]      // col
    cmp w11, w19
    b.ne inc_next
    cmp w13, w20
    b.ne inc_next

    // Found, increment count
    ldr w14, [x10, #8]
    add w14, w14, #1
    str w14, [x10, #8]
    b inc_done

inc_next:
    add x22, x22, #1
    and x22, x22, #HASH_MASK
    b inc_probe

inc_insert:
    str w19, [x10, #0]      // row
    str w20, [x10, #4]      // col
    mov w14, #1
    str w14, [x10, #8]      // count = 1

inc_done:
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Get proposal count for position
// w0 = row, w1 = col
// Returns count in x0
get_proposal_count:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov w19, w0
    mov w20, w1

    bl hash_pos

    adrp x21, proposal_counts@PAGE
    add x21, x21, proposal_counts@PAGEOFF

    mov x22, x0

get_probe:
    lsl x10, x22, #4
    add x10, x21, x10
    ldr w11, [x10]

    mov w12, #0x8000
    lsl w12, w12, #16       // w12 = 0x80000000 (EMPTY sentinel)
    cmp w11, w12
    b.eq get_not_found

    ldr w13, [x10, #4]
    cmp w11, w19
    b.ne get_next
    cmp w13, w20
    b.ne get_next

    ldr w0, [x10, #8]
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

get_next:
    add x22, x22, #1
    and x22, x22, #HASH_MASK
    b get_probe

get_not_found:
    mov x0, #0
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Rotate directions: move first to end
rotate_directions:
    adrp x0, dir_order@PAGE
    add x0, x0, dir_order@PAGEOFF

    ldrb w1, [x0, #0]       // Save first
    ldrb w2, [x0, #1]
    ldrb w3, [x0, #2]
    ldrb w4, [x0, #3]

    strb w2, [x0, #0]
    strb w3, [x0, #1]
    strb w4, [x0, #2]
    strb w1, [x0, #3]

    ret

// Calculate empty tiles in bounding box
calc_empty_tiles:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    adrp x10, elves@PAGE
    add x10, x10, elves@PAGEOFF
    adrp x11, elf_count@PAGE
    add x11, x11, elf_count@PAGEOFF
    ldr x11, [x11]

    // Initialize min/max - will be set from first elf

    // First elf initializes
    ldr w19, [x10, #0]      // min_row = first row
    mov w20, w19            // max_row = first row
    ldr w21, [x10, #4]      // min_col = first col
    mov w22, w21            // max_col = first col

    mov x23, #1             // index (start from 1)

bounds_loop:
    cmp x23, x11
    b.ge bounds_done

    lsl x24, x23, #3
    add x24, x10, x24
    ldr w0, [x24, #0]       // row
    ldr w1, [x24, #4]       // col

    cmp w0, w19
    csel w19, w0, w19, lt   // min_row
    cmp w0, w20
    csel w20, w0, w20, gt   // max_row
    cmp w1, w21
    csel w21, w1, w21, lt   // min_col
    cmp w1, w22
    csel w22, w1, w22, gt   // max_col

    add x23, x23, #1
    b bounds_loop

bounds_done:
    // Calculate area
    sub w0, w20, w19        // height - 1
    add w0, w0, #1          // height
    sub w1, w22, w21        // width - 1
    add w1, w1, #1          // width
    mul w0, w0, w1          // area

    // Subtract elf count
    sub w0, w0, w11         // area - elf_count

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print null-terminated string
// x0 = string address
print_string:
    mov x10, x0
    mov x11, #0

strlen_loop:
    ldrb w12, [x10, x11]
    cbz w12, strlen_done
    add x11, x11, #1
    b strlen_loop

strlen_done:
    mov x16, #SYS_WRITE
    mov x0, #STDOUT
    mov x1, x10
    mov x2, x11
    svc #0x80
    ret

// Print number
// x0 = number to print
print_num:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    adrp x19, num_buf@PAGE
    add x19, x19, num_buf@PAGEOFF
    add x19, x19, #30       // End of buffer
    mov x20, x0             // Number

    // Handle 0
    cbnz x20, not_zero
    mov w10, #'0'
    strb w10, [x19]
    sub x19, x19, #1
    b print_num_done

not_zero:
    mov x10, #10

digit_loop:
    cbz x20, print_num_done
    udiv x11, x20, x10
    msub x12, x11, x10, x20 // remainder
    add w12, w12, #'0'
    strb w12, [x19]
    sub x19, x19, #1
    mov x20, x11
    b digit_loop

print_num_done:
    add x19, x19, #1        // Point to first digit

    mov x16, #SYS_WRITE
    mov x0, #STDOUT
    mov x1, x19
    adrp x10, num_buf@PAGE
    add x10, x10, num_buf@PAGEOFF
    add x10, x10, #31
    sub x2, x10, x19
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
