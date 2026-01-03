// Day 22: Sand Slabs - ARM64 Assembly for macOS
// 3D brick falling simulation

.global _start
.align 4

// Constants
.equ MAX_BRICKS, 1600
.equ MAX_SUPPORTS, 100          // Max bricks one brick can support
.equ BUFFER_SIZE, 65536
.equ HASH_SIZE, 199999          // Prime number for hash table

// Syscall numbers for macOS
.equ SYS_EXIT, 1
.equ SYS_READ, 3
.equ SYS_WRITE, 4
.equ SYS_OPEN, 5
.equ SYS_CLOSE, 6

// File flags
.equ O_RDONLY, 0

.text

// Macro for loading data addresses
.macro load_addr reg, symbol
    adrp \reg, \symbol@PAGE
    add \reg, \reg, \symbol@PAGEOFF
.endm

_start:
    // Open input file
    load_addr x0, input_path
    mov x1, #O_RDONLY
    mov x2, #0
    mov x16, #SYS_OPEN
    svc #0x80

    cmp x0, #0
    b.lt exit_error
    mov x19, x0                 // Save fd

    // Read file into buffer
    mov x0, x19
    load_addr x1, file_buffer
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

    // Parse bricks
    load_addr x0, file_buffer
    mov x1, x20
    bl parse_bricks
    mov x19, x0                 // Number of bricks

    // Settle bricks and build support graph
    mov x0, x19
    bl settle_bricks

    // Part 1: Count safe bricks
    mov x0, x19
    bl count_safe_bricks
    mov x20, x0                 // Part 1 answer

    // Part 2: Count chain reactions
    mov x0, x19
    bl count_chain_falls
    mov x21, x0                 // Part 2 answer

    // Print results
    load_addr x0, part1_msg
    bl print_string
    mov x0, x20
    bl print_number
    bl print_newline

    load_addr x0, part2_msg
    bl print_string
    mov x0, x21
    bl print_number
    bl print_newline

    // Exit
    mov x0, #0
    mov x16, #SYS_EXIT
    svc #0x80

exit_error:
    mov x0, #1
    mov x16, #SYS_EXIT
    svc #0x80

// ============================================================
// parse_bricks: Parse input and store brick coordinates
// Input: x0 = buffer ptr, x1 = length
// Output: x0 = number of bricks
// Brick format in memory: x1,y1,z1,x2,y2,z2 (6 x 32-bit ints)
// ============================================================
parse_bricks:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0                 // buffer ptr
    add x20, x0, x1             // end ptr
    load_addr x21, bricks       // brick array
    mov x22, #0                 // brick count

parse_loop:
    cmp x19, x20
    b.ge parse_done

    // Parse x1
    mov x0, x19
    bl parse_int
    mov x23, x0                 // x1
    mov x19, x1
    add x19, x19, #1            // skip comma

    // Parse y1
    mov x0, x19
    bl parse_int
    mov x24, x0                 // y1
    mov x19, x1
    add x19, x19, #1            // skip comma

    // Parse z1
    mov x0, x19
    bl parse_int
    str w23, [x21]              // store x1
    str w24, [x21, #4]          // store y1
    str w0, [x21, #8]           // store z1
    mov x23, x0                 // save z1
    mov x19, x1
    add x19, x19, #1            // skip ~

    // Parse x2
    mov x0, x19
    bl parse_int
    str w0, [x21, #12]          // store x2
    mov x19, x1
    add x19, x19, #1            // skip comma

    // Parse y2
    mov x0, x19
    bl parse_int
    str w0, [x21, #16]          // store y2
    mov x19, x1
    add x19, x19, #1            // skip comma

    // Parse z2
    mov x0, x19
    bl parse_int
    str w0, [x21, #20]          // store z2
    mov x24, x0                 // z2
    mov x19, x1

    // Ensure z1 <= z2 (swap if needed)
    ldr w0, [x21, #8]           // z1
    cmp w0, w24
    b.le no_swap

    // Swap all coordinates
    ldr w0, [x21]               // x1
    ldr w1, [x21, #12]          // x2
    str w1, [x21]
    str w0, [x21, #12]

    ldr w0, [x21, #4]           // y1
    ldr w1, [x21, #16]          // y2
    str w1, [x21, #4]
    str w0, [x21, #16]

    ldr w0, [x21, #8]           // z1
    ldr w1, [x21, #20]          // z2
    str w1, [x21, #8]
    str w0, [x21, #20]

no_swap:
    add x21, x21, #24           // next brick
    add x22, x22, #1            // count++

    // Skip to next line
skip_newline:
    cmp x19, x20
    b.ge parse_done
    ldrb w0, [x19]
    cmp w0, #'\n'
    add x19, x19, #1
    b.ne skip_newline
    b parse_loop

parse_done:
    mov x0, x22
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// parse_int: Parse integer from string
// Input: x0 = string ptr
// Output: x0 = integer, x1 = ptr after number
// ============================================================
parse_int:
    mov x2, #0                  // result
    mov x1, x0                  // current ptr

parse_int_loop:
    ldrb w3, [x1]
    sub w4, w3, #'0'
    cmp w4, #9
    b.hi parse_int_done

    mov x5, #10
    mul x2, x2, x5
    add x2, x2, x4
    add x1, x1, #1
    b parse_int_loop

parse_int_done:
    mov x0, x2
    ret

// ============================================================
// settle_bricks: Drop all bricks and build support graph
// Input: x0 = number of bricks
// ============================================================
settle_bricks:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    mov x19, x0                 // num_bricks

    // Clear occupied hash table (set all brick_idx to -1)
    load_addr x0, occupied_table
    ldr x1, =HASH_SIZE
    mov x2, #16                 // entry size (x,y,z,brick_idx) each 4 bytes
    mul x1, x1, x2
    mov w2, #-1
clear_occ:
    str w2, [x0, #12]           // set brick_idx to -1 (empty)
    add x0, x0, #16
    subs x1, x1, #16
    b.gt clear_occ

    // Clear support arrays
    load_addr x0, supports_count
    mov x1, #MAX_BRICKS
    lsl x1, x1, #2
clear_sup:
    str wzr, [x0], #4
    subs x1, x1, #4
    b.gt clear_sup

    load_addr x0, supporters_count
    mov x1, #MAX_BRICKS
    lsl x1, x1, #2
clear_supp:
    str wzr, [x0], #4
    subs x1, x1, #4
    b.gt clear_supp

    // Build sorted index by min z
    load_addr x0, sorted_indices
    mov x1, #0
build_idx:
    str w1, [x0, x1, lsl #2]
    add x1, x1, #1
    cmp x1, x19
    b.lt build_idx

    // Sort indices by z1 (bubble sort - simple for this size)
    mov x0, x19
    bl sort_by_z

    // Process each brick in sorted order
    mov x20, #0                 // i = 0

settle_loop:
    cmp x20, x19
    b.ge settle_done

    // Get original brick index
    load_addr x0, sorted_indices
    ldr w21, [x0, x20, lsl #2]  // orig_idx

    // Get brick coordinates
    load_addr x0, bricks
    mov x1, #24
    mul x1, x21, x1
    add x0, x0, x1

    ldr w22, [x0]               // x1
    ldr w23, [x0, #4]           // y1
    ldr w24, [x0, #8]           // z1
    ldr w25, [x0, #12]          // x2
    ldr w26, [x0, #16]          // y2
    ldr w27, [x0, #20]          // z2

    // Find max drop
    sub w28, w24, #1            // max_drop = z1 - 1

    // For each xy in footprint, find max occupied z below
    mov w9, w22                 // x = x1
find_drop_x:
    cmp w9, w25
    b.gt find_drop_done

    mov w10, w23                // y = y1
find_drop_y:
    cmp w10, w26
    b.gt find_drop_y_done

    // Check each z below
    sub w11, w24, #1            // z = z1 - 1
find_drop_z:
    cmp w11, #0
    b.le find_drop_z_done

    // Hash lookup for (x, y, z)
    mov w0, w9
    mov w1, w10
    mov w2, w11
    bl hash_lookup

    cmp x0, #0
    b.eq find_drop_z_next

    // Found occupied cell, calc drop
    sub w0, w24, w11
    sub w0, w0, #1
    cmp w0, w28
    csel w28, w0, w28, lt       // min(drop, current_min)
    b find_drop_z_done

find_drop_z_next:
    sub w11, w11, #1
    b find_drop_z

find_drop_z_done:
    add w10, w10, #1
    b find_drop_y

find_drop_y_done:
    add w9, w9, #1
    b find_drop_x

find_drop_done:
    // Apply drop to brick
    sub w24, w24, w28           // new_z1 = z1 - drop
    sub w27, w27, w28           // new_z2 = z2 - drop

    // Store settled coordinates
    load_addr x0, bricks
    mov x1, #24
    mul x1, x21, x1
    add x0, x0, x1
    str w24, [x0, #8]           // update z1
    str w27, [x0, #20]          // update z2

    // Mark cells as occupied and find supporters
    mov w9, w22                 // x = x1
mark_x:
    cmp w9, w25
    b.gt mark_done

    mov w10, w23                // y = y1
mark_y:
    cmp w10, w26
    b.gt mark_y_done

    // Check for supporter at z-1
    sub w11, w24, #1
    cmp w11, #0
    b.le no_supporter

    mov w0, w9
    mov w1, w10
    mov w2, w11
    bl hash_lookup

    cmp x0, #0
    b.eq no_supporter

    // Add supporter relationship
    ldr w0, [x0]                // supporter brick index
    mov w1, w21                 // current brick
    bl add_support_relation

no_supporter:
    // Mark all z cells of this brick as occupied
    mov w11, w24                // z = new_z1
mark_z:
    cmp w11, w27
    b.gt mark_z_done

    mov w0, w9
    mov w1, w10
    mov w2, w11
    mov w3, w21                 // brick index
    bl hash_insert

    add w11, w11, #1
    b mark_z

mark_z_done:
    add w10, w10, #1
    b mark_y

mark_y_done:
    add w9, w9, #1
    b mark_x

mark_done:
    add x20, x20, #1
    b settle_loop

settle_done:
    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// sort_by_z: Sort indices by brick's z1 coordinate
// Input: x0 = count
// ============================================================
sort_by_z:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0                 // n
    sub x19, x19, #1

sort_outer:
    cmp x19, #0
    b.le sort_done

    mov x20, #0                 // i

sort_inner:
    cmp x20, x19
    b.ge sort_outer_next

    load_addr x0, sorted_indices
    ldr w21, [x0, x20, lsl #2]
    add x1, x20, #1
    ldr w22, [x0, x1, lsl #2]

    // Get z1 for both
    load_addr x2, bricks
    mov x3, #24
    mul x4, x21, x3
    add x4, x2, x4
    ldr w5, [x4, #8]            // z1 of first brick

    mul x4, x22, x3
    add x4, x2, x4
    ldr w6, [x4, #8]            // z1 of second brick

    cmp w5, w6
    b.le sort_no_swap

    // Swap
    load_addr x0, sorted_indices
    str w22, [x0, x20, lsl #2]
    add x1, x20, #1
    str w21, [x0, x1, lsl #2]

sort_no_swap:
    add x20, x20, #1
    b sort_inner

sort_outer_next:
    sub x19, x19, #1
    b sort_outer

sort_done:
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// hash_lookup: Look up (x,y,z) in occupied table
// Input: w0=x, w1=y, w2=z
// Output: x0 = ptr to brick_idx or 0 if not found
// ============================================================
hash_lookup:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    // Compute hash: ((x * 1000 + y) * 1000 + z) % HASH_SIZE
    mov w19, w0                 // save x
    mov w20, w1                 // save y
    mov w21, w2                 // save z

    mov w4, #1000
    mul w0, w19, w4
    add w0, w0, w20
    mul w0, w0, w4
    add w0, w0, w21

    // Modulo by HASH_SIZE
    ldr w4, =HASH_SIZE
    udiv w5, w0, w4
    msub w0, w5, w4, w0         // hash = value % HASH_SIZE

    load_addr x23, occupied_table
    mov x2, #16                 // entry size

    // Linear probing
    mov x22, #0                 // probes
    ldr x24, =HASH_SIZE
lookup_probe:
    cmp x22, x24
    b.ge lookup_not_found

    mul x5, x0, x2
    add x5, x23, x5

    ldr w6, [x5, #12]           // check if occupied (brick_idx, -1 = empty)
    cmn w6, #1
    b.eq lookup_not_found       // empty slot

    // Check if matches
    ldr w7, [x5]                // stored x
    cmp w7, w19
    b.ne lookup_next

    ldr w7, [x5, #4]            // stored y
    cmp w7, w20
    b.ne lookup_next

    ldr w7, [x5, #8]            // stored z
    cmp w7, w21
    b.ne lookup_next

    // Found
    add x0, x5, #12
    b lookup_done

lookup_next:
    add x0, x0, #1
    cmp x0, x24
    b.lt lookup_no_wrap
    mov x0, #0
lookup_no_wrap:
    add x22, x22, #1
    b lookup_probe

lookup_not_found:
    mov x0, #0

lookup_done:
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// hash_insert: Insert (x,y,z) -> brick_idx into table
// Input: w0=x, w1=y, w2=z, w3=brick_idx
// ============================================================
hash_insert:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov w19, w0                 // x
    mov w20, w1                 // y
    mov w21, w2                 // z
    mov w22, w3                 // brick_idx

    // Compute hash
    mov w4, #1000
    mul w0, w19, w4
    add w0, w0, w20
    mul w0, w0, w4
    add w0, w0, w21

    ldr w4, =HASH_SIZE
    udiv w5, w0, w4
    msub w0, w5, w4, w0

    load_addr x23, occupied_table
    mov x2, #16
    ldr x24, =HASH_SIZE

    // Linear probing for empty slot
insert_probe:
    mul x5, x0, x2
    add x5, x23, x5

    ldr w6, [x5, #12]
    cmn w6, #1                  // -1 = empty
    b.eq insert_here

    // Check if same key (update)
    ldr w7, [x5]
    cmp w7, w19
    b.ne insert_next
    ldr w7, [x5, #4]
    cmp w7, w20
    b.ne insert_next
    ldr w7, [x5, #8]
    cmp w7, w21
    b.eq insert_here            // same key, update

insert_next:
    add x0, x0, #1
    cmp x0, x24
    b.lt insert_no_wrap
    mov x0, #0
insert_no_wrap:
    b insert_probe

insert_here:
    str w19, [x5]
    str w20, [x5, #4]
    str w21, [x5, #8]
    str w22, [x5, #12]

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// add_support_relation: Record that supporter supports current
// Input: w0=supporter, w1=current
// ============================================================
add_support_relation:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov w19, w0                 // supporter
    mov w20, w1                 // current (supported)

    // Add to supports[supporter]
    load_addr x21, supports_count
    ldr w1, [x21, x19, lsl #2]

    // Check if already added (avoid duplicates)
    load_addr x2, supports_array
    mov x3, #MAX_SUPPORTS
    lsl x3, x3, #2
    mul x4, x19, x3
    add x4, x2, x4

    mov x5, #0
check_dup_sup:
    cmp x5, x1
    b.ge add_sup
    ldr w6, [x4, x5, lsl #2]
    cmp w6, w20
    b.eq skip_add_sup
    add x5, x5, #1
    b check_dup_sup

add_sup:
    str w20, [x4, x1, lsl #2]
    add w1, w1, #1
    str w1, [x21, x19, lsl #2]

skip_add_sup:
    // Add to supporters[current]
    load_addr x21, supporters_count
    ldr w1, [x21, x20, lsl #2]

    load_addr x2, supporters_array
    mov x3, #MAX_SUPPORTS
    lsl x3, x3, #2
    mul x4, x20, x3
    add x4, x2, x4

    mov x5, #0
check_dup_supp:
    cmp x5, x1
    b.ge add_supp
    ldr w6, [x4, x5, lsl #2]
    cmp w6, w19
    b.eq skip_add_supp
    add x5, x5, #1
    b check_dup_supp

add_supp:
    str w19, [x4, x1, lsl #2]
    add w1, w1, #1
    str w1, [x21, x20, lsl #2]

skip_add_supp:
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// count_safe_bricks: Part 1 - count bricks safe to disintegrate
// Input: x0 = num_bricks
// Output: x0 = count
// ============================================================
count_safe_bricks:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0                 // num_bricks
    mov x20, #0                 // i
    mov x21, #0                 // safe_count
    load_addr x23, supports_count
    load_addr x24, supporters_count

safe_loop:
    cmp x20, x19
    b.ge safe_done

    // Check if brick i can be safely removed
    // i.e., every brick it supports has at least 2 supporters

    ldr w22, [x23, x20, lsl #2] // number of bricks i supports

    cmp w22, #0
    b.eq is_safe                // supports nothing, safe

    load_addr x0, supports_array
    mov x1, #MAX_SUPPORTS
    lsl x1, x1, #2
    mul x2, x20, x1
    add x0, x0, x2              // supports[i] array

    mov x3, #0                  // j
check_supported:
    cmp x3, x22
    b.ge is_safe

    ldr w4, [x0, x3, lsl #2]    // supported brick index

    // Check how many supporters it has
    ldr w6, [x24, x4, lsl #2]

    cmp w6, #1
    b.le not_safe               // only one supporter (us)

    add x3, x3, #1
    b check_supported

is_safe:
    add x21, x21, #1
    b safe_next

not_safe:
safe_next:
    add x20, x20, #1
    b safe_loop

safe_done:
    mov x0, x21
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// count_chain_falls: Part 2 - count total chain reaction falls
// Input: x0 = num_bricks
// Output: x0 = total falls
// ============================================================
count_chain_falls:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0                 // num_bricks
    mov x20, #0                 // brick index i
    mov x21, #0                 // total_falls

chain_loop:
    cmp x20, x19
    b.ge chain_done

    // Simulate removing brick i
    mov x0, x20
    mov x1, x19
    bl simulate_removal

    add x21, x21, x0
    add x20, x20, #1
    b chain_loop

chain_done:
    mov x0, x21
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// simulate_removal: BFS to count chain reaction
// Input: x0 = removed brick, x1 = num_bricks
// Output: x0 = number of bricks that fall (not including initial)
// ============================================================
simulate_removal:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    mov x19, x0                 // removed brick
    mov x20, x1                 // num_bricks

    // Clear falling set
    load_addr x0, falling_set
    mov x1, #MAX_BRICKS
    lsr x1, x1, #3
    add x1, x1, #1
clear_falling:
    str xzr, [x0], #8
    subs x1, x1, #1
    b.gt clear_falling

    // Mark initial brick as falling
    load_addr x27, falling_set
    lsr x1, x19, #6             // qword index
    and x2, x19, #63            // bit index
    mov x3, #1
    lsl x3, x3, x2
    ldr x4, [x27, x1, lsl #3]
    orr x4, x4, x3
    str x4, [x27, x1, lsl #3]

    // Initialize queue
    load_addr x21, bfs_queue
    str w19, [x21]
    mov x22, #0                 // queue head
    mov x23, #1                 // queue tail
    mov x24, #0                 // fall count

    // Preload addresses for inner loop
    load_addr x25, supports_count
    load_addr x26, supports_array
    load_addr x28, supporters_count

bfs_loop:
    cmp x22, x23
    b.ge bfs_done

    // Dequeue
    ldr w0, [x21, x22, lsl #2]
    add x22, x22, #1
    mov w9, w0                  // current brick in w9

    // Get bricks that this brick supports
    ldr w10, [x25, x9, lsl #2]  // supports_count[brick]

    cmp w10, #0
    b.eq bfs_loop

    mov x1, #MAX_SUPPORTS
    lsl x1, x1, #2
    mul x2, x9, x1
    add x11, x26, x2            // supports[brick] array in x11

    mov x3, #0                  // j
check_falls:
    cmp x3, x10
    b.ge bfs_loop

    ldr w4, [x11, x3, lsl #2]   // supported brick

    // Check if already falling
    lsr x6, x4, #6
    and x7, x4, #63
    ldr x8, [x27, x6, lsl #3]
    lsr x12, x8, x7
    and x12, x12, #1
    cbnz x12, check_falls_next  // already falling

    // Check if all supporters are falling
    // Inline the check for performance
    mov w13, w4                 // brick to check

    // Get supporters of brick w13
    load_addr x14, supporters_array
    mov x15, #MAX_SUPPORTS
    lsl x15, x15, #2
    mul x16, x13, x15
    add x14, x14, x16           // supporters[brick] array

    ldr w15, [x28, x13, lsl #2] // supporters_count[brick]

    cmp w15, #0
    b.eq mark_falling           // no supporters = falls

    mov x16, #0                 // i
check_supp_loop:
    cmp x16, x15
    b.ge mark_falling           // all supporters falling

    ldr w17, [x14, x16, lsl #2] // supporter brick

    // Check if in falling set
    lsr x0, x17, #6
    and x1, x17, #63
    ldr x2, [x27, x0, lsl #3]
    lsr x2, x2, x1
    and x2, x2, #1

    cbz x2, check_falls_next    // not falling, so brick doesn't fall

    add x16, x16, #1
    b check_supp_loop

mark_falling:
    // Mark as falling and enqueue
    lsr x6, x4, #6
    and x7, x4, #63
    mov x8, #1
    lsl x8, x8, x7
    ldr x12, [x27, x6, lsl #3]
    orr x12, x12, x8
    str x12, [x27, x6, lsl #3]

    str w4, [x21, x23, lsl #2]
    add x23, x23, #1
    add x24, x24, #1

check_falls_next:
    add x3, x3, #1
    b check_falls

bfs_done:
    mov x0, x24
    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// Output routines
// ============================================================
print_string:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    mov x19, x0

    // Find length
    mov x1, #0
str_len:
    ldrb w2, [x0, x1]
    cbz w2, str_len_done
    add x1, x1, #1
    b str_len

str_len_done:
    mov x2, x1
    mov x1, x19
    mov x0, #1                  // stdout
    mov x16, #SYS_WRITE
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

print_number:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    load_addr x20, num_buffer
    add x20, x20, #20
    mov x1, #0
    strb w1, [x20]
    sub x20, x20, #1

    cmp x19, #0
    b.ne convert_loop

    mov w1, #'0'
    strb w1, [x20]
    b print_num_str

convert_loop:
    cbz x19, print_num_str

    mov x1, #10
    udiv x2, x19, x1
    msub x3, x2, x1, x19
    add w3, w3, #'0'
    strb w3, [x20]
    sub x20, x20, #1
    mov x19, x2
    b convert_loop

print_num_str:
    add x0, x20, #1
    bl print_string

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

print_newline:
    stp x29, x30, [sp, #-16]!

    load_addr x1, newline
    mov x0, #1
    mov x2, #1
    mov x16, #SYS_WRITE
    svc #0x80

    ldp x29, x30, [sp], #16
    ret

// ============================================================
// Data section
// ============================================================
.data

input_path:
    .asciz "../input.txt"

part1_msg:
    .asciz "Part 1: "

part2_msg:
    .asciz "Part 2: "

newline:
    .ascii "\n"

num_buffer:
    .space 24

.bss

.align 4

file_buffer:
    .space BUFFER_SIZE

// Brick storage: 6 x 4 bytes each = 24 bytes per brick
bricks:
    .space MAX_BRICKS * 24

// Sorted indices for processing order
sorted_indices:
    .space MAX_BRICKS * 4

// Hash table for occupied cells: (x, y, z, brick_idx) each 4 bytes = 16 bytes
// Initialize brick_idx to -1 for empty
occupied_table:
    .space HASH_SIZE * 16

// Support arrays: supports[i] = list of bricks that brick i supports
supports_count:
    .space MAX_BRICKS * 4

supports_array:
    .space MAX_BRICKS * MAX_SUPPORTS * 4

// Supporters arrays: supporters[i] = list of bricks that support brick i
supporters_count:
    .space MAX_BRICKS * 4

supporters_array:
    .space MAX_BRICKS * MAX_SUPPORTS * 4

// BFS queue for part 2
bfs_queue:
    .space MAX_BRICKS * 4

// Falling set as bitmap for part 2
falling_set:
    .space (MAX_BRICKS / 8) + 8
