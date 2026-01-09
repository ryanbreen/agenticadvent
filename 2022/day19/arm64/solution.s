// Day 19: Not Enough Minerals - ARM64 Assembly Solution
// Robot factory optimization with DFS and pruning

.global _start
.align 4

// Constants
.equ MAX_BLUEPRINTS, 30
.equ BUFFER_SIZE, 8192

// Blueprint offsets (7 values per blueprint: id, ore_ore, clay_ore, obs_ore, obs_clay, geo_ore, geo_obs)
.equ BP_ID, 0
.equ BP_ORE_ORE, 4
.equ BP_CLAY_ORE, 8
.equ BP_OBS_ORE, 12
.equ BP_OBS_CLAY, 16
.equ BP_GEO_ORE, 20
.equ BP_GEO_OBS, 24
.equ BP_SIZE, 28

// DFS state offsets on stack
.equ ST_ORE_ORE, 0
.equ ST_CLAY_ORE, 4
.equ ST_OBS_ORE, 8
.equ ST_OBS_CLAY, 12
.equ ST_GEO_ORE, 16
.equ ST_GEO_OBS, 20
.equ ST_MAX_ORE, 24
.equ ST_MAX_CLAY, 28
.equ ST_MAX_OBS, 32
.equ ST_TIME_LIM, 36
.equ ST_BEST, 40
.equ ST_BASE, 48   // pointer to base of this structure

.data
.align 4
filename:       .asciz "../input.txt"
fmt_part1:      .asciz "Part 1: "
fmt_part2:      .asciz "Part 2: "
newline:        .asciz "\n"

.bss
.align 4
buffer:         .skip BUFFER_SIZE
blueprints:     .skip MAX_BLUEPRINTS * BP_SIZE
num_blueprints: .skip 4
dfs_state:      .skip 64         // Blueprint data for current DFS

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

    // Parse blueprints
    adrp x0, buffer@PAGE
    add x0, x0, buffer@PAGEOFF
    bl parse_blueprints

    // Part 1
    mov w19, #24            // time limit
    bl solve_part1
    mov x20, x0             // save part1 result

    // Print Part 1
    adrp x0, fmt_part1@PAGE
    add x0, x0, fmt_part1@PAGEOFF
    bl print_string
    mov x0, x20
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    // Part 2
    mov w19, #32            // time limit
    bl solve_part2
    mov x21, x0             // save part2 result

    // Print Part 2
    adrp x0, fmt_part2@PAGE
    add x0, x0, fmt_part2@PAGEOFF
    bl print_string
    mov x0, x21
    bl print_number
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

    // Null terminate
    adrp x1, buffer@PAGE
    add x1, x1, buffer@PAGEOFF
    strb wzr, [x1, x0]

    // Close file
    mov x0, x19
    mov x16, #6             // close syscall
    svc #0x80

    ldp x29, x30, [sp], #16
    ret

// ============================================================
// parse_blueprints - Parse all blueprints from buffer
// x0 = buffer pointer
// ============================================================
parse_blueprints:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0             // buffer ptr
    adrp x20, blueprints@PAGE
    add x20, x20, blueprints@PAGEOFF  // blueprints array
    mov w21, #0             // blueprint count

.parse_loop:
    // Skip to first digit
    mov x0, x19
    bl skip_to_digit
    cbz x0, .parse_done
    mov x19, x0

    // Parse blueprint id
    mov x0, x19
    bl parse_int
    mov x19, x1
    str w0, [x20, #BP_ID]

    // Skip to ore robot cost
    mov x0, x19
    bl skip_to_digit
    mov x19, x0
    mov x0, x19
    bl parse_int
    mov x19, x1
    str w0, [x20, #BP_ORE_ORE]

    // Skip to clay robot cost
    mov x0, x19
    bl skip_to_digit
    mov x19, x0
    mov x0, x19
    bl parse_int
    mov x19, x1
    str w0, [x20, #BP_CLAY_ORE]

    // Skip to obsidian robot ore cost
    mov x0, x19
    bl skip_to_digit
    mov x19, x0
    mov x0, x19
    bl parse_int
    mov x19, x1
    str w0, [x20, #BP_OBS_ORE]

    // Skip to obsidian robot clay cost
    mov x0, x19
    bl skip_to_digit
    mov x19, x0
    mov x0, x19
    bl parse_int
    mov x19, x1
    str w0, [x20, #BP_OBS_CLAY]

    // Skip to geode robot ore cost
    mov x0, x19
    bl skip_to_digit
    mov x19, x0
    mov x0, x19
    bl parse_int
    mov x19, x1
    str w0, [x20, #BP_GEO_ORE]

    // Skip to geode robot obsidian cost
    mov x0, x19
    bl skip_to_digit
    mov x19, x0
    mov x0, x19
    bl parse_int
    mov x19, x1
    str w0, [x20, #BP_GEO_OBS]

    add x20, x20, #BP_SIZE
    add w21, w21, #1
    b .parse_loop

.parse_done:
    adrp x0, num_blueprints@PAGE
    add x0, x0, num_blueprints@PAGEOFF
    str w21, [x0]

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// skip_to_digit - Find next digit in string
// x0 = string ptr
// Returns: x0 = ptr to digit or 0 if end
// ============================================================
skip_to_digit:
.skip_loop:
    ldrb w1, [x0]
    cbz w1, .skip_end
    cmp w1, #'0'
    blt .skip_next
    cmp w1, #'9'
    ble .skip_found
.skip_next:
    add x0, x0, #1
    b .skip_loop
.skip_end:
    mov x0, #0
.skip_found:
    ret

// ============================================================
// parse_int - Parse integer from string
// x0 = string ptr
// Returns: w0 = value, x1 = new ptr
// ============================================================
parse_int:
    mov w2, #0              // value
    mov x1, x0              // ptr
.pi_loop:
    ldrb w3, [x1]
    cmp w3, #'0'
    blt .pi_done
    cmp w3, #'9'
    bgt .pi_done
    sub w3, w3, #'0'
    mov w4, #10
    mul w2, w2, w4
    add w2, w2, w3
    add x1, x1, #1
    b .pi_loop
.pi_done:
    mov w0, w2
    ret

// ============================================================
// solve_part1 - Sum quality levels (id * geodes) for all blueprints
// w19 = time limit
// Returns: x0 = total quality
// ============================================================
solve_part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov w20, w19            // time limit
    adrp x21, blueprints@PAGE
    add x21, x21, blueprints@PAGEOFF  // current blueprint
    adrp x22, num_blueprints@PAGE
    add x22, x22, num_blueprints@PAGEOFF
    ldr w22, [x22]          // num blueprints
    mov w23, #0             // total quality
    mov w24, #0             // index

.p1_loop:
    cmp w24, w22
    bge .p1_done

    mov x0, x21             // blueprint ptr
    mov w1, w20             // time limit
    bl max_geodes

    // quality = id * geodes
    ldr w2, [x21, #BP_ID]
    mul w0, w0, w2
    add w23, w23, w0

    add x21, x21, #BP_SIZE
    add w24, w24, #1
    b .p1_loop

.p1_done:
    mov w0, w23

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// solve_part2 - Product of geodes for first 3 blueprints
// w19 = time limit
// Returns: x0 = product
// ============================================================
solve_part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov w20, w19            // time limit
    adrp x21, blueprints@PAGE
    add x21, x21, blueprints@PAGEOFF  // current blueprint
    adrp x22, num_blueprints@PAGE
    add x22, x22, num_blueprints@PAGEOFF
    ldr w22, [x22]
    // min(num_blueprints, 3)
    mov w3, #3
    cmp w22, w3
    csel w22, w3, w22, gt   // if num_blueprints > 3, use 3, else use num_blueprints
    mov w23, #1             // product
    mov w24, #0             // index

.p2_loop:
    cmp w24, w22
    bge .p2_done

    mov x0, x21             // blueprint ptr
    mov w1, w20             // time limit
    bl max_geodes

    mul w23, w23, w0

    add x21, x21, #BP_SIZE
    add w24, w24, #1
    b .p2_loop

.p2_done:
    mov w0, w23

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// max_geodes - Find max geodes for a blueprint using DFS
// x0 = blueprint ptr
// w1 = time limit
// Returns: w0 = max geodes
// ============================================================
max_geodes:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    // Setup dfs_state in BSS
    adrp x10, dfs_state@PAGE
    add x10, x10, dfs_state@PAGEOFF

    ldr w2, [x0, #BP_ORE_ORE]
    str w2, [x10, #ST_ORE_ORE]
    ldr w3, [x0, #BP_CLAY_ORE]
    str w3, [x10, #ST_CLAY_ORE]
    ldr w4, [x0, #BP_OBS_ORE]
    str w4, [x10, #ST_OBS_ORE]
    ldr w5, [x0, #BP_OBS_CLAY]
    str w5, [x10, #ST_OBS_CLAY]
    ldr w6, [x0, #BP_GEO_ORE]
    str w6, [x10, #ST_GEO_ORE]
    ldr w7, [x0, #BP_GEO_OBS]
    str w7, [x10, #ST_GEO_OBS]

    str w1, [x10, #ST_TIME_LIM]  // time_limit
    str wzr, [x10, #ST_BEST]     // best = 0

    // max_ore = max(ore_ore, clay_ore, obs_ore, geo_ore)
    cmp w2, w3
    csel w8, w2, w3, gt
    cmp w8, w4
    csel w8, w8, w4, gt
    cmp w8, w6
    csel w8, w8, w6, gt
    str w8, [x10, #ST_MAX_ORE]

    // max_clay = obs_clay
    str w5, [x10, #ST_MAX_CLAY]

    // max_obs = geo_obs
    str w7, [x10, #ST_MAX_OBS]

    // Initial state: time=0, ore=0, clay=0, obs=0, geodes=0, oreR=1, clayR=0, obsR=0, geoR=0
    mov w0, #0              // time
    mov w1, #0              // ore
    mov w2, #0              // clay
    mov w3, #0              // obs
    mov w4, #0              // geodes
    mov w5, #1              // oreR
    mov w6, #0              // clayR
    mov w7, #0              // obsR
    mov w8, #0              // geoR
    bl dfs

    adrp x10, dfs_state@PAGE
    add x10, x10, dfs_state@PAGEOFF
    ldr w0, [x10, #ST_BEST]     // return best

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// dfs - Recursive DFS to find max geodes
// w0=time, w1=ore, w2=clay, w3=obs, w4=geodes
// w5=oreR, w6=clayR, w7=obsR, w8=geoR
// Uses dfs_state in BSS for blueprint data
// ============================================================
dfs:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    // Save state in callee-saved regs
    mov w19, w0             // time
    mov w20, w1             // ore
    mov w21, w2             // clay
    mov w22, w3             // obs
    mov w23, w4             // geodes
    mov w24, w5             // oreR
    mov w25, w6             // clayR
    mov w26, w7             // obsR
    mov w27, w8             // geoR

    // Get pointer to dfs_state
    adrp x28, dfs_state@PAGE
    add x28, x28, dfs_state@PAGEOFF

    // Load time_limit
    ldr w9, [x28, #ST_TIME_LIM]

    // Calculate remaining time
    sub w10, w9, w19        // remaining = time_limit - time

    // Upper bound pruning: geodes + geoR * remaining + remaining*(remaining-1)/2
    mul w11, w27, w10       // geoR * remaining
    add w11, w11, w23       // + geodes
    sub w12, w10, #1        // remaining - 1
    // Handle negative (if remaining is 0)
    cmp w12, #0
    csel w12, wzr, w12, lt
    mul w12, w10, w12       // remaining * (remaining-1)
    lsr w12, w12, #1        // / 2
    add w11, w11, w12       // upper_bound

    ldr w13, [x28, #ST_BEST]
    cmp w11, w13
    ble .dfs_ret            // prune if upper_bound <= best

    // Check if time == time_limit
    cmp w19, w9
    bne .dfs_not_done

    // Update best
    cmp w23, w13
    ble .dfs_ret
    str w23, [x28, #ST_BEST]     // best = geodes
    b .dfs_ret

.dfs_not_done:
    // Cap resources at remaining * max_needed
    ldr w14, [x28, #ST_MAX_ORE]
    mul w14, w14, w10       // remaining * max_ore
    cmp w20, w14
    csel w20, w14, w20, gt  // cappedOre = min(ore, cap)

    ldr w14, [x28, #ST_MAX_CLAY]
    mul w14, w14, w10
    cmp w21, w14
    csel w21, w14, w21, gt  // cappedClay

    ldr w14, [x28, #ST_MAX_OBS]
    mul w14, w14, w10
    cmp w22, w14
    csel w22, w14, w22, gt  // cappedObs

    // Collect resources (calculate new values)
    add w15, w20, w24       // newOre = cappedOre + oreR
    add w16, w21, w25       // newClay = cappedClay + clayR
    add w17, w22, w26       // newObs = cappedObs + obsR
    add w18, w23, w27       // newGeodes = geodes + geoR

    // Load geode robot costs
    ldr w9, [x28, #ST_GEO_ORE]
    ldr w10, [x28, #ST_GEO_OBS]

    // Check if can build geode robot
    cmp w20, w9             // ore >= geo_ore
    blt .try_obs
    cmp w22, w10            // obs >= geo_obs
    blt .try_obs

    // Build geode robot and return immediately (always do this)
    add w0, w19, #1         // time + 1
    sub w1, w15, w9         // newOre - geo_ore
    mov w2, w16             // newClay
    sub w3, w17, w10        // newObs - geo_obs
    mov w4, w18             // newGeodes
    mov w5, w24             // oreR
    mov w6, w25             // clayR
    mov w7, w26             // obsR
    add w8, w27, #1         // geoR + 1
    bl dfs
    b .dfs_ret

.try_obs:
    // Save new values for subsequent branches
    sub sp, sp, #32
    str w15, [sp, #0]       // newOre
    str w16, [sp, #4]       // newClay
    str w17, [sp, #8]       // newObs
    str w18, [sp, #12]      // newGeodes

    // Try building obsidian robot
    ldr w9, [x28, #ST_OBS_ORE]
    ldr w10, [x28, #ST_OBS_CLAY]
    ldr w11, [x28, #ST_MAX_OBS]

    cmp w26, w11            // obsR < max_obs
    bge .try_clay
    cmp w20, w9             // ore >= obs_ore
    blt .try_clay
    cmp w21, w10            // clay >= obs_clay
    blt .try_clay

    // Build obsidian robot
    add w0, w19, #1
    sub w1, w15, w9         // newOre - obs_ore
    sub w2, w16, w10        // newClay - obs_clay
    mov w3, w17
    mov w4, w18
    mov w5, w24
    mov w6, w25
    add w7, w26, #1         // obsR + 1
    mov w8, w27             // geoR
    bl dfs

.try_clay:
    // Reload saved values
    ldr w15, [sp, #0]
    ldr w16, [sp, #4]
    ldr w17, [sp, #8]
    ldr w18, [sp, #12]

    // Reload dfs_state pointer (may have been clobbered)
    adrp x28, dfs_state@PAGE
    add x28, x28, dfs_state@PAGEOFF

    // Try building clay robot
    ldr w9, [x28, #ST_CLAY_ORE]
    ldr w10, [x28, #ST_MAX_CLAY]

    cmp w25, w10            // clayR < max_clay
    bge .try_ore
    cmp w20, w9             // ore >= clay_ore
    blt .try_ore

    // Build clay robot
    add w0, w19, #1
    sub w1, w15, w9         // newOre - clay_ore
    mov w2, w16
    mov w3, w17
    mov w4, w18
    mov w5, w24
    add w6, w25, #1         // clayR + 1
    mov w7, w26
    mov w8, w27
    bl dfs

.try_ore:
    // Reload saved values
    ldr w15, [sp, #0]
    ldr w16, [sp, #4]
    ldr w17, [sp, #8]
    ldr w18, [sp, #12]

    // Reload dfs_state pointer
    adrp x28, dfs_state@PAGE
    add x28, x28, dfs_state@PAGEOFF

    // Try building ore robot
    ldr w9, [x28, #ST_ORE_ORE]
    ldr w10, [x28, #ST_MAX_ORE]

    cmp w24, w10            // oreR < max_ore
    bge .try_wait
    cmp w20, w9             // ore >= ore_ore
    blt .try_wait

    // Build ore robot
    add w0, w19, #1
    sub w1, w15, w9         // newOre - ore_ore
    mov w2, w16
    mov w3, w17
    mov w4, w18
    add w5, w24, #1         // oreR + 1
    mov w6, w25
    mov w7, w26
    mov w8, w27
    bl dfs

.try_wait:
    // Reload saved values
    ldr w15, [sp, #0]
    ldr w16, [sp, #4]
    ldr w17, [sp, #8]
    ldr w18, [sp, #12]

    // Do nothing (wait)
    add w0, w19, #1         // time + 1
    mov w1, w15             // newOre
    mov w2, w16             // newClay
    mov w3, w17             // newObs
    mov w4, w18             // newGeodes
    mov w5, w24             // oreR
    mov w6, w25             // clayR
    mov w7, w26             // obsR
    mov w8, w27             // geoR
    bl dfs

    add sp, sp, #32         // restore saved values space

.dfs_ret:
    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
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
    str x19, [sp, #-16]!

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

    ldr x19, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// print_number - Print integer
// x0 = number
// ============================================================
print_number:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #32

    mov x1, x0              // number
    add x2, sp, #31         // end of buffer
    strb wzr, [x2]          // null terminator

    // Handle zero
    cbnz x1, .pn_loop
    sub x2, x2, #1
    mov w3, #'0'
    strb w3, [x2]
    b .pn_print

.pn_loop:
    cbz x1, .pn_print
    mov x3, #10
    udiv x4, x1, x3
    msub x5, x4, x3, x1     // remainder
    add w5, w5, #'0'
    sub x2, x2, #1
    strb w5, [x2]
    mov x1, x4
    b .pn_loop

.pn_print:
    mov x0, x2
    bl print_string

    add sp, sp, #32
    ldp x29, x30, [sp], #16
    ret
