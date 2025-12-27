// Advent of Code 2023 Day 5: If You Give A Seed A Fertilizer - ARM64 Assembly for macOS
// This solution handles seed-to-location mapping with interval transformations

.global _main
.align 4

// Constants
.equ MAX_SEEDS, 32
.equ MAX_RANGES_PER_MAP, 64
.equ MAX_INTERVALS, 256
.equ NUM_MAPS, 7

.data
    .align 4
    filename: .asciz "../input.txt"
    msg_part1: .asciz "Part 1: "
    msg_part2: .asciz "Part 2: "
    newline: .asciz "\n"

    // Allocate space in data section (writable)
    .align 4
    buffer: .space 32768              // File contents
    num_buffer: .space 32             // For printing numbers
    seeds: .space 256                 // Up to 32 seeds (8 bytes each)
    seed_count: .quad 0               // Number of seeds

    // Maps storage: 7 maps
    // Each map has: count (8 bytes) followed by ranges (24 bytes each: dst, src, len)
    map0_count: .quad 0
    map0_ranges: .space 1536          // 64 * 24 bytes
    map1_count: .quad 0
    map1_ranges: .space 1536
    map2_count: .quad 0
    map2_ranges: .space 1536
    map3_count: .quad 0
    map3_ranges: .space 1536
    map4_count: .quad 0
    map4_ranges: .space 1536
    map5_count: .quad 0
    map5_ranges: .space 1536
    map6_count: .quad 0
    map6_ranges: .space 1536

    // For part 2: intervals to process (start, end pairs)
    intervals_a: .space 4096          // 256 * 16 bytes
    intervals_b: .space 4096          // 256 * 16 bytes
    intervals_a_count: .quad 0
    intervals_b_count: .quad 0

    // Temp arrays for apply_map_to_intervals (in data section for safety)
    remaining_arr: .space 2048        // 128 * 16 bytes for remaining intervals
    remaining_count: .quad 0
    new_remaining_arr: .space 2048    // 128 * 16 bytes for new remaining intervals
    new_remaining_count: .quad 0

.text

_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    bl read_file
    cbz x0, exit_error

    bl parse_input

    // Part 1
    adrp x0, msg_part1@PAGE
    add x0, x0, msg_part1@PAGEOFF
    bl print_str

    bl solve_part1
    bl print_num

    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2
    adrp x0, msg_part2@PAGE
    add x0, x0, msg_part2@PAGEOFF
    bl print_str

    bl solve_part2
    bl print_num

    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    mov x0, #0
    ldp x29, x30, [sp], #16
    mov x16, #1
    svc #0x80

exit_error:
    mov x0, #1
    ldp x29, x30, [sp], #16
    mov x16, #1
    svc #0x80

// Read file into buffer, returns size in x0
read_file:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    // Open file
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0              // O_RDONLY
    mov x16, #5             // SYS_OPEN
    svc #0x80
    cmp x0, #0
    b.lt read_error
    mov x19, x0             // Save fd

    // Read file
    mov x0, x19
    adrp x1, buffer@PAGE
    add x1, x1, buffer@PAGEOFF
    mov x2, #32768
    mov x16, #3             // SYS_READ
    svc #0x80
    mov x20, x0             // Save size

    // Close file
    mov x0, x19
    mov x16, #6             // SYS_CLOSE
    svc #0x80

    mov x0, x20
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

read_error:
    mov x0, #0
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Parse number from buffer
// Input: x0 = pointer to buffer
// Output: x0 = parsed number, x1 = pointer after number
parse_number:
    mov x2, x0              // current position
    mov x0, #0              // result

    // Skip spaces
1:  ldrb w3, [x2]
    cmp w3, #' '
    b.ne 2f
    add x2, x2, #1
    b 1b

2:  // Accumulate digits
    ldrb w3, [x2]
    cmp w3, #'0'
    b.lt 3f
    cmp w3, #'9'
    b.gt 3f

    mov x4, #10
    mul x0, x0, x4
    sub w3, w3, #'0'
    add x0, x0, x3
    add x2, x2, #1
    b 2b

3:  mov x1, x2
    ret

// Skip to character c (in w1), returns pointer after c
// Input: x0 = pointer, w1 = character to find
// Output: x0 = pointer after character
skip_to_char:
1:  ldrb w2, [x0]
    cbz w2, 2f
    cmp w2, w1
    b.eq 2f
    add x0, x0, #1
    b 1b
2:  add x0, x0, #1
    ret

// Skip whitespace (spaces, newlines)
// Input: x0 = pointer
// Output: x0 = updated pointer
skip_ws:
1:  ldrb w1, [x0]
    cmp w1, #' '
    b.eq 2f
    cmp w1, #'\n'
    b.eq 2f
    cmp w1, #'\t'
    b.eq 2f
    ret
2:  add x0, x0, #1
    b 1b

// Parse input: seeds and maps
parse_input:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    adrp x19, buffer@PAGE
    add x19, x19, buffer@PAGEOFF

    // Skip "seeds: "
    mov x0, x19
    mov w1, #':'
    bl skip_to_char
    bl skip_ws
    mov x19, x0

    // Parse seeds
    adrp x20, seeds@PAGE
    add x20, x20, seeds@PAGEOFF
    mov x21, #0             // seed count

parse_seeds_loop:
    ldrb w0, [x19]
    cmp w0, #'\n'
    b.eq seeds_done
    cbz w0, seeds_done
    cmp w0, #'0'
    b.lt seeds_done
    cmp w0, #'9'
    b.gt seeds_done

    mov x0, x19
    bl parse_number
    str x0, [x20, x21, lsl #3]
    add x21, x21, #1
    mov x19, x1
    mov x0, x19
    bl skip_ws
    mov x19, x0
    b parse_seeds_loop

seeds_done:
    adrp x0, seed_count@PAGE
    add x0, x0, seed_count@PAGEOFF
    str x21, [x0]

    // Set up map pointers in x22-x25
    // x22 = current map index (0-6)
    mov x22, #0

    // Parse 7 maps
parse_maps_loop:
    cmp x22, #7
    b.ge parse_complete

    // Get map count and ranges pointers based on map index
    // Calculate: count_ptr = map0_count + x22 * (8 + 1536)
    // ranges_ptr = map0_ranges + x22 * (8 + 1536)
    adrp x23, map0_count@PAGE
    add x23, x23, map0_count@PAGEOFF
    mov x0, #1544           // 8 + 1536
    mul x0, x22, x0
    add x23, x23, x0        // x23 = count pointer for this map
    add x24, x23, #8        // x24 = ranges pointer for this map

    // For map 0, x19 already points to header (after skip_ws skipped \n\n)
    // For maps 1-6, x19 points to the empty line (\n) between sections
    // We need to skip: empty line's \n, then header line
    cbz x22, skip_empty_line_done

    // Skip the empty line (just advance past the \n)
    add x19, x19, #1

skip_empty_line_done:
    // Skip the header line (e.g., "seed-to-soil map:")
    mov x0, x19
    mov w1, #'\n'
    bl skip_to_char
    mov x19, x0

    mov x25, #0             // range count for this map

parse_ranges_loop:
    // Check if at end of section
    ldrb w0, [x19]
    cbz w0, map_section_done
    cmp w0, #'\n'
    b.eq map_section_done
    cmp w0, #'0'
    b.lt map_section_done
    cmp w0, #'9'
    b.gt map_section_done

    // Parse dst_start
    mov x0, x19
    bl parse_number
    mov x26, x0             // dst_start
    mov x19, x1

    // Parse src_start
    mov x0, x19
    bl parse_number
    mov x27, x0             // src_start
    mov x19, x1

    // Parse length
    mov x0, x19
    bl parse_number
    // x0 = length
    mov x19, x1

    // Store range: [dst_start, src_start, length]
    mov x1, #24
    mul x1, x25, x1
    add x1, x24, x1
    str x26, [x1]           // dst_start
    str x27, [x1, #8]       // src_start
    str x0, [x1, #16]       // length

    add x25, x25, #1

    // Skip to next line
    mov x0, x19
    mov w1, #'\n'
    bl skip_to_char
    mov x19, x0
    b parse_ranges_loop

map_section_done:
    str x25, [x23]          // store range count

    add x22, x22, #1
    b parse_maps_loop

parse_complete:
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Apply a single map to a value
// Input: x0 = value, x1 = map count ptr
// Output: x0 = mapped value
apply_map:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0             // value
    ldr x20, [x1]           // number of ranges
    add x21, x1, #8         // ranges base pointer

    mov x22, #0             // range index

apply_map_loop:
    cmp x22, x20
    b.ge apply_map_not_found

    // Load range: dst_start, src_start, length
    mov x0, #24
    mul x0, x22, x0
    add x0, x21, x0

    ldr x1, [x0]            // dst_start
    ldr x2, [x0, #8]        // src_start
    ldr x3, [x0, #16]       // length

    // Check if value in range: src_start <= value < src_start + length
    cmp x19, x2
    b.lo next_range         // value < src_start
    add x4, x2, x3          // src_start + length
    cmp x19, x4
    b.hs next_range         // value >= src_start + length

    // Found: return dst_start + (value - src_start)
    sub x0, x19, x2
    add x0, x0, x1

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

next_range:
    add x22, x22, #1
    b apply_map_loop

apply_map_not_found:
    mov x0, x19             // return unchanged value

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Convert seed to location through all 7 maps
// Input: x0 = seed
// Output: x0 = location
seed_to_location:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0             // current value
    adrp x20, map0_count@PAGE
    add x20, x20, map0_count@PAGEOFF
    mov x21, #0             // map index

s2l_loop:
    cmp x21, #7
    b.ge s2l_done

    // Calculate map pointer
    mov x22, #1544          // 8 + 1536
    mul x22, x21, x22
    add x1, x20, x22

    mov x0, x19
    bl apply_map
    mov x19, x0

    add x21, x21, #1
    b s2l_loop

s2l_done:
    mov x0, x19

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 1: Find minimum location for all seeds
solve_part1:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    adrp x19, seeds@PAGE
    add x19, x19, seeds@PAGEOFF

    adrp x0, seed_count@PAGE
    add x0, x0, seed_count@PAGEOFF
    ldr x20, [x0]           // number of seeds

    mov x21, #0             // seed index
    mov x22, #-1            // min location (max value initially)

part1_loop:
    cmp x21, x20
    b.ge part1_done

    ldr x0, [x19, x21, lsl #3]
    bl seed_to_location

    cmp x0, x22
    csel x22, x0, x22, lo   // update min if lower

    add x21, x21, #1
    b part1_loop

part1_done:
    mov x0, x22

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Add interval to array
// x0 = start, x1 = end, x2 = array ptr, x3 = count ptr
add_interval:
    cmp x0, x1
    b.ge add_interval_skip  // skip empty intervals

    ldr x4, [x3]
    lsl x5, x4, #4          // offset = count * 16
    add x5, x2, x5
    str x0, [x5]
    str x1, [x5, #8]
    add x4, x4, #1
    str x4, [x3]

add_interval_skip:
    ret

// Apply one map to intervals
// x0 = map count ptr, x1 = input array, x2 = input count ptr, x3 = output array, x4 = output count ptr
// Uses static remaining_arr, remaining_count, new_remaining_arr, new_remaining_count in .data
apply_map_to_intervals:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    mov x19, x0             // map count ptr
    mov x20, x1             // input array
    mov x21, x2             // input count ptr
    mov x22, x3             // output array
    mov x23, x4             // output count ptr

    ldr x24, [x19]          // map range count
    add x25, x19, #8        // map ranges base

    str xzr, [x23]          // clear output count

    ldr x26, [x21]          // input interval count
    mov x27, #0             // input interval index

    // Get static buffer pointers
    adrp x9, remaining_arr@PAGE
    add x9, x9, remaining_arr@PAGEOFF      // x9 = remaining_arr
    adrp x10, remaining_count@PAGE
    add x10, x10, remaining_count@PAGEOFF  // x10 = remaining_count ptr
    adrp x11, new_remaining_arr@PAGE
    add x11, x11, new_remaining_arr@PAGEOFF    // x11 = new_remaining_arr
    adrp x12, new_remaining_count@PAGE
    add x12, x12, new_remaining_count@PAGEOFF  // x12 = new_remaining_count ptr

process_interval:
    cmp x27, x26
    b.ge intervals_done

    // Load interval
    lsl x0, x27, #4
    add x0, x20, x0
    ldr x13, [x0]            // interval start
    ldr x14, [x0, #8]        // interval end

    // Initialize remaining with this interval
    mov x0, #1
    str x0, [x10]           // remaining_count = 1
    str x13, [x9]           // remaining_arr[0].start
    str x14, [x9, #8]       // remaining_arr[0].end

    mov x28, #0             // map range index

process_map_range:
    cmp x28, x24
    b.ge add_remaining

    // Load map range
    mov x0, #24
    mul x0, x28, x0
    add x0, x25, x0
    ldr x13, [x0]           // dst_start
    ldr x14, [x0, #8]       // src_start
    ldr x15, [x0, #16]      // length
    add x16, x14, x15       // src_end

    // Process remaining intervals
    ldr x17, [x10]          // remaining count
    cbz x17, next_map_range

    str xzr, [x12]          // new remaining count = 0

    mov x18, #0             // remaining index

process_remaining_interval:
    cmp x18, x17
    b.ge copy_new_remaining

    // Load remaining interval
    lsl x0, x18, #4
    add x0, x9, x0
    ldr x5, [x0]            // r_start
    ldr x6, [x0, #8]        // r_end

    // Part before src_start (unmapped)
    cmp x5, x14
    b.hs skip_before_part
    cmp x6, x14
    csel x1, x6, x14, ls    // min(r_end, src_start)
    mov x0, x5
    // Add to new remaining
    ldr x2, [x12]           // new_remaining_count
    lsl x3, x2, #4
    add x3, x11, x3
    str x0, [x3]
    str x1, [x3, #8]
    add x2, x2, #1
    str x2, [x12]

skip_before_part:
    // Overlapping part (mapped)
    cmp x5, x14
    csel x7, x5, x14, hi    // overlap_start = max(r_start, src_start)
    cmp x6, x16
    csel x8, x6, x16, lo    // overlap_end = min(r_end, src_end)

    cmp x7, x8
    b.hs skip_overlap_part

    // Save values before function call (x5, x6, x9-x18 are caller-saved)
    sub sp, sp, #96
    stp x5, x6, [sp]
    stp x9, x10, [sp, #16]
    stp x11, x12, [sp, #32]
    stp x13, x14, [sp, #48]
    stp x15, x16, [sp, #64]
    stp x17, x18, [sp, #80]

    // Map and add to output
    sub x0, x13, x14        // offset = dst_start - src_start
    add x0, x7, x0          // mapped_start = overlap_start + offset
    sub x1, x13, x14
    add x1, x8, x1          // mapped_end = overlap_end + offset
    mov x2, x22
    mov x3, x23
    bl add_interval

    // Restore values
    ldp x5, x6, [sp]
    ldp x9, x10, [sp, #16]
    ldp x11, x12, [sp, #32]
    ldp x13, x14, [sp, #48]
    ldp x15, x16, [sp, #64]
    ldp x17, x18, [sp, #80]
    add sp, sp, #96

skip_overlap_part:
    // Part after src_end (unmapped)
    cmp x6, x16
    b.ls skip_after_part
    cmp x5, x16
    csel x0, x5, x16, hi    // max(r_start, src_end)
    mov x1, x6
    // Add to new remaining
    ldr x2, [x12]
    lsl x3, x2, #4
    add x3, x11, x3
    str x0, [x3]
    str x1, [x3, #8]
    add x2, x2, #1
    str x2, [x12]

skip_after_part:
    add x18, x18, #1
    b process_remaining_interval

copy_new_remaining:
    // Copy new remaining to remaining
    ldr x17, [x12]
    str x17, [x10]

    mov x18, #0
copy_rem_loop:
    cmp x18, x17
    b.ge next_map_range

    lsl x0, x18, #4
    add x1, x11, x0         // src = new_remaining_arr + offset
    ldr x2, [x1]
    ldr x3, [x1, #8]

    add x0, x9, x0          // dst = remaining_arr + offset
    str x2, [x0]
    str x3, [x0, #8]

    add x18, x18, #1
    b copy_rem_loop

next_map_range:
    add x28, x28, #1
    b process_map_range

add_remaining:
    // Add remaining intervals to output (identity mapped)
    ldr x17, [x10]
    mov x18, #0

add_rem_loop:
    cmp x18, x17
    b.ge next_interval

    lsl x0, x18, #4
    add x0, x9, x0
    ldr x5, [x0]            // start
    ldr x6, [x0, #8]        // end

    // Save x9, x10, x17, x18 before function call
    sub sp, sp, #32
    stp x9, x10, [sp]
    stp x17, x18, [sp, #16]

    mov x0, x5
    mov x1, x6
    mov x2, x22
    mov x3, x23
    bl add_interval

    // Restore x9, x10, x17, x18
    ldp x9, x10, [sp]
    ldp x17, x18, [sp, #16]
    add sp, sp, #32

    add x18, x18, #1
    b add_rem_loop

next_interval:
    add x27, x27, #1
    b process_interval

intervals_done:
    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2: Treat seeds as ranges
solve_part2:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    // Convert seed pairs to intervals in intervals_a
    adrp x19, seeds@PAGE
    add x19, x19, seeds@PAGEOFF

    adrp x0, seed_count@PAGE
    add x0, x0, seed_count@PAGEOFF
    ldr x20, [x0]

    adrp x21, intervals_a@PAGE
    add x21, x21, intervals_a@PAGEOFF

    adrp x22, intervals_a_count@PAGE
    add x22, x22, intervals_a_count@PAGEOFF

    str xzr, [x22]          // clear count

    mov x23, #0             // seed index

convert_seeds_loop:
    cmp x23, x20
    b.ge seeds_converted

    ldr x0, [x19, x23, lsl #3]      // start
    add x1, x23, #1
    ldr x1, [x19, x1, lsl #3]       // length
    add x1, x0, x1                   // end = start + length

    mov x2, x21
    mov x3, x22
    bl add_interval

    add x23, x23, #2
    b convert_seeds_loop

seeds_converted:
    // Now apply all 7 maps
    // We alternate between intervals_a and intervals_b
    adrp x19, map0_count@PAGE
    add x19, x19, map0_count@PAGEOFF

    adrp x20, intervals_a@PAGE
    add x20, x20, intervals_a@PAGEOFF
    adrp x21, intervals_a_count@PAGE
    add x21, x21, intervals_a_count@PAGEOFF

    adrp x22, intervals_b@PAGE
    add x22, x22, intervals_b@PAGEOFF
    adrp x23, intervals_b_count@PAGE
    add x23, x23, intervals_b_count@PAGEOFF

    mov x24, #0             // map index

apply_maps_loop:
    cmp x24, #7
    b.ge maps_done

    // Calculate map pointer
    mov x0, #1544
    mul x0, x24, x0
    add x0, x19, x0

    // Determine input/output based on even/odd map index
    tst x24, #1
    b.ne use_b_as_input

    // Even: input=a, output=b
    mov x1, x20
    mov x2, x21
    mov x3, x22
    mov x4, x23
    b do_apply_map

use_b_as_input:
    // Odd: input=b, output=a
    mov x1, x22
    mov x2, x23
    mov x3, x20
    mov x4, x21

do_apply_map:
    bl apply_map_to_intervals

    add x24, x24, #1
    b apply_maps_loop

maps_done:
    // Result is in intervals_a (since 7 is odd, last output was to a)
    // Actually, map 0 (even) -> output to b
    // map 1 (odd) -> output to a
    // map 2 (even) -> output to b
    // map 3 (odd) -> output to a
    // map 4 (even) -> output to b
    // map 5 (odd) -> output to a
    // map 6 (even) -> output to b
    // So final result is in intervals_b!

    ldr x24, [x23]          // count from intervals_b

    mov x25, #-1            // min (max value)
    mov x26, #0             // index

find_min_loop:
    cmp x26, x24
    b.ge find_min_done

    lsl x0, x26, #4
    add x0, x22, x0
    ldr x0, [x0]            // interval start

    cmp x0, x25
    csel x25, x0, x25, lo

    add x26, x26, #1
    b find_min_loop

find_min_done:
    mov x0, x25

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print string at x0
print_str:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    mov x19, x0

    // Find length
    mov x1, #0
1:  ldrb w2, [x19, x1]
    cbz w2, 2f
    add x1, x1, #1
    b 1b

2:  mov x2, x1
    mov x1, x19
    mov x0, #1              // stdout
    mov x16, #4             // SYS_WRITE
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print number in x0
print_num:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    adrp x20, num_buffer@PAGE
    add x20, x20, num_buffer@PAGEOFF
    add x20, x20, #30
    strb wzr, [x20]

    mov x2, #10
1:  sub x20, x20, #1
    udiv x3, x19, x2
    msub x4, x3, x2, x19
    add w4, w4, #'0'
    strb w4, [x20]
    mov x19, x3
    cbnz x19, 1b

    mov x0, x20
    bl print_str

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
