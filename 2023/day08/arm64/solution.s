// ============================================================================
// Advent of Code 2023 - Day 8: Haunted Wasteland
// ARM64 Assembly Solution for macOS
// ============================================================================
//
// ALGORITHM:
// Part 1: Navigate from AAA to ZZZ following L/R instructions
// Part 2: Find cycle lengths for nodes ending in 'A', compute LCM
//
// Node encoding: 3-char names encoded as base-36 number
// Chars: 0-9 -> 0-9, A-Z -> 10-35
// Max index = 36^3 = 46656
//
// ============================================================================

.global _start
.align 4

// ============================================================================
// DATA SECTION - Read-only strings
// ============================================================================
.section __DATA,__data
    .align 4
    filename:   .asciz "../input.txt"
    msg_part1:  .asciz "Part 1: "
    msg_part2:  .asciz "Part 2: "
    newline:    .asciz "\n"

// ============================================================================
// BSS SECTION - Writable uninitialized data
// ============================================================================
.section __DATA,__bss
    .align 4
    buffer:     .space 20480          // Input file buffer

    .align 4
    num_buffer: .space 32             // Number printing buffer

    .align 4
    instructions:   .space 512        // L/R instruction sequence
    instr_len:      .space 8          // Length of instructions

    // Node network: each entry is 8 bytes (left index: 4 bytes, right index: 4 bytes)
    // Max index = 36^3 = 46656
    .align 4
    network:    .space 373248         // 46656 * 8 bytes

    // List of starting nodes (ending with 'A') for Part 2
    .align 4
    start_nodes:    .space 64         // Up to 16 starting nodes, 4 bytes each
    start_count:    .space 8

    // Cycle lengths for Part 2
    .align 4
    cycle_lengths:  .space 128        // Up to 16 cycle lengths, 8 bytes each

// ============================================================================
// TEXT SECTION
// ============================================================================
.text

_start:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    bl      read_file
    cbz     x0, exit_error

    bl      parse_input

    // Part 1
    adrp    x0, msg_part1@PAGE
    add     x0, x0, msg_part1@PAGEOFF
    bl      print_str

    bl      part1
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // Part 2
    adrp    x0, msg_part2@PAGE
    add     x0, x0, msg_part2@PAGEOFF
    bl      print_str

    bl      part2
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    mov     x0, #0
    ldp     x29, x30, [sp], #16
    mov     x16, #1
    svc     #0x80

exit_error:
    mov     x0, #1
    ldp     x29, x30, [sp], #16
    mov     x16, #1
    svc     #0x80

// ============================================================================
// FILE I/O
// ============================================================================
read_file:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    adrp    x0, filename@PAGE
    add     x0, x0, filename@PAGEOFF
    mov     x1, #0                    // O_RDONLY
    mov     x16, #5                   // open
    svc     #0x80
    cmp     x0, #0
    b.lt    read_error
    mov     x19, x0                   // fd

    mov     x0, x19
    adrp    x1, buffer@PAGE
    add     x1, x1, buffer@PAGEOFF
    mov     x2, #20480
    mov     x16, #3                   // read
    svc     #0x80
    mov     x20, x0                   // bytes read

    mov     x0, x19
    mov     x16, #6                   // close
    svc     #0x80

    mov     x0, x20
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

read_error:
    mov     x0, #0
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// ENCODING
// ============================================================================

// encode_node: Encode a 3-letter node name to an index
// Input: x0 = pointer to 3 characters
// Output: x0 = encoded index (0-46655)
encode_node:
    ldrb    w1, [x0]
    ldrb    w2, [x0, #1]
    ldrb    w3, [x0, #2]

    // Convert each char to value 0-35
    // '0'-'9' (48-57) -> 0-9
    // 'A'-'Z' (65-90) -> 10-35
    cmp     w1, #'A'
    b.ge    1f
    sub     w1, w1, #'0'              // digit
    b       2f
1:  sub     w1, w1, #'A'
    add     w1, w1, #10
2:

    cmp     w2, #'A'
    b.ge    3f
    sub     w2, w2, #'0'
    b       4f
3:  sub     w2, w2, #'A'
    add     w2, w2, #10
4:

    cmp     w3, #'A'
    b.ge    5f
    sub     w3, w3, #'0'
    b       6f
5:  sub     w3, w3, #'A'
    add     w3, w3, #10
6:

    // index = c1 * 36 * 36 + c2 * 36 + c3
    mov     w4, #36
    mul     w1, w1, w4
    add     w1, w1, w2
    mul     w1, w1, w4
    add     w0, w1, w3

    ret

// ============================================================================
// PARSING
// ============================================================================

// parse_input: Parse the input file
//
// Register allocation:
//   x19 = buffer pointer (current position)
//   x20 = instructions base address
//   x21 = instruction length counter
//   x22 = network base address
//   x23 = start_nodes base address
//   x24 = start node count
//   x25 = current node index
//   x26 = left node index
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    adrp    x19, buffer@PAGE
    add     x19, x19, buffer@PAGEOFF

    // Parse instructions (first line)
    adrp    x20, instructions@PAGE
    add     x20, x20, instructions@PAGEOFF
    mov     x21, #0                   // instruction length

parse_instr_loop:
    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.eq    parse_instr_done
    cmp     w0, #'L'
    b.eq    store_instr
    cmp     w0, #'R'
    b.eq    store_instr
    b       parse_instr_done

store_instr:
    strb    w0, [x20, x21]
    add     x21, x21, #1
    add     x19, x19, #1
    b       parse_instr_loop

parse_instr_done:
    // Store instruction length
    adrp    x0, instr_len@PAGE
    add     x0, x0, instr_len@PAGEOFF
    str     x21, [x0]

    // Skip to next line (skip newline and blank line)
    add     x19, x19, #1              // skip '\n'
skip_blank:
    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.ne    parse_nodes_start
    add     x19, x19, #1
    b       skip_blank

parse_nodes_start:
    adrp    x22, network@PAGE
    add     x22, x22, network@PAGEOFF

    adrp    x23, start_nodes@PAGE
    add     x23, x23, start_nodes@PAGEOFF
    mov     x24, #0                   // start node count

parse_node_loop:
    ldrb    w0, [x19]
    cbz     w0, parse_done
    cmp     w0, #'\n'
    b.eq    skip_newline_parse

    // Check if this is a valid node line (first char should be alphanumeric)
    cmp     w0, #'0'
    b.lt    skip_newline_parse
    cmp     w0, #'9'
    b.le    valid_node
    cmp     w0, #'A'
    b.lt    skip_newline_parse
    cmp     w0, #'Z'
    b.gt    skip_newline_parse

valid_node:
    // Parse: XXX = (YYY, ZZZ)
    // Current position at node name
    mov     x0, x19
    bl      encode_node
    mov     x25, x0                   // node index

    // Check if node ends with 'A' (for Part 2)
    ldrb    w0, [x19, #2]
    cmp     w0, #'A'
    b.ne    not_start_node
    // Store as starting node
    lsl     x0, x24, #2
    add     x0, x23, x0
    str     w25, [x0]
    add     x24, x24, #1

not_start_node:
    // Skip "XXX = ("
    add     x19, x19, #7

    // Parse left node
    mov     x0, x19
    bl      encode_node
    mov     x26, x0                   // left index

    // Skip "YYY, "
    add     x19, x19, #5

    // Parse right node
    mov     x0, x19
    bl      encode_node
    // x0 = right index

    // Store in network: network[node_idx] = (left, right)
    lsl     x1, x25, #3               // offset = node_idx * 8
    add     x1, x22, x1
    str     w26, [x1]                 // left at offset 0
    str     w0, [x1, #4]              // right at offset 4

    // Skip "ZZZ)"
    add     x19, x19, #4

next_line:
    // Skip to next line
    ldrb    w0, [x19]
    cbz     w0, parse_done
    cmp     w0, #'\n'
    b.eq    skip_newline_parse
    add     x19, x19, #1
    b       next_line

skip_newline_parse:
    add     x19, x19, #1
    b       parse_node_loop

parse_done:
    // Store start node count
    adrp    x0, start_count@PAGE
    add     x0, x0, start_count@PAGEOFF
    str     x24, [x0]

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// PART 1
// ============================================================================

// part1: Navigate from AAA to ZZZ
// Output: x0 = number of steps
//
// Register allocation:
//   x19 = network base address
//   x20 = instructions base address
//   x21 = instruction length
//   x22 = current node index
//   x23 = target node index (ZZZ)
//   x24 = step counter
part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    adrp    x19, network@PAGE
    add     x19, x19, network@PAGEOFF

    adrp    x20, instructions@PAGE
    add     x20, x20, instructions@PAGEOFF

    adrp    x0, instr_len@PAGE
    add     x0, x0, instr_len@PAGEOFF
    ldr     x21, [x0]                 // instruction length

    // AAA encoded: A=10, so 10*36*36 + 10*36 + 10 = 13330
    mov     x22, #13330               // current node = AAA

    // ZZZ encoded: Z=35, so 35*36*36 + 35*36 + 35 = 46655
    mov     x23, #46655               // target = ZZZ

    mov     x24, #0                   // steps

part1_loop:
    cmp     x22, x23
    b.eq    part1_done

    // Get instruction
    udiv    x0, x24, x21
    msub    x0, x0, x21, x24          // step % instr_len
    ldrb    w1, [x20, x0]             // 'L' or 'R'

    // Get network entry for current node
    lsl     x0, x22, #3
    add     x0, x19, x0

    cmp     w1, #'L'
    b.eq    part1_go_left
    ldr     w22, [x0, #4]             // right
    b       part1_step_done

part1_go_left:
    ldr     w22, [x0]                 // left

part1_step_done:
    add     x24, x24, #1
    b       part1_loop

part1_done:
    mov     x0, x24

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// PART 2
// ============================================================================

// Check if node ends with 'Z'
// Input: x0 = node index
// Output: x0 = 1 if ends with Z, 0 otherwise
//
// Node encoding: index = c1*36*36 + c2*36 + c3
// Last char c3 = index % 36; Z='Z'-'A'+10 = 35
ends_with_z:
    mov     x1, #36
    udiv    x2, x0, x1
    msub    x0, x2, x1, x0            // c3 = index % 36
    cmp     x0, #35                   // Z = 35
    cset    x0, eq
    ret

// find_cycle_length: Find steps to reach a node ending in 'Z'
// Input: x0 = starting node index
// Output: x0 = cycle length
//
// Register allocation:
//   x19 = network base address
//   x20 = instructions base address
//   x21 = instruction length
//   x22 = current node index
//   x23 = step counter
find_cycle_length:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    str     x23, [sp, #-16]!

    mov     x22, x0                   // current node

    adrp    x19, network@PAGE
    add     x19, x19, network@PAGEOFF

    adrp    x20, instructions@PAGE
    add     x20, x20, instructions@PAGEOFF

    adrp    x0, instr_len@PAGE
    add     x0, x0, instr_len@PAGEOFF
    ldr     x21, [x0]                 // instruction length

    mov     x23, #0                   // steps

find_cycle_loop:
    // Check if current ends with 'Z'
    mov     x0, x22
    bl      ends_with_z
    cbnz    x0, find_cycle_done

    // Get instruction
    udiv    x0, x23, x21
    msub    x0, x0, x21, x23          // step % instr_len
    ldrb    w1, [x20, x0]             // 'L' or 'R'

    // Get network entry for current node
    lsl     x0, x22, #3
    add     x0, x19, x0

    cmp     w1, #'L'
    b.eq    cycle_go_left
    ldr     w22, [x0, #4]             // right
    b       cycle_step_done

cycle_go_left:
    ldr     w22, [x0]                 // left

cycle_step_done:
    add     x23, x23, #1
    b       find_cycle_loop

find_cycle_done:
    mov     x0, x23

    ldr     x23, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// gcd: Compute GCD of two numbers
// Input: x0 = a, x1 = b
// Output: x0 = gcd(a, b)
gcd:
    cbz     x1, gcd_done
gcd_loop:
    udiv    x2, x0, x1
    msub    x2, x2, x1, x0            // a % b
    mov     x0, x1
    mov     x1, x2
    cbnz    x1, gcd_loop
gcd_done:
    ret

// lcm: Compute LCM of two numbers
// Input: x0 = a, x1 = b
// Output: x0 = lcm(a, b)
lcm:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0                   // a
    mov     x20, x1                   // b

    bl      gcd
    // x0 = gcd(a, b)

    // lcm = a * b / gcd
    mul     x1, x19, x20              // a * b
    udiv    x0, x1, x0                // / gcd

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// part2: Find LCM of all cycle lengths
// Output: x0 = steps until all paths end on 'Z'
//
// Register allocation:
//   x19 = start_nodes base address
//   x20 = number of starting nodes
//   x21 = cycle_lengths base address
//   x22 = loop counter (i)
//   x23 = running LCM result
part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    str     x23, [sp, #-16]!

    adrp    x19, start_nodes@PAGE
    add     x19, x19, start_nodes@PAGEOFF

    adrp    x0, start_count@PAGE
    add     x0, x0, start_count@PAGEOFF
    ldr     x20, [x0]                 // number of starting nodes

    adrp    x21, cycle_lengths@PAGE
    add     x21, x21, cycle_lengths@PAGEOFF

    // Find cycle length for each starting node
    mov     x22, #0                   // i
find_all_cycles:
    cmp     x22, x20
    b.ge    compute_lcm

    lsl     x0, x22, #2
    ldr     w0, [x19, x0]             // start_nodes[i]
    bl      find_cycle_length

    // Store cycle length
    lsl     x1, x22, #3
    str     x0, [x21, x1]

    add     x22, x22, #1
    b       find_all_cycles

compute_lcm:
    // LCM of all cycle lengths
    ldr     x23, [x21]                // result = cycle_lengths[0]
    mov     x22, #1                   // i = 1

lcm_loop:
    cmp     x22, x20
    b.ge    part2_done

    lsl     x0, x22, #3
    ldr     x1, [x21, x0]             // cycle_lengths[i]
    mov     x0, x23
    bl      lcm
    mov     x23, x0

    add     x22, x22, #1
    b       lcm_loop

part2_done:
    mov     x0, x23

    ldr     x23, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// OUTPUT
// ============================================================================

print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    mov     x19, x0

    mov     x1, #0
strlen_loop:
    ldrb    w2, [x19, x1]
    cbz     w2, strlen_done
    add     x1, x1, #1
    b       strlen_loop

strlen_done:
    mov     x2, x1                    // length
    mov     x1, x19                   // string pointer
    mov     x0, #1                    // stdout
    mov     x16, #4                   // write syscall
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    adrp    x20, num_buffer@PAGE
    add     x20, x20, num_buffer@PAGEOFF
    add     x20, x20, #30             // Start at end of buffer
    strb    wzr, [x20]                // Null terminator

    cbz     x19, print_num_zero

    mov     x2, #10
convert_digit_loop:
    sub     x20, x20, #1
    udiv    x3, x19, x2               // quotient
    msub    x4, x3, x2, x19           // remainder = n - (quotient * 10)
    add     w4, w4, #'0'              // Convert to ASCII
    strb    w4, [x20]
    mov     x19, x3
    cbnz    x19, convert_digit_loop
    b       print_num_output

print_num_zero:
    sub     x20, x20, #1
    mov     w4, #'0'
    strb    w4, [x20]

print_num_output:
    mov     x0, x20
    bl      print_str

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
