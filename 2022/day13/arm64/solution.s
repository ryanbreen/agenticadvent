// Day 13: Distress Signal - ARM64 Assembly (macOS)
//
// Algorithm:
//   Parse nested list structures from input
//   Part 1: Sum indices of pairs in correct order
//   Part 2: Sort all packets with [[2]] and [[6]] dividers, return product of positions
//
// Node structure:
//   type (8 bytes): 0=integer, 1=list
//   value (8 bytes): integer value if type=0, child count if type=1
//   children[0..n-1] (8 bytes each): pointers to child nodes (only if list)
//
// Each node is 16 bytes minimum, with additional space for list children

.global _start
.align 4

// Constants
.equ BUFFER_SIZE, 32768         // Input file buffer
.equ NODE_POOL_SIZE, 1048576    // Memory pool for nodes (1MB)
.equ MAX_PACKETS, 512           // Maximum number of packets
.equ NODE_TYPE, 0               // Offset for node type
.equ NODE_VALUE, 8              // Offset for value/count
.equ NODE_CHILDREN, 16          // Offset for first child pointer

// Macro for loading addresses
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
error_msg:      .asciz "Error reading file\n"

.align 3
file_buffer:    .space BUFFER_SIZE
buffer_len:     .quad 0
buffer_pos:     .quad 0         // Current parse position

// Node memory pool
.align 4
node_pool:      .space NODE_POOL_SIZE
node_pool_ptr:  .quad 0         // Next free position in pool

// Packet pointers array
.align 3
packets:        .space MAX_PACKETS * 8
packet_count:   .quad 0

// Divider packet pointers
divider1:       .quad 0         // [[2]]
divider2:       .quad 0         // [[6]]

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

    LOAD_ADDR x1, buffer_len
    str     x0, [x1]

    // Close file
    mov     x0, x19
    mov     x16, #6                         // close() syscall
    svc     #0x80

    // Initialize node pool
    LOAD_ADDR x0, node_pool
    LOAD_ADDR x1, node_pool_ptr
    str     x0, [x1]

    // Parse all packets
    bl      parse_all_packets

    // Part 1: Sum indices of pairs in correct order
    bl      part1
    mov     x19, x0                         // Save part1 result

    // Print Part 1
    LOAD_ADDR x0, part1_msg
    bl      print_str
    mov     x0, x19
    bl      print_num
    LOAD_ADDR x0, newline
    bl      print_str

    // Part 2: Sort and find decoder key
    bl      part2
    mov     x20, x0                         // Save part2 result

    // Print Part 2
    LOAD_ADDR x0, part2_msg
    bl      print_str
    mov     x0, x20
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
// alloc_node: Allocate a node from the pool
// Input: x0 = size in bytes
// Output: x0 = pointer to allocated memory
// ============================================================================
alloc_node:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    LOAD_ADDR x1, node_pool_ptr
    ldr     x2, [x1]                        // Current pointer
    mov     x19, x2                         // Save for return

    add     x2, x2, x0                      // Advance pointer
    // Align to 8 bytes
    add     x2, x2, #7
    and     x2, x2, #~7

    str     x2, [x1]                        // Store new pointer

    mov     x0, x19

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// parse_all_packets: Parse all packets from input
// ============================================================================
parse_all_packets:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    LOAD_ADDR x19, buffer_pos
    str     xzr, [x19]                      // Reset position

    LOAD_ADDR x20, packets
    mov     x21, #0                         // Packet count

parse_packets_loop:
    // Skip whitespace
    bl      skip_whitespace

    // Check if at end
    LOAD_ADDR x0, buffer_pos
    ldr     x1, [x0]
    LOAD_ADDR x0, buffer_len
    ldr     x2, [x0]
    cmp     x1, x2
    b.ge    parse_packets_done

    // Check if we have a '['
    LOAD_ADDR x0, file_buffer
    ldrb    w1, [x0, x1]
    cmp     w1, #'['
    b.ne    parse_packets_done

    // Parse a packet
    bl      parse_node

    // Store packet pointer
    str     x0, [x20, x21, lsl #3]
    add     x21, x21, #1

    b       parse_packets_loop

parse_packets_done:
    LOAD_ADDR x0, packet_count
    str     x21, [x0]

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// skip_whitespace: Skip whitespace characters
// ============================================================================
skip_whitespace:
    stp     x29, x30, [sp, #-16]!

    LOAD_ADDR x0, buffer_pos
    ldr     x1, [x0]
    LOAD_ADDR x2, buffer_len
    ldr     x3, [x2]
    LOAD_ADDR x4, file_buffer

skip_ws_loop:
    cmp     x1, x3
    b.ge    skip_ws_done
    ldrb    w5, [x4, x1]
    cmp     w5, #' '
    b.eq    skip_ws_next
    cmp     w5, #'\t'
    b.eq    skip_ws_next
    cmp     w5, #'\n'
    b.eq    skip_ws_next
    cmp     w5, #'\r'
    b.eq    skip_ws_next
    b       skip_ws_done

skip_ws_next:
    add     x1, x1, #1
    b       skip_ws_loop

skip_ws_done:
    LOAD_ADDR x0, buffer_pos
    str     x1, [x0]

    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// parse_node: Parse a node (list or integer)
// Output: x0 = pointer to parsed node
// ============================================================================
parse_node:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    // Get current character
    LOAD_ADDR x0, buffer_pos
    ldr     x1, [x0]
    LOAD_ADDR x2, file_buffer
    ldrb    w3, [x2, x1]

    cmp     w3, #'['
    b.eq    parse_list

    // Parse integer
    bl      parse_integer
    b       parse_node_done

parse_list:
    bl      parse_list_node

parse_node_done:
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// parse_integer: Parse an integer value
// Output: x0 = pointer to integer node
// ============================================================================
parse_integer:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    // Parse the number
    LOAD_ADDR x19, buffer_pos
    ldr     x20, [x19]
    LOAD_ADDR x21, file_buffer

    mov     x22, #0                         // Accumulated value

parse_int_loop:
    ldrb    w0, [x21, x20]
    cmp     w0, #'0'
    b.lt    parse_int_done
    cmp     w0, #'9'
    b.gt    parse_int_done

    sub     w0, w0, #'0'
    and     x0, x0, #0xFF
    mov     x1, #10
    mul     x22, x22, x1
    add     x22, x22, x0
    add     x20, x20, #1
    b       parse_int_loop

parse_int_done:
    str     x20, [x19]                      // Update position

    // Allocate node
    mov     x0, #16                         // type + value
    bl      alloc_node
    mov     x19, x0

    // Set type to integer (0)
    str     xzr, [x19, #NODE_TYPE]
    str     x22, [x19, #NODE_VALUE]

    mov     x0, x19

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// parse_list_node: Parse a list [...]
// Output: x0 = pointer to list node
// ============================================================================
parse_list_node:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    // Skip '['
    LOAD_ADDR x19, buffer_pos
    ldr     x0, [x19]
    add     x0, x0, #1
    str     x0, [x19]

    // Temporary storage for children (up to 32 children on stack)
    sub     sp, sp, #256                    // 32 * 8 bytes
    mov     x20, sp                         // children array
    mov     x21, #0                         // child count

    // Parse list contents
parse_list_loop:
    // Get current character
    ldr     x0, [x19]
    LOAD_ADDR x1, file_buffer
    ldrb    w2, [x1, x0]

    cmp     w2, #']'
    b.eq    parse_list_end

    cmp     w2, #','
    b.ne    parse_list_elem

    // Skip comma
    add     x0, x0, #1
    str     x0, [x19]
    b       parse_list_loop

parse_list_elem:
    // Parse child node
    bl      parse_node

    // Store child pointer
    str     x0, [x20, x21, lsl #3]
    add     x21, x21, #1
    b       parse_list_loop

parse_list_end:
    // Skip ']'
    ldr     x0, [x19]
    add     x0, x0, #1
    str     x0, [x19]

    // Allocate list node (16 + 8*children)
    mov     x0, #16
    mov     x1, x21
    lsl     x1, x1, #3
    add     x0, x0, x1
    bl      alloc_node
    mov     x22, x0                         // node pointer

    // Set type to list (1)
    mov     x0, #1
    str     x0, [x22, #NODE_TYPE]
    str     x21, [x22, #NODE_VALUE]         // child count

    // Copy children
    mov     x23, #0
copy_children:
    cmp     x23, x21
    b.ge    copy_children_done
    ldr     x0, [x20, x23, lsl #3]
    add     x1, x22, #NODE_CHILDREN
    str     x0, [x1, x23, lsl #3]
    add     x23, x23, #1
    b       copy_children

copy_children_done:
    add     sp, sp, #256
    mov     x0, x22

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// compare: Compare two nodes
// Input: x0 = left node, x1 = right node
// Output: x0 = -1 (left < right), 0 (equal), 1 (left > right)
// ============================================================================
compare:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0                         // left
    mov     x20, x1                         // right

    // Get types
    ldr     x21, [x19, #NODE_TYPE]          // left type
    ldr     x22, [x20, #NODE_TYPE]          // right type

    // Both integers?
    orr     x0, x21, x22
    cbnz    x0, not_both_int

    // Compare integers
    ldr     x0, [x19, #NODE_VALUE]
    ldr     x1, [x20, #NODE_VALUE]
    cmp     x0, x1
    b.lt    cmp_less
    b.gt    cmp_greater
    mov     x0, #0
    b       compare_done

not_both_int:
    // Both lists?
    cmp     x21, #1
    b.ne    left_is_int
    cmp     x22, #1
    b.ne    right_is_int

    // Both are lists - compare element by element
    ldr     x23, [x19, #NODE_VALUE]         // left count
    ldr     x24, [x20, #NODE_VALUE]         // right count
    mov     x25, #0                         // index

compare_lists_loop:
    // Check if we've exhausted either list
    cmp     x25, x23
    b.ge    left_exhausted
    cmp     x25, x24
    b.ge    right_exhausted

    // Compare elements at index x25
    add     x0, x19, #NODE_CHILDREN
    ldr     x0, [x0, x25, lsl #3]           // left child
    add     x1, x20, #NODE_CHILDREN
    ldr     x1, [x1, x25, lsl #3]           // right child
    bl      compare

    cbnz    x0, compare_done                // If not equal, return result

    add     x25, x25, #1
    b       compare_lists_loop

left_exhausted:
    cmp     x25, x24
    b.lt    cmp_less                        // Left shorter = correct order
    mov     x0, #0                          // Same length = equal
    b       compare_done

right_exhausted:
    b       cmp_greater                     // Right shorter = wrong order

left_is_int:
    // Left is int, right is list
    // Instead of allocating, handle directly: compare [left_int] vs right_list
    ldr     x23, [x20, #NODE_VALUE]         // right list count

    // If right is empty, left > right
    cbz     x23, cmp_greater

    // Compare left_int with right[0]
    add     x0, x20, #NODE_CHILDREN
    ldr     x1, [x0]                        // right first child
    mov     x0, x19                         // left int
    bl      compare

    // If not equal, return result
    cbnz    x0, compare_done

    // If equal and right has more elements, left < right
    ldr     x23, [x20, #NODE_VALUE]
    cmp     x23, #1
    b.gt    cmp_less

    // Otherwise equal
    mov     x0, #0
    b       compare_done

right_is_int:
    // Left is list, right is int
    // Compare left_list vs [right_int]
    ldr     x23, [x19, #NODE_VALUE]         // left list count

    // If left is empty, left < right
    cbz     x23, cmp_less

    // Compare left[0] with right_int
    add     x0, x19, #NODE_CHILDREN
    ldr     x0, [x0]                        // left first child
    mov     x1, x20                         // right int
    bl      compare

    // If not equal, return result
    cbnz    x0, compare_done

    // If equal and left has more elements, left > right
    ldr     x23, [x19, #NODE_VALUE]
    cmp     x23, #1
    b.gt    cmp_greater

    // Otherwise equal
    mov     x0, #0
    b       compare_done

cmp_less:
    mov     x0, #-1
    b       compare_done

cmp_greater:
    mov     x0, #1

compare_done:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// part1: Sum indices of pairs in correct order
// Output: x0 = sum
// ============================================================================
part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    LOAD_ADDR x19, packets
    LOAD_ADDR x0, packet_count
    ldr     x20, [x0]

    mov     x21, #0                         // sum
    mov     x22, #0                         // pair index (0-based in packets)
    mov     x23, #1                         // pair number (1-based for answer)

part1_loop:
    // Need 2 packets for a pair
    add     x0, x22, #1
    cmp     x0, x20
    b.ge    part1_done

    // Load pair
    ldr     x0, [x19, x22, lsl #3]          // left
    add     x1, x22, #1
    ldr     x1, [x19, x1, lsl #3]           // right

    bl      compare

    // If result is -1 (correct order), add pair index
    cmp     x0, #-1
    b.ne    part1_next

    add     x21, x21, x23

part1_next:
    add     x22, x22, #2                    // Next pair
    add     x23, x23, #1
    b       part1_loop

part1_done:
    mov     x0, x21

    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// create_divider: Create a divider packet [[n]]
// Input: x0 = inner value
// Output: x0 = pointer to [[n]] node
// ============================================================================
create_divider:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0                         // Save inner value

    // Create innermost integer node
    mov     x0, #16
    bl      alloc_node
    mov     x20, x0
    str     xzr, [x20, #NODE_TYPE]          // type = int
    str     x19, [x20, #NODE_VALUE]         // value = n

    // Create [n] list
    mov     x0, #24
    bl      alloc_node
    mov     x21, x0
    mov     x0, #1
    str     x0, [x21, #NODE_TYPE]           // type = list
    str     x0, [x21, #NODE_VALUE]          // count = 1
    str     x20, [x21, #NODE_CHILDREN]      // child = int node

    // Create [[n]] list
    mov     x0, #24
    bl      alloc_node
    mov     x22, x0
    mov     x0, #1
    str     x0, [x22, #NODE_TYPE]           // type = list
    str     x0, [x22, #NODE_VALUE]          // count = 1
    str     x21, [x22, #NODE_CHILDREN]      // child = [n] list

    mov     x0, x22

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// part2: Sort all packets with dividers, find decoder key
// Output: x0 = product of divider positions
// ============================================================================
part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    LOAD_ADDR x19, packets
    LOAD_ADDR x0, packet_count
    ldr     x20, [x0]                       // current count

    // Create divider [[2]]
    mov     x0, #2
    bl      create_divider
    mov     x21, x0
    LOAD_ADDR x1, divider1
    str     x0, [x1]

    // Add to packets
    str     x21, [x19, x20, lsl #3]
    add     x20, x20, #1

    // Create divider [[6]]
    mov     x0, #6
    bl      create_divider
    mov     x22, x0
    LOAD_ADDR x1, divider2
    str     x0, [x1]

    // Add to packets
    str     x22, [x19, x20, lsl #3]
    add     x20, x20, #1

    // Update packet count
    LOAD_ADDR x0, packet_count
    str     x20, [x0]

    // Sort packets using bubble sort
    mov     x23, x20                        // n

sort_outer:
    sub     x23, x23, #1
    cbz     x23, sort_done

    mov     x24, #0                         // swapped flag
    mov     x25, #0                         // index

sort_inner:
    cmp     x25, x23
    b.ge    sort_inner_done

    // Compare packets[i] and packets[i+1]
    ldr     x0, [x19, x25, lsl #3]
    add     x1, x25, #1
    ldr     x1, [x19, x1, lsl #3]
    bl      compare

    // If result > 0, swap
    cmp     x0, #1
    b.ne    no_swap

    // Swap
    ldr     x0, [x19, x25, lsl #3]
    add     x1, x25, #1
    ldr     x2, [x19, x1, lsl #3]
    str     x2, [x19, x25, lsl #3]
    str     x0, [x19, x1, lsl #3]
    mov     x24, #1                         // swapped = true

no_swap:
    add     x25, x25, #1
    b       sort_inner

sort_inner_done:
    cbnz    x24, sort_outer                 // Continue if swapped

sort_done:
    // Find positions of dividers
    LOAD_ADDR x0, packet_count
    ldr     x20, [x0]

    mov     x23, #0                         // pos1
    mov     x24, #0                         // pos2

    mov     x25, #0                         // index
find_dividers:
    cmp     x25, x20
    b.ge    find_dividers_done

    ldr     x0, [x19, x25, lsl #3]
    cmp     x0, x21                         // Compare with divider1
    b.ne    check_div2

    add     x23, x25, #1                    // 1-indexed position
    b       find_next

check_div2:
    cmp     x0, x22                         // Compare with divider2
    b.ne    find_next

    add     x24, x25, #1                    // 1-indexed position

find_next:
    add     x25, x25, #1
    b       find_dividers

find_dividers_done:
    // Return product
    mul     x0, x23, x24

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_str: Print null-terminated string
// Input: x0 = string address
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

2:  // Write
    mov     x0, #1
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// ============================================================================
// print_num: Print 64-bit unsigned number
// Input: x0 = number
// ============================================================================
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

    // Handle zero
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
