.global _start
.align 4

.data
.align 3
filename:       .asciz "../input.txt"
.align 3
msg_part1:      .asciz "Part 1: "
.align 3
msg_part2:      .asciz "Part 2: "
.align 3
newline:        .asciz "\n"

// Keep data compact and aligned
.align 16
node_names:     .space 2048 * 8
.align 16
node_count:     .quad 0
.align 16
adj_offset:     .space 2048 * 8
.align 16
adj_count:      .space 2048 * 8
.align 16
adj_data:       .space 16384 * 8
.align 16
path_counts:    .space 2048 * 8
.align 16
visited_dac:    .space 2048 * 8
.align 16
visited_fft:    .space 2048 * 8
.align 16
visited_both:   .space 2048 * 8
.align 16
input_buffer:   .space 65536
.align 16
you_idx:        .quad 0
.align 16
out_idx:        .quad 0
.align 16
svr_idx:        .quad 0
.align 16
dac_idx:        .quad 0
.align 16
fft_idx:        .quad 0

.text

_start:
    // Open file
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0                  // O_RDONLY
    mov x2, #0                  // mode
    mov x16, #0x2000000
    add x16, x16, #5            // SYS_open
    svc #0

    cmp x0, #0
    b.lt exit_error
    mov x19, x0                 // Save fd

    // Read file
    mov x0, x19
    adrp x1, input_buffer@PAGE
    add x1, x1, input_buffer@PAGEOFF
    mov x2, #65536
    mov x16, #0x2000000
    add x16, x16, #3            // SYS_read
    svc #0

    mov x20, x0                 // Save bytes read

    // Close file
    mov x0, x19
    mov x16, #0x2000000
    add x16, x16, #6            // SYS_close
    svc #0

    // Parse input
    adrp x0, input_buffer@PAGE
    add x0, x0, input_buffer@PAGEOFF
    mov x1, x20
    bl parse_input

    // Part 1
    bl solve_part1
    mov x21, x0

    // Part 2
    bl solve_part2
    mov x22, x0

    // Print Part 1
    adrp x0, msg_part1@PAGE
    add x0, x0, msg_part1@PAGEOFF
    bl print_str
    mov x0, x21
    bl print_num
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Print Part 2
    adrp x0, msg_part2@PAGE
    add x0, x0, msg_part2@PAGEOFF
    bl print_str
    mov x0, x22
    bl print_num
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Exit
    mov x16, #0x2000000
    add x16, x16, #1            // SYS_exit
    mov x0, #0
    svc #0

exit_error:
    mov x16, #0x2000000
    add x16, x16, #1
    mov x0, #1
    svc #0

// Parse input
parse_input:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0                 // buffer
    mov x20, x1                 // length
    mov x21, #0                 // position
    adrp x22, adj_data@PAGE
    add x22, x22, adj_data@PAGEOFF
    mov x23, #0                 // adj offset

parse_loop:
    cmp x21, x20
    b.ge parse_done

    // Parse node name (3 chars)
    add x0, x19, x21
    ldrb w1, [x0]
    ldrb w2, [x0, #1]
    ldrb w3, [x0, #2]
    orr x24, x1, x2, lsl #8
    orr x24, x24, x3, lsl #16

    // Find or create
    mov x0, x24
    bl find_or_create_node
    mov x24, x0                 // node index

    // Skip to colon
    add x21, x21, #3
skip_colon:
    add x0, x19, x21
    ldrb w1, [x0]
    add x21, x21, #1
    cmp w1, #58                 // ':'
    b.ne skip_colon
    add x21, x21, #1            // Skip space

    // Store adj offset
    adrp x0, adj_offset@PAGE
    add x0, x0, adj_offset@PAGEOFF
    str x23, [x0, x24, lsl #3]

    // Parse neighbors
    mov x25, #0                 // count
parse_neighbors:
    cmp x21, x20
    b.ge neighbors_done
    add x0, x19, x21
    ldrb w1, [x0]
    cmp w1, #10                 // newline
    b.eq neighbors_done
    cmp w1, #32                 // space
    b.ne parse_neighbor
    add x21, x21, #1
    b parse_neighbors

parse_neighbor:
    // Read 3 chars
    add x0, x19, x21
    ldrb w1, [x0]
    ldrb w2, [x0, #1]
    ldrb w3, [x0, #2]
    orr x26, x1, x2, lsl #8
    orr x26, x26, x3, lsl #16

    // Find or create
    mov x0, x26
    bl find_or_create_node

    // Store neighbor
    str x0, [x22, x23]
    add x23, x23, #8
    add x25, x25, #1
    add x21, x21, #3
    b parse_neighbors

neighbors_done:
    // Store count
    adrp x0, adj_count@PAGE
    add x0, x0, adj_count@PAGEOFF
    str x25, [x0, x24, lsl #3]

    // Skip newline
    add x21, x21, #1
    b parse_loop

parse_done:
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Find or create node
// x0 = packed name
// Returns: x0 = node index
find_or_create_node:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0                 // name
    adrp x20, node_names@PAGE
    add x20, x20, node_names@PAGEOFF
    adrp x21, node_count@PAGE
    add x21, x21, node_count@PAGEOFF
    ldr x22, [x21]              // count

    // Search
    mov x1, #0
find_loop:
    cmp x1, x22
    b.ge create_new
    ldr x2, [x20, x1, lsl #3]
    cmp x2, x19
    b.eq found
    add x1, x1, #1
    b find_loop

create_new:
    // Store name
    str x19, [x20, x22, lsl #3]

    // Check special nodes
    // "you" = 0x756f79
    mov x0, #0x6f79
    movk x0, #0x75, lsl #16
    cmp x19, x0
    b.ne check_out
    adrp x0, you_idx@PAGE
    add x0, x0, you_idx@PAGEOFF
    str x22, [x0]

check_out:
    // "out" = 0x74756f
    mov x0, #0x756f
    movk x0, #0x74, lsl #16
    cmp x19, x0
    b.ne check_svr
    adrp x0, out_idx@PAGE
    add x0, x0, out_idx@PAGEOFF
    str x22, [x0]

check_svr:
    // "svr" = 0x727673
    mov x0, #0x7673
    movk x0, #0x72, lsl #16
    cmp x19, x0
    b.ne check_dac
    adrp x0, svr_idx@PAGE
    add x0, x0, svr_idx@PAGEOFF
    str x22, [x0]

check_dac:
    // "dac" = 0x636164
    mov x0, #0x6164
    movk x0, #0x63, lsl #16
    cmp x19, x0
    b.ne check_fft
    adrp x0, dac_idx@PAGE
    add x0, x0, dac_idx@PAGEOFF
    str x22, [x0]

check_fft:
    // "fft" = 0x746666
    mov x0, #0x6666
    movk x0, #0x74, lsl #16
    cmp x19, x0
    b.ne store_count
    adrp x0, fft_idx@PAGE
    add x0, x0, fft_idx@PAGEOFF
    str x22, [x0]

store_count:
    add x22, x22, #1
    str x22, [x21]
    sub x0, x22, #1
    b find_done

found:
    mov x0, x1

find_done:
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 1 - Count paths using DFS with memoization
solve_part1:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    // Initialize path_counts to -1 (unvisited)
    adrp x19, path_counts@PAGE
    add x19, x19, path_counts@PAGEOFF
    adrp x20, node_count@PAGE
    add x20, x20, node_count@PAGEOFF
    ldr x20, [x20]
    mov x0, #0
init1:
    cmp x0, x20
    b.ge init1_done
    mov x1, #-1
    str x1, [x19, x0, lsl #3]
    add x0, x0, #1
    b init1

init1_done:
    // Call dfs('you')
    adrp x0, you_idx@PAGE
    add x0, x0, you_idx@PAGEOFF
    ldr x0, [x0]
    bl dfs_count_paths

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// DFS path counting
// x0 = node index
// Returns: x0 = path count
dfs_count_paths:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0                 // Save node

    // Check if node is 'out'
    adrp x0, out_idx@PAGE
    add x0, x0, out_idx@PAGEOFF
    ldr x0, [x0]
    cmp x19, x0
    b.ne check_memo
    mov x0, #1
    b dfs_done

check_memo:
    // Check if already computed
    adrp x20, path_counts@PAGE
    add x20, x20, path_counts@PAGEOFF
    ldr x0, [x20, x19, lsl #3]
    cmn x0, #1                  // Compare with -1
    b.ne dfs_done               // Already computed

    // Get neighbors
    adrp x0, adj_offset@PAGE
    add x0, x0, adj_offset@PAGEOFF
    ldr x21, [x0, x19, lsl #3]  // offset
    adrp x0, adj_count@PAGE
    add x0, x0, adj_count@PAGEOFF
    ldr x22, [x0, x19, lsl #3]  // count
    adrp x23, adj_data@PAGE
    add x23, x23, adj_data@PAGEOFF
    add x23, x23, x21           // neighbor array

    // Sum paths through all neighbors
    mov x24, #0                 // sum
    mov x4, #0                  // index
dfs_loop:
    cmp x4, x22
    b.ge dfs_loop_done

    // Save state
    stp x4, x24, [sp, #-16]!

    // Recursive call
    ldr x0, [x23, x4, lsl #3]
    bl dfs_count_paths

    // Restore and accumulate
    ldp x4, x24, [sp], #16
    add x24, x24, x0
    add x4, x4, #1
    b dfs_loop

dfs_loop_done:
    // Store result
    str x24, [x20, x19, lsl #3]
    mov x0, x24

dfs_done:
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2
// Count paths from svr -> out that visit both dac and fft
// paths = paths(svr->dac) * paths(dac->fft) * paths(fft->out)
//       + paths(svr->fft) * paths(fft->dac) * paths(dac->out)
solve_part2:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    // Initialize path_counts to -1
    adrp x19, path_counts@PAGE
    add x19, x19, path_counts@PAGEOFF
    adrp x20, node_count@PAGE
    add x20, x20, node_count@PAGEOFF
    ldr x20, [x20]
    mov x0, #0
init2:
    cmp x0, x20
    b.ge init2_done
    mov x1, #-1
    str x1, [x19, x0, lsl #3]
    add x0, x0, #1
    b init2

init2_done:
    // Compute dac_before_fft:
    // paths(svr -> dac)
    adrp x0, dac_idx@PAGE
    add x0, x0, dac_idx@PAGEOFF
    ldr x21, [x0]               // target = dac
    adrp x0, svr_idx@PAGE
    add x0, x0, svr_idx@PAGEOFF
    ldr x0, [x0]                // start = svr
    bl count_paths_to_target
    mov x22, x0                 // save paths(svr -> dac)

    // Re-init for next count
    bl reinit_paths

    // paths(dac -> fft)
    adrp x0, fft_idx@PAGE
    add x0, x0, fft_idx@PAGEOFF
    ldr x21, [x0]               // target = fft
    adrp x0, dac_idx@PAGE
    add x0, x0, dac_idx@PAGEOFF
    ldr x0, [x0]                // start = dac
    bl count_paths_to_target
    mul x22, x22, x0            // *= paths(dac -> fft)

    // Re-init
    bl reinit_paths

    // paths(fft -> out)
    adrp x0, out_idx@PAGE
    add x0, x0, out_idx@PAGEOFF
    ldr x21, [x0]               // target = out
    adrp x0, fft_idx@PAGE
    add x0, x0, fft_idx@PAGEOFF
    ldr x0, [x0]                // start = fft
    bl count_paths_to_target
    mul x22, x22, x0            // dac_before_fft complete

    // Now compute fft_before_dac:
    bl reinit_paths

    // paths(svr -> fft)
    adrp x0, fft_idx@PAGE
    add x0, x0, fft_idx@PAGEOFF
    ldr x21, [x0]
    adrp x0, svr_idx@PAGE
    add x0, x0, svr_idx@PAGEOFF
    ldr x0, [x0]
    bl count_paths_to_target
    mov x23, x0                 // save paths(svr -> fft)

    bl reinit_paths

    // paths(fft -> dac)
    adrp x0, dac_idx@PAGE
    add x0, x0, dac_idx@PAGEOFF
    ldr x21, [x0]
    adrp x0, fft_idx@PAGE
    add x0, x0, fft_idx@PAGEOFF
    ldr x0, [x0]
    bl count_paths_to_target
    mul x23, x23, x0

    bl reinit_paths

    // paths(dac -> out)
    adrp x0, out_idx@PAGE
    add x0, x0, out_idx@PAGEOFF
    ldr x21, [x0]
    adrp x0, dac_idx@PAGE
    add x0, x0, dac_idx@PAGEOFF
    ldr x0, [x0]
    bl count_paths_to_target
    mul x23, x23, x0            // fft_before_dac complete

    // Return sum
    add x0, x22, x23

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Reinitialize path_counts to -1
reinit_paths:
    stp x29, x30, [sp, #-16]!
    stp x0, x1, [sp, #-16]!
    stp x2, x3, [sp, #-16]!

    adrp x0, path_counts@PAGE
    add x0, x0, path_counts@PAGEOFF
    adrp x1, node_count@PAGE
    add x1, x1, node_count@PAGEOFF
    ldr x1, [x1]
    mov x2, #0
reinit_loop:
    cmp x2, x1
    b.ge reinit_done
    mov x3, #-1
    str x3, [x0, x2, lsl #3]
    add x2, x2, #1
    b reinit_loop
reinit_done:
    ldp x2, x3, [sp], #16
    ldp x0, x1, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Count paths from x0 (start) to x21 (target)
// x0 = start node, x21 = target node
// Returns: x0 = count
count_paths_to_target:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0                 // start
    mov x20, x21                // target

    // Call dfs
    mov x0, x19
    mov x21, x20
    bl dfs_to_target

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// DFS to count paths to target
// x0 = current node, x21 = target
dfs_to_target:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x22, x23, [sp, #-16]!
    stp x24, x25, [sp, #-16]!

    mov x19, x0                 // current node
    mov x20, x21                // target (preserved)

    // Check if at target
    cmp x19, x20
    b.ne check_memo2
    mov x0, #1
    b dfs2_done

check_memo2:
    // Check if memoized
    adrp x22, path_counts@PAGE
    add x22, x22, path_counts@PAGEOFF
    ldr x0, [x22, x19, lsl #3]
    cmn x0, #1
    b.ne dfs2_done

    // Get neighbors
    adrp x0, adj_offset@PAGE
    add x0, x0, adj_offset@PAGEOFF
    ldr x23, [x0, x19, lsl #3]
    adrp x0, adj_count@PAGE
    add x0, x0, adj_count@PAGEOFF
    ldr x24, [x0, x19, lsl #3]
    adrp x25, adj_data@PAGE
    add x25, x25, adj_data@PAGEOFF
    add x25, x25, x23

    // Sum paths
    mov x3, #0                  // sum
    mov x4, #0                  // index
dfs2_loop:
    cmp x4, x24
    b.ge dfs2_loop_done

    // Save state
    stp x3, x4, [sp, #-16]!

    // Recursive call
    ldr x0, [x25, x4, lsl #3]
    mov x21, x20                // restore target
    bl dfs_to_target

    // Restore and accumulate
    ldp x3, x4, [sp], #16
    add x3, x3, x0
    add x4, x4, #1
    b dfs2_loop

dfs2_loop_done:
    // Store and return
    str x3, [x22, x19, lsl #3]
    mov x0, x3

dfs2_done:
    ldp x24, x25, [sp], #16
    ldp x22, x23, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print string
print_str:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    mov x20, #0
strlen:
    ldrb w1, [x19, x20]
    cbz w1, strlen_done
    add x20, x20, #1
    b strlen
strlen_done:
    mov x16, #0x2000000
    add x16, x16, #4            // SYS_write
    mov x0, #1
    mov x1, x19
    mov x2, x20
    svc #0

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print number
print_num:
    stp x29, x30, [sp, #-16]!
    sub sp, sp, #32

    mov x1, sp
    add x1, x1, #31
    mov x2, #0
    mov x3, x0

    cbz x3, print_zero

convert:
    cbz x3, convert_done
    mov x4, #10
    udiv x5, x3, x4
    msub x6, x5, x4, x3
    add x6, x6, #48
    sub x1, x1, #1
    strb w6, [x1]
    add x2, x2, #1
    mov x3, x5
    b convert

print_zero:
    mov w3, #48
    sub x1, x1, #1
    strb w3, [x1]
    add x2, x2, #1

convert_done:
    mov x16, #0x2000000
    add x16, x16, #4
    mov x0, #1
    svc #0

    add sp, sp, #32
    ldp x29, x30, [sp], #16
    ret
