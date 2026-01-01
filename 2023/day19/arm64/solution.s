// =============================================================================
// Advent of Code 2023 - Day 19: Aplenty
// ARM64 Assembly Solution for macOS
//
// Problem: Workflow processing and range analysis
// Part 1: Parse workflows and parts, process parts through workflows starting
//         from "in", sum x+m+a+s for accepted parts
// Part 2: Count all combinations (x,m,a,s each 1-4000) that lead to acceptance
//         using range splitting and recursive processing
// =============================================================================

.global _main
.align 4

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------
.equ BUFFER_SIZE, 131072
.equ MAX_WORKFLOWS, 600
.equ MAX_RULES, 8
.equ MAX_PARTS, 250
.equ MAX_NAME_LEN, 4              // Workflow names up to 3 chars + null
.equ RULE_SIZE, 24                // attr(1), op(1), pad(2), value(4), dest_idx(4), pad(4), dest_name(8)
.equ WORKFLOW_SIZE, 216           // name(8) + num_rules(8) + 8*RULE_SIZE(192) + default_idx(8)

// Attribute indices: x=0, m=1, a=2, s=3
.equ ATTR_X, 0
.equ ATTR_M, 1
.equ ATTR_A, 2
.equ ATTR_S, 3

// Special workflow indices
.equ WORKFLOW_ACCEPT, -1
.equ WORKFLOW_REJECT, -2

// =============================================================================
// DATA SECTION
// =============================================================================
.data
input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

// =============================================================================
// BSS SECTION
// =============================================================================
.bss
.align 4
file_buffer:        .skip BUFFER_SIZE
file_size:          .skip 8
output_buffer:      .skip 32

// Workflow storage
// Each workflow: name(8), num_rules(8), rules[8](each 24 bytes), default_dest_idx(8)
workflows:          .skip MAX_WORKFLOWS * WORKFLOW_SIZE
num_workflows:      .skip 8
in_workflow_idx:    .skip 8         // Index of "in" workflow

// Parts storage
parts:              .skip MAX_PARTS * 32   // 4 x 64-bit values per part
num_parts:          .skip 8

// Temporary name storage
temp_name:          .skip 16

// Stack for Part 2 recursion (iterative with explicit stack)
// Each entry: workflow_idx(8), rule_idx(8), ranges(32: x_lo,x_hi,m_lo,m_hi,a_lo,a_hi,s_lo,s_hi)
.equ STACK_ENTRY_SIZE, 48
.equ MAX_STACK_DEPTH, 2048
recursion_stack:    .skip MAX_STACK_DEPTH * STACK_ENTRY_SIZE

// =============================================================================
// TEXT SECTION
// =============================================================================
.text

// -----------------------------------------------------------------------------
// _main: Program entry point
// -----------------------------------------------------------------------------
_main:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Open input file
    adrp    x0, input_path@PAGE
    add     x0, x0, input_path@PAGEOFF
    mov     x1, #0                      // O_RDONLY
    mov     x16, #5                     // open syscall
    svc     #0x80

    cmp     x0, #0
    b.lt    exit_error
    mov     x19, x0                     // Save fd

    // Read file
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #BUFFER_SIZE
    mov     x16, #3                     // read syscall
    svc     #0x80

    adrp    x1, file_size@PAGE
    add     x1, x1, file_size@PAGEOFF
    str     x0, [x1]

    // Close file
    mov     x0, x19
    mov     x16, #6                     // close syscall
    svc     #0x80

    // Parse input
    bl      parse_input

    // Resolve workflow destinations to indices
    bl      resolve_destinations

    // Part 1
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    bl      print_str

    bl      solve_part1
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // Part 2
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    bl      print_str

    bl      solve_part2
    bl      print_num

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // Exit
    mov     x0, #0
    mov     x16, #1
    svc     #0x80

exit_error:
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

// =============================================================================
// parse_input: Parse workflows and parts from input
// =============================================================================
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!
    mov     x29, sp

    // x19 = current position in buffer
    adrp    x19, file_buffer@PAGE
    add     x19, x19, file_buffer@PAGEOFF

    // x20 = end of buffer
    adrp    x20, file_size@PAGE
    add     x20, x20, file_size@PAGEOFF
    ldr     x20, [x20]
    adrp    x0, file_buffer@PAGE
    add     x0, x0, file_buffer@PAGEOFF
    add     x20, x20, x0

    // x21 = workflows base
    adrp    x21, workflows@PAGE
    add     x21, x21, workflows@PAGEOFF

    // x22 = workflow count
    mov     x22, #0

    // Parse workflows until blank line
.L_parse_workflow_loop:
    cmp     x19, x20
    b.ge    .L_parse_done

    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.eq    .L_start_parts           // Blank line = start of parts

    // Parse workflow name
    mov     x23, x19                  // Start of name
.L_find_brace:
    ldrb    w0, [x19]
    cmp     w0, #'{'
    b.eq    .L_found_brace
    add     x19, x19, #1
    b       .L_find_brace

.L_found_brace:
    // x23 = name start, x19 = position of '{'
    // Calculate workflow offset
    mov     x0, #WORKFLOW_SIZE
    mul     x24, x22, x0              // x24 = workflow offset
    add     x24, x21, x24             // x24 = workflow address

    // Copy name (up to 7 chars)
    mov     x25, x23                  // src
    mov     x26, x24                  // dest
    mov     x27, #0                   // count
.L_copy_name:
    cmp     x25, x19
    b.ge    .L_name_done
    cmp     x27, #7
    b.ge    .L_name_done
    ldrb    w0, [x25]
    strb    w0, [x26]
    add     x25, x25, #1
    add     x26, x26, #1
    add     x27, x27, #1
    b       .L_copy_name
.L_name_done:
    strb    wzr, [x26]               // Null terminate

    // Check if this is "in"
    ldrb    w0, [x24]
    cmp     w0, #'i'
    b.ne    .L_not_in
    ldrb    w0, [x24, #1]
    cmp     w0, #'n'
    b.ne    .L_not_in
    ldrb    w0, [x24, #2]
    cbnz    w0, .L_not_in
    // Found "in" workflow
    adrp    x0, in_workflow_idx@PAGE
    add     x0, x0, in_workflow_idx@PAGEOFF
    str     x22, [x0]
.L_not_in:

    add     x19, x19, #1             // Skip '{'

    // Parse rules
    mov     x25, #0                   // rule count
    add     x26, x24, #16             // rules array start (after name + num_rules)

.L_parse_rule_loop:
    // Check what kind of rule this is
    ldrb    w0, [x19]
    ldrb    w1, [x19, #1]

    // Check if it's a condition (has < or > after first char)
    cmp     w1, #'<'
    b.eq    .L_conditional_rule
    cmp     w1, #'>'
    b.eq    .L_conditional_rule

    // Default rule (just a destination name)
    b       .L_default_rule

.L_conditional_rule:
    // Parse attribute (x, m, a, s)
    ldrb    w0, [x19]
    mov     w27, #0
    cmp     w0, #'x'
    b.eq    .L_attr_found
    mov     w27, #1
    cmp     w0, #'m'
    b.eq    .L_attr_found
    mov     w27, #2
    cmp     w0, #'a'
    b.eq    .L_attr_found
    mov     w27, #3               // 's'
.L_attr_found:

    // Calculate rule offset
    mov     x0, #RULE_SIZE
    mul     x28, x25, x0
    add     x28, x26, x28         // x28 = rule address

    // Store attribute
    strb    w27, [x28, #0]

    // Store operator
    ldrb    w0, [x19, #1]
    strb    w0, [x28, #1]

    add     x19, x19, #2          // Skip attr and op

    // Parse number
    mov     x27, #0
.L_parse_cond_num:
    ldrb    w0, [x19]
    cmp     w0, #'0'
    b.lt    .L_cond_num_done
    cmp     w0, #'9'
    b.gt    .L_cond_num_done
    sub     w0, w0, #'0'
    mov     x1, #10
    mul     x27, x27, x1
    add     x27, x27, x0
    add     x19, x19, #1
    b       .L_parse_cond_num
.L_cond_num_done:
    str     w27, [x28, #4]        // Store value

    add     x19, x19, #1          // Skip ':'

    // Parse destination name (store as string for now)
    add     x0, x28, #16          // dest_name offset
    mov     x1, #0
.L_parse_cond_dest:
    ldrb    w2, [x19]
    cmp     w2, #','
    b.eq    .L_cond_dest_done
    cmp     w2, #'}'
    b.eq    .L_cond_dest_done
    strb    w2, [x0, x1]
    add     x1, x1, #1
    add     x19, x19, #1
    b       .L_parse_cond_dest
.L_cond_dest_done:
    strb    wzr, [x0, x1]         // Null terminate

    add     x25, x25, #1          // Increment rule count

    ldrb    w0, [x19]
    cmp     w0, #','
    b.ne    .L_rules_done
    add     x19, x19, #1          // Skip ','
    b       .L_parse_rule_loop

.L_default_rule:
    // Parse default destination
    mov     x0, #RULE_SIZE
    mul     x28, x25, x0
    add     x28, x26, x28

    // Mark as default rule (attr = 255)
    mov     w0, #255
    strb    w0, [x28, #0]

    // Parse destination name
    add     x0, x28, #16
    mov     x1, #0
.L_parse_default_dest:
    ldrb    w2, [x19]
    cmp     w2, #'}'
    b.eq    .L_default_dest_done
    strb    w2, [x0, x1]
    add     x1, x1, #1
    add     x19, x19, #1
    b       .L_parse_default_dest
.L_default_dest_done:
    strb    wzr, [x0, x1]

    add     x25, x25, #1

.L_rules_done:
    // Store rule count
    str     x25, [x24, #8]

    // Skip to next line
.L_skip_to_newline:
    ldrb    w0, [x19]
    cmp     w0, #'\n'
    b.eq    .L_next_workflow
    add     x19, x19, #1
    b       .L_skip_to_newline

.L_next_workflow:
    add     x19, x19, #1
    add     x22, x22, #1          // Increment workflow count
    b       .L_parse_workflow_loop

.L_start_parts:
    add     x19, x19, #1          // Skip blank line

    // Store workflow count
    adrp    x0, num_workflows@PAGE
    add     x0, x0, num_workflows@PAGEOFF
    str     x22, [x0]

    // Parse parts
    adrp    x21, parts@PAGE
    add     x21, x21, parts@PAGEOFF
    mov     x22, #0               // part count

.L_parse_part_loop:
    cmp     x19, x20
    b.ge    .L_parse_done

    ldrb    w0, [x19]
    cmp     w0, #'{'
    b.ne    .L_skip_part_char

    add     x19, x19, #1          // Skip '{'

    // Calculate part address
    lsl     x23, x22, #5          // x23 = part_index * 32
    add     x23, x21, x23         // x23 = part address

    // Parse x=NNN
    add     x19, x19, #2          // Skip "x="
    bl      .L_parse_part_num
    str     x0, [x23, #0]

    add     x19, x19, #3          // Skip ",m="
    bl      .L_parse_part_num
    str     x0, [x23, #8]

    add     x19, x19, #3          // Skip ",a="
    bl      .L_parse_part_num
    str     x0, [x23, #16]

    add     x19, x19, #3          // Skip ",s="
    bl      .L_parse_part_num
    str     x0, [x23, #24]

    add     x22, x22, #1          // Increment part count

.L_skip_part_char:
    add     x19, x19, #1
    b       .L_parse_part_loop

.L_parse_done:
    // Store part count
    adrp    x0, num_parts@PAGE
    add     x0, x0, num_parts@PAGEOFF
    str     x22, [x0]

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Helper: parse number, advance x19
.L_parse_part_num:
    mov     x0, #0
.L_ppn_loop:
    ldrb    w1, [x19]
    cmp     w1, #'0'
    b.lt    .L_ppn_done
    cmp     w1, #'9'
    b.gt    .L_ppn_done
    sub     w1, w1, #'0'
    mov     x2, #10
    mul     x0, x0, x2
    add     x0, x0, x1
    add     x19, x19, #1
    b       .L_ppn_loop
.L_ppn_done:
    ret

// =============================================================================
// resolve_destinations: Convert destination names to workflow indices
// =============================================================================
resolve_destinations:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    mov     x29, sp

    adrp    x19, workflows@PAGE
    add     x19, x19, workflows@PAGEOFF

    adrp    x20, num_workflows@PAGE
    add     x20, x20, num_workflows@PAGEOFF
    ldr     x20, [x20]

    mov     x21, #0               // workflow index

.L_resolve_wf_loop:
    cmp     x21, x20
    b.ge    .L_resolve_done

    // Calculate workflow address
    mov     x0, #WORKFLOW_SIZE
    mul     x22, x21, x0
    add     x22, x19, x22         // x22 = workflow address

    // Get rule count
    ldr     x23, [x22, #8]

    // Process each rule
    mov     x24, #0               // rule index
    add     x25, x22, #16         // rules base

.L_resolve_rule_loop:
    cmp     x24, x23
    b.ge    .L_resolve_next_wf

    mov     x0, #RULE_SIZE
    mul     x26, x24, x0
    add     x26, x25, x26         // x26 = rule address

    // Get destination name
    add     x0, x26, #16          // dest_name
    bl      find_workflow_idx

    // Store destination index
    str     w0, [x26, #8]

    add     x24, x24, #1
    b       .L_resolve_rule_loop

.L_resolve_next_wf:
    add     x21, x21, #1
    b       .L_resolve_wf_loop

.L_resolve_done:
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// find_workflow_idx: Find workflow index by name
// Input: x0 = name pointer
// Output: x0 = index (-1 for A, -2 for R)
// =============================================================================
find_workflow_idx:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    mov     x29, sp

    mov     x19, x0               // Save name pointer

    // Check for "A"
    ldrb    w0, [x19]
    cmp     w0, #'A'
    b.ne    .L_check_R
    ldrb    w0, [x19, #1]
    cbnz    w0, .L_check_R
    mov     x0, #WORKFLOW_ACCEPT
    b       .L_find_done

.L_check_R:
    ldrb    w0, [x19]
    cmp     w0, #'R'
    b.ne    .L_search_workflows
    ldrb    w0, [x19, #1]
    cbnz    w0, .L_search_workflows
    mov     x0, #WORKFLOW_REJECT
    b       .L_find_done

.L_search_workflows:
    adrp    x20, workflows@PAGE
    add     x20, x20, workflows@PAGEOFF

    adrp    x21, num_workflows@PAGE
    add     x21, x21, num_workflows@PAGEOFF
    ldr     x21, [x21]

    mov     x22, #0

.L_search_loop:
    cmp     x22, x21
    b.ge    .L_not_found

    mov     x0, #WORKFLOW_SIZE
    mul     x23, x22, x0
    add     x23, x20, x23         // workflow address

    // Compare names
    mov     x0, x19
    mov     x1, x23
    bl      str_equal
    cbnz    x0, .L_found_wf

    add     x22, x22, #1
    b       .L_search_loop

.L_found_wf:
    mov     x0, x22
    b       .L_find_done

.L_not_found:
    mov     x0, #-3               // Should not happen

.L_find_done:
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// str_equal: Compare two null-terminated strings
// Input: x0, x1 = string pointers
// Output: x0 = 1 if equal, 0 otherwise
// =============================================================================
str_equal:
    stp     x19, x20, [sp, #-16]!
    mov     x19, x0
    mov     x20, x1

.L_cmp_loop:
    ldrb    w0, [x19]
    ldrb    w1, [x20]
    cmp     w0, w1
    b.ne    .L_not_equal
    cbz     w0, .L_equal
    add     x19, x19, #1
    add     x20, x20, #1
    b       .L_cmp_loop

.L_equal:
    mov     x0, #1
    ldp     x19, x20, [sp], #16
    ret

.L_not_equal:
    mov     x0, #0
    ldp     x19, x20, [sp], #16
    ret

// =============================================================================
// solve_part1: Process all parts and sum accepted ratings
// Output: x0 = total sum
// =============================================================================
solve_part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    mov     x29, sp

    adrp    x19, parts@PAGE
    add     x19, x19, parts@PAGEOFF

    adrp    x20, num_parts@PAGE
    add     x20, x20, num_parts@PAGEOFF
    ldr     x20, [x20]

    mov     x21, #0               // part index
    mov     x22, #0               // total sum

.L_p1_part_loop:
    cmp     x21, x20
    b.ge    .L_p1_done

    // Get part address
    lsl     x0, x21, #5
    add     x23, x19, x0          // x23 = part address

    // Process part through workflows
    mov     x0, x23
    bl      process_part

    cmp     x0, #0
    b.eq    .L_p1_next_part

    // Accepted - add ratings
    ldr     x0, [x23, #0]         // x
    ldr     x1, [x23, #8]         // m
    add     x0, x0, x1
    ldr     x1, [x23, #16]        // a
    add     x0, x0, x1
    ldr     x1, [x23, #24]        // s
    add     x0, x0, x1
    add     x22, x22, x0

.L_p1_next_part:
    add     x21, x21, #1
    b       .L_p1_part_loop

.L_p1_done:
    mov     x0, x22

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// process_part: Process a part through workflows
// Input: x0 = part address (4 x 64-bit values: x, m, a, s)
// Output: x0 = 1 if accepted, 0 if rejected
// =============================================================================
process_part:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!
    mov     x29, sp

    mov     x19, x0               // part address

    // Load part values
    ldr     x20, [x19, #0]        // x
    ldr     x21, [x19, #8]        // m
    ldr     x22, [x19, #16]       // a
    ldr     x23, [x19, #24]       // s

    // Get starting workflow
    adrp    x24, in_workflow_idx@PAGE
    add     x24, x24, in_workflow_idx@PAGEOFF
    ldr     x24, [x24]            // current workflow index

    adrp    x25, workflows@PAGE
    add     x25, x25, workflows@PAGEOFF

.L_pp_workflow_loop:
    // Check for accept/reject
    cmn     x24, #1               // cmp x24, #-1
    b.eq    .L_pp_accept
    cmn     x24, #2               // cmp x24, #-2
    b.eq    .L_pp_reject

    // Get workflow address
    mov     x0, #WORKFLOW_SIZE
    mul     x26, x24, x0
    add     x26, x25, x26         // workflow address

    // Get rule count
    ldr     x27, [x26, #8]

    // Process rules
    mov     x28, #0               // rule index
    add     x26, x26, #16         // rules base

.L_pp_rule_loop:
    cmp     x28, x27
    b.ge    .L_pp_workflow_loop   // Should not reach here

    // Get rule address
    mov     x0, #RULE_SIZE
    mul     x0, x28, x0
    add     x0, x26, x0           // rule address

    // Get attribute
    ldrb    w1, [x0, #0]
    cmp     w1, #255
    b.eq    .L_pp_default_rule

    // Get part value for this attribute
    cmp     w1, #0
    b.eq    .L_pp_use_x
    cmp     w1, #1
    b.eq    .L_pp_use_m
    cmp     w1, #2
    b.eq    .L_pp_use_a
    mov     x2, x23               // s
    b       .L_pp_have_val
.L_pp_use_x:
    mov     x2, x20
    b       .L_pp_have_val
.L_pp_use_m:
    mov     x2, x21
    b       .L_pp_have_val
.L_pp_use_a:
    mov     x2, x22
.L_pp_have_val:

    // Get operator and value
    ldrb    w3, [x0, #1]          // operator
    ldr     w4, [x0, #4]          // value (as 32-bit)

    // Compare
    cmp     w3, #'<'
    b.eq    .L_pp_less_than
    // Greater than
    cmp     x2, x4
    b.le    .L_pp_next_rule
    b       .L_pp_take_dest

.L_pp_less_than:
    cmp     x2, x4
    b.ge    .L_pp_next_rule

.L_pp_take_dest:
    // Take this destination
    ldr     w24, [x0, #8]         // dest_idx (as 32-bit signed)
    sxtw    x24, w24              // Sign extend to 64-bit
    b       .L_pp_workflow_loop

.L_pp_default_rule:
    ldr     w24, [x0, #8]
    sxtw    x24, w24
    b       .L_pp_workflow_loop

.L_pp_next_rule:
    add     x28, x28, #1
    b       .L_pp_rule_loop

.L_pp_accept:
    mov     x0, #1
    b       .L_pp_done

.L_pp_reject:
    mov     x0, #0

.L_pp_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// solve_part2: Count all accepted combinations using range splitting
// Output: x0 = total count
// =============================================================================
solve_part2:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!
    mov     x29, sp

    // Initialize stack with starting state
    adrp    x19, recursion_stack@PAGE
    add     x19, x19, recursion_stack@PAGEOFF

    // Get in_workflow_idx
    adrp    x0, in_workflow_idx@PAGE
    add     x0, x0, in_workflow_idx@PAGEOFF
    ldr     x20, [x0]             // starting workflow

    // Push initial state: workflow=in, rule=0, ranges all 1-4000
    str     x20, [x19, #0]        // workflow_idx
    str     xzr, [x19, #8]        // rule_idx = 0
    mov     x0, #1
    str     x0, [x19, #16]        // x_lo = 1
    mov     x0, #4000
    str     x0, [x19, #24]        // x_hi = 4000
    mov     x0, #1
    str     x0, [x19, #32]        // m_lo = 1
    mov     x0, #4000
    str     x0, [x19, #40]        // m_hi = 4000
    mov     x0, #1
    str     x0, [x19, #48]        // a_lo = 1
    mov     x0, #4000
    str     x0, [x19, #56]        // a_hi = 4000
    mov     x0, #1
    str     x0, [x19, #64]        // s_lo = 1
    mov     x0, #4000
    str     x0, [x19, #72]        // s_hi = 4000

    mov     x21, #1               // stack pointer (1 entry)
    mov     x22, #0               // total count

    adrp    x23, workflows@PAGE
    add     x23, x23, workflows@PAGEOFF

.L_p2_stack_loop:
    cbz     x21, .L_p2_done

    // Pop state
    sub     x21, x21, #1
    mov     x0, #80               // stack entry size = 80 bytes (with all ranges)
    mul     x0, x21, x0
    add     x24, x19, x0          // x24 = stack entry address

    // Load state
    ldr     x25, [x24, #0]        // workflow_idx
    ldr     x26, [x24, #8]        // rule_idx

    // Check for accept
    cmn     x25, #1
    b.ne    .L_p2_check_reject

    // Accepted - calculate combinations
    ldr     x0, [x24, #16]        // x_lo
    ldr     x1, [x24, #24]        // x_hi
    sub     x0, x1, x0
    add     x0, x0, #1

    ldr     x1, [x24, #32]        // m_lo
    ldr     x2, [x24, #40]        // m_hi
    sub     x1, x2, x1
    add     x1, x1, #1
    mul     x0, x0, x1

    ldr     x1, [x24, #48]        // a_lo
    ldr     x2, [x24, #56]        // a_hi
    sub     x1, x2, x1
    add     x1, x1, #1
    mul     x0, x0, x1

    ldr     x1, [x24, #64]        // s_lo
    ldr     x2, [x24, #72]        // s_hi
    sub     x1, x2, x1
    add     x1, x1, #1
    mul     x0, x0, x1

    add     x22, x22, x0
    b       .L_p2_stack_loop

.L_p2_check_reject:
    cmn     x25, #2
    b.eq    .L_p2_stack_loop      // Rejected - skip

    // Get workflow
    mov     x0, #WORKFLOW_SIZE
    mul     x0, x25, x0
    add     x27, x23, x0          // workflow address

    // Get rule count
    ldr     x28, [x27, #8]

    // Get rule
    cmp     x26, x28
    b.ge    .L_p2_stack_loop      // Should not happen

    add     x27, x27, #16         // rules base
    mov     x0, #RULE_SIZE
    mul     x0, x26, x0
    add     x27, x27, x0          // rule address

    // Get attribute
    ldrb    w0, [x27, #0]
    cmp     w0, #255
    b.eq    .L_p2_default_rule

    // Conditional rule
    // Get operator and value
    ldrb    w1, [x27, #1]         // operator
    ldr     w2, [x27, #4]         // value

    // Get destination
    ldr     w3, [x27, #8]
    sxtw    x3, w3                // dest_idx

    // Calculate offset for this attribute's range
    // attr 0 (x): offset 16/24, attr 1 (m): offset 32/40,
    // attr 2 (a): offset 48/56, attr 3 (s): offset 64/72
    and     x0, x0, #3
    lsl     x4, x0, #4            // x4 = attr * 16
    add     x4, x4, #16           // offset to lo

    // Load current range for this attribute
    ldr     x5, [x24, x4]         // lo
    add     x6, x4, #8
    ldr     x6, [x24, x6]         // hi

    // Split based on operator
    cmp     w1, #'<'
    b.eq    .L_p2_split_less

    // Operator is '>'
    // Match: [value+1, hi], Continue: [lo, value]
    add     x7, x2, #1            // match_lo = value + 1
    mov     x8, x6                // match_hi = hi
    mov     x9, x5                // cont_lo = lo
    mov     x10, x2               // cont_hi = value
    b       .L_p2_do_split

.L_p2_split_less:
    // Operator is '<'
    // Match: [lo, value-1], Continue: [value, hi]
    mov     x7, x5                // match_lo = lo
    sub     x8, x2, #1            // match_hi = value - 1
    mov     x9, x2                // cont_lo = value
    mov     x10, x6               // cont_hi = hi

.L_p2_do_split:
    // Push continue state if valid (next rule, same workflow)
    cmp     x9, x10
    b.gt    .L_p2_no_continue

    // Check we have stack space
    cmp     x21, #MAX_STACK_DEPTH
    b.ge    .L_p2_no_continue

    // Push continue state
    mov     x0, #80
    mul     x0, x21, x0
    add     x11, x19, x0          // new entry address

    str     x25, [x11, #0]        // same workflow
    add     x0, x26, #1
    str     x0, [x11, #8]         // next rule

    // Copy all ranges from current state
    ldr     x0, [x24, #16]
    str     x0, [x11, #16]
    ldr     x0, [x24, #24]
    str     x0, [x11, #24]
    ldr     x0, [x24, #32]
    str     x0, [x11, #32]
    ldr     x0, [x24, #40]
    str     x0, [x11, #40]
    ldr     x0, [x24, #48]
    str     x0, [x11, #48]
    ldr     x0, [x24, #56]
    str     x0, [x11, #56]
    ldr     x0, [x24, #64]
    str     x0, [x11, #64]
    ldr     x0, [x24, #72]
    str     x0, [x11, #72]

    // Update the specific attribute range for continue
    str     x9, [x11, x4]         // cont_lo
    add     x0, x4, #8
    str     x10, [x11, x0]        // cont_hi

    add     x21, x21, #1

.L_p2_no_continue:
    // Push match state if valid (rule 0, destination workflow)
    cmp     x7, x8
    b.gt    .L_p2_stack_loop

    cmp     x21, #MAX_STACK_DEPTH
    b.ge    .L_p2_stack_loop

    mov     x0, #80
    mul     x0, x21, x0
    add     x11, x19, x0

    str     x3, [x11, #0]         // destination workflow
    str     xzr, [x11, #8]        // rule 0

    // Copy all ranges
    ldr     x0, [x24, #16]
    str     x0, [x11, #16]
    ldr     x0, [x24, #24]
    str     x0, [x11, #24]
    ldr     x0, [x24, #32]
    str     x0, [x11, #32]
    ldr     x0, [x24, #40]
    str     x0, [x11, #40]
    ldr     x0, [x24, #48]
    str     x0, [x11, #48]
    ldr     x0, [x24, #56]
    str     x0, [x11, #56]
    ldr     x0, [x24, #64]
    str     x0, [x11, #64]
    ldr     x0, [x24, #72]
    str     x0, [x11, #72]

    // Update the specific attribute range for match
    str     x7, [x11, x4]         // match_lo
    add     x0, x4, #8
    str     x8, [x11, x0]         // match_hi

    add     x21, x21, #1
    b       .L_p2_stack_loop

.L_p2_default_rule:
    // Default rule - just push destination with current ranges
    ldr     w3, [x27, #8]
    sxtw    x3, w3

    cmp     x21, #MAX_STACK_DEPTH
    b.ge    .L_p2_stack_loop

    mov     x0, #80
    mul     x0, x21, x0
    add     x11, x19, x0

    str     x3, [x11, #0]         // destination workflow
    str     xzr, [x11, #8]        // rule 0

    // Copy all ranges
    ldr     x0, [x24, #16]
    str     x0, [x11, #16]
    ldr     x0, [x24, #24]
    str     x0, [x11, #24]
    ldr     x0, [x24, #32]
    str     x0, [x11, #32]
    ldr     x0, [x24, #40]
    str     x0, [x11, #40]
    ldr     x0, [x24, #48]
    str     x0, [x11, #48]
    ldr     x0, [x24, #56]
    str     x0, [x11, #56]
    ldr     x0, [x24, #64]
    str     x0, [x11, #64]
    ldr     x0, [x24, #72]
    str     x0, [x11, #72]

    add     x21, x21, #1
    b       .L_p2_stack_loop

.L_p2_done:
    mov     x0, x22

    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// =============================================================================
// UTILITY FUNCTIONS
// =============================================================================

// -----------------------------------------------------------------------------
// print_str: Write null-terminated string to stdout
// Input: x0 = string address
// -----------------------------------------------------------------------------
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Calculate length
    mov     x20, #0
.L_strlen:
    ldrb    w1, [x19, x20]
    cbz     w1, .L_strlen_done
    add     x20, x20, #1
    b       .L_strlen
.L_strlen_done:

    mov     x0, #1                // stdout
    mov     x1, x19
    mov     x2, x20
    mov     x16, #4               // write syscall
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// -----------------------------------------------------------------------------
// print_num: Write decimal number to stdout
// Input: x0 = number to print
// -----------------------------------------------------------------------------
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0
    adrp    x20, output_buffer@PAGE
    add     x20, x20, output_buffer@PAGEOFF
    add     x20, x20, #31
    mov     x21, #0

    // Handle zero
    cbnz    x19, .L_num_loop
    mov     w22, #'0'
    strb    w22, [x20, #-1]!
    mov     x21, #1
    b       .L_num_print

.L_num_loop:
    cbz     x19, .L_num_print

    mov     x1, #10
    udiv    x2, x19, x1
    msub    x3, x2, x1, x19

    add     w3, w3, #'0'
    strb    w3, [x20, #-1]!
    add     x21, x21, #1

    mov     x19, x2
    b       .L_num_loop

.L_num_print:
    mov     x0, #1                // stdout
    mov     x1, x20
    mov     x2, x21
    mov     x16, #4               // write syscall
    svc     #0x80

    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
