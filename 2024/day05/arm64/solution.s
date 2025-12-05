// ARM64 Assembly solution for AoC 2024 Day 5
// macOS syscalls

.global _main
.align 2

.equ STDOUT, 1
.equ MAX_RULES, 2000
.equ MAX_UPDATES, 250
.equ MAX_PAGES, 30

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 65536
// Rules: pairs of (before, after)
rules: .skip MAX_RULES * 16  // Each rule: 8 bytes before, 8 bytes after
num_rules: .skip 8
// Updates: each update has count followed by page numbers
updates: .skip MAX_UPDATES * (MAX_PAGES + 1) * 8
num_updates: .skip 8
// Temporary arrays for sorting
temp_update: .skip MAX_PAGES * 8
temp_count: .skip 8
output_buffer: .skip 32

.text
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Open file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0005
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0  // O_RDONLY
    mov x2, #0
    svc #0x80
    cmp x0, #0
    b.lt exit_error
    mov x19, x0  // Save fd

    // Read file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0003
    mov x0, x19
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #65536
    svc #0x80
    mov x20, x0  // Save bytes read

    // Close file
    movz x16, #0x2000, lsl #16
    movk x16, #0x0006
    mov x0, x19
    svc #0x80

    // Parse input
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl parse_input

    // Part 1
    bl part1
    mov x21, x0  // Save result

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str
    mov x0, x21
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2
    bl part2
    mov x22, x0  // Save result

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str
    mov x0, x22
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    mov x0, #0
    ldp x29, x30, [sp], #16
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    svc #0x80

exit_error:
    mov x0, #1
    ldp x29, x30, [sp], #16
    movz x16, #0x2000, lsl #16
    movk x16, #0x0001
    svc #0x80

// Parse input: rules and updates sections
// x0 = buffer, x1 = length
parse_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0  // Buffer pointer
    mov x20, x1  // Length
    add x20, x19, x20  // Buffer end

    adrp x21, rules@PAGE
    add x21, x21, rules@PAGEOFF
    mov x22, #0  // Rule count

    // Parse rules section (until blank line)
parse_rules_loop:
    cmp x19, x20
    b.ge parse_rules_done
    ldrb w0, [x19]
    cmp w0, #'\n'
    b.eq parse_rules_done  // Blank line = end of rules

    // Parse "X|Y" format
    mov x0, x19
    bl parse_number
    mov x23, x1  // X (before)
    mov x19, x0

    // Skip '|'
    add x19, x19, #1

    // Parse Y
    mov x0, x19
    bl parse_number
    mov x24, x1  // Y (after)
    mov x19, x0

    // Store rule: [before, after]
    str x23, [x21]
    str x24, [x21, #8]
    add x21, x21, #16
    add x22, x22, #1

    // Skip to next line
    mov x0, x19
    bl skip_to_newline
    mov x19, x0
    b parse_rules_loop

parse_rules_done:
    // Save rule count
    adrp x0, num_rules@PAGE
    add x0, x0, num_rules@PAGEOFF
    str x22, [x0]

    // Skip blank line(s)
    mov x0, x19
    bl skip_blank_lines
    mov x19, x0

    // Parse updates section
    adrp x21, updates@PAGE
    add x21, x21, updates@PAGEOFF
    mov x22, #0  // Update count

parse_updates_loop:
    cmp x19, x20
    b.ge parse_updates_done

    // Check if line is empty
    ldrb w0, [x19]
    cmp w0, #'\n'
    b.eq parse_update_skip
    cmp w0, #0
    b.eq parse_updates_done

    mov x23, #0  // Page count
    mov x24, x21
    add x24, x24, #8  // Skip count field

parse_update_pages:
    cmp x19, x20
    b.ge parse_update_done
    ldrb w0, [x19]
    cmp w0, #'\n'
    b.eq parse_update_done
    cmp w0, #0
    b.eq parse_update_done

    // Parse number
    mov x0, x19
    bl parse_number
    str x1, [x24], #8
    mov x19, x0
    add x23, x23, #1

    // Skip comma if present
    ldrb w0, [x19]
    cmp w0, #','
    b.ne parse_update_pages
    add x19, x19, #1
    b parse_update_pages

parse_update_done:
    // Store page count
    str x23, [x21]
    // Move to next update
    mov x0, #MAX_PAGES
    add x0, x0, #1
    lsl x0, x0, #3
    add x21, x21, x0
    add x22, x22, #1

parse_update_skip:
    // Skip to next line
    mov x0, x19
    bl skip_to_newline
    mov x19, x0
    b parse_updates_loop

parse_updates_done:
    adrp x0, num_updates@PAGE
    add x0, x0, num_updates@PAGEOFF
    str x22, [x0]

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Parse a number from string
// x0 = pointer, returns x0 = new pointer, x1 = number
parse_number:
    mov x1, #0  // Result
    mov x2, #10 // Base
parse_num_loop:
    ldrb w3, [x0]
    cmp w3, #'0'
    b.lt parse_num_done
    cmp w3, #'9'
    b.gt parse_num_done
    sub w3, w3, #'0'
    mul x1, x1, x2
    add x1, x1, x3
    add x0, x0, #1
    b parse_num_loop
parse_num_done:
    ret

// Skip to newline
skip_to_newline:
skip_nl_loop:
    ldrb w1, [x0]
    cmp w1, #'\n'
    b.eq skip_nl_found
    cmp w1, #0
    b.eq skip_nl_done
    add x0, x0, #1
    b skip_nl_loop
skip_nl_found:
    add x0, x0, #1
skip_nl_done:
    ret

// Skip blank lines
skip_blank_lines:
sbl_loop:
    ldrb w1, [x0]
    cmp w1, #'\n'
    b.ne sbl_done
    add x0, x0, #1
    b sbl_loop
sbl_done:
    ret

// Check if update is valid
// x0 = pointer to update (count at [x0], pages at [x0+8])
// Returns 1 if valid, 0 if invalid
is_valid_update:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    str x27, [sp, #-16]!

    mov x19, x0  // Update pointer
    ldr x20, [x19]  // Page count
    add x19, x19, #8  // Point to pages

    // For each page i in update
    mov x21, #0  // i = 0
outer_page_loop:
    cmp x21, x20
    b.ge valid_true

    ldr x22, [x19, x21, lsl #3]  // page = update->pages[i]

    // For each rule
    adrp x23, rules@PAGE
    add x23, x23, rules@PAGEOFF
    adrp x0, num_rules@PAGE
    add x0, x0, num_rules@PAGEOFF
    ldr x24, [x0]  // Number of rules

rule_loop:
    cbz x24, next_outer_page

    ldr x25, [x23]       // rule.before
    ldr x26, [x23, #8]   // rule.after
    add x23, x23, #16

    // If rule.before != page, skip this rule
    cmp x25, x22
    b.ne next_rule

    // rule.before == page
    // Check if rule.after is in the update and comes before position i
    mov x27, #0  // k = 0
search_after:
    cmp x27, x20
    b.ge next_rule

    ldr x0, [x19, x27, lsl #3]  // update->pages[k]
    cmp x0, x26                  // == rule.after?
    b.ne search_after_next

    // Found rule.after at position k
    // If k < i, that's a violation
    cmp x27, x21
    b.lt valid_false

    // Otherwise continue
    b next_rule

search_after_next:
    add x27, x27, #1
    b search_after

next_rule:
    sub x24, x24, #1
    b rule_loop

next_outer_page:
    add x21, x21, #1
    b outer_page_loop

valid_true:
    mov x0, #1
    ldr x27, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

valid_false:
    mov x0, #0
    ldr x27, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 1: Sum middle pages of valid updates
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    adrp x19, updates@PAGE
    add x19, x19, updates@PAGEOFF
    adrp x0, num_updates@PAGE
    add x0, x0, num_updates@PAGEOFF
    ldr x20, [x0]
    mov x21, #0  // Sum

part1_loop:
    cbz x20, part1_done

    mov x0, x19
    bl is_valid_update
    cmp x0, #0
    b.eq part1_next

    // Get middle page
    ldr x22, [x19]  // Page count
    lsr x22, x22, #1  // Divide by 2
    add x0, x19, #8
    ldr x0, [x0, x22, lsl #3]
    add x21, x21, x0

part1_next:
    mov x0, #MAX_PAGES
    add x0, x0, #1
    lsl x0, x0, #3
    add x19, x19, x0
    sub x20, x20, #1
    b part1_loop

part1_done:
    mov x0, x21
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Fix order of update using bubble sort with rule comparator
// x0 = pointer to update
// Modifies update in place
fix_update:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0  // Update pointer
    ldr x20, [x19]  // Page count
    add x19, x19, #8  // Point to pages

    // Bubble sort with rule-based comparator
    mov x21, #1  // Swapped flag

fix_outer:
    cbz x21, fix_done
    mov x21, #0  // Reset swapped

    mov x22, #0  // i
fix_inner:
    add x23, x22, #1
    cmp x23, x20
    b.ge fix_outer

    // Compare pages[i] and pages[i+1] using rules
    ldr x24, [x19, x22, lsl #3]  // pages[i]
    ldr x25, [x19, x23, lsl #3]  // pages[i+1]

    mov x0, x24
    mov x1, x25
    bl should_swap
    cmp x0, #1
    b.ne fix_no_swap

    // Swap
    str x25, [x19, x22, lsl #3]
    str x24, [x19, x23, lsl #3]
    mov x21, #1  // Set swapped

fix_no_swap:
    add x22, x22, #1
    b fix_inner

fix_done:
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Check if two pages should be swapped based on rules
// x0 = page a, x1 = page b
// Returns 1 if should swap (b should come before a), 0 otherwise
should_swap:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0  // page a
    mov x20, x1  // page b

    // Check if there's a rule b|a (b must come before a)
    adrp x21, rules@PAGE
    add x21, x21, rules@PAGEOFF
    adrp x0, num_rules@PAGE
    add x0, x0, num_rules@PAGEOFF
    ldr x22, [x0]

swap_check_loop:
    cbz x22, swap_no

    ldr x0, [x21]      // before
    ldr x1, [x21, #8]  // after
    add x21, x21, #16

    // If rule is b|a, should swap
    cmp x0, x20
    b.ne swap_next
    cmp x1, x19
    b.ne swap_next

    mov x0, #1
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

swap_next:
    sub x22, x22, #1
    b swap_check_loop

swap_no:
    mov x0, #0
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2: Fix invalid updates and sum middle pages
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    adrp x19, updates@PAGE
    add x19, x19, updates@PAGEOFF
    adrp x0, num_updates@PAGE
    add x0, x0, num_updates@PAGEOFF
    ldr x20, [x0]
    mov x21, #0  // Sum

part2_loop:
    cbz x20, part2_done

    mov x0, x19
    bl is_valid_update
    cmp x0, #1
    b.eq part2_next  // Skip if already valid

    // Fix the update
    mov x0, x19
    bl fix_update

    // Get middle page
    ldr x22, [x19]  // Page count
    lsr x22, x22, #1  // Divide by 2
    add x0, x19, #8
    ldr x0, [x0, x22, lsl #3]
    add x21, x21, x0

part2_next:
    mov x0, #MAX_PAGES
    add x0, x0, #1
    lsl x0, x0, #3
    add x19, x19, x0
    sub x20, x20, #1
    b part2_loop

part2_done:
    mov x0, x21
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print string
// x0 = string pointer
print_str:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    mov x20, #0
ps_len_loop:
    ldrb w1, [x19, x20]
    cbz w1, ps_write
    add x20, x20, #1
    b ps_len_loop
ps_write:
    movz x16, #0x2000, lsl #16
    movk x16, #0x0004
    mov x0, #STDOUT
    mov x1, x19
    mov x2, x20
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print number
// x0 = number to print
print_number:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    adrp x19, output_buffer@PAGE
    add x19, x19, output_buffer@PAGEOFF
    add x19, x19, #31
    mov w1, #0
    strb w1, [x19]
    mov x20, x0
    mov x2, #10

pn_loop:
    udiv x3, x20, x2
    msub x4, x3, x2, x20
    add w4, w4, #'0'
    sub x19, x19, #1
    strb w4, [x19]
    mov x20, x3
    cbnz x20, pn_loop

    mov x0, x19
    bl print_str

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
