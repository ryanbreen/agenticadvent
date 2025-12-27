// Advent of Code 2023 Day 4: Scratchcards - ARM64 Assembly for macOS

.global _start
.align 4

.data
    filename: .asciz "../input.txt"
    msg_part1: .asciz "Part 1: "
    msg_part2: .asciz "Part 2: "
    newline: .asciz "\n"

.bss
    .align 4
    buffer: .skip 32768           // File contents
    num_buffer: .skip 32          // For printing numbers
    winning: .skip 128            // Bitmap for winning numbers (100 possible values)
    matches: .skip 2048           // Match counts per card (up to 256 cards)
    copies: .skip 2048            // Copy counts per card (up to 256 cards)
    card_count: .skip 8           // Number of cards

.text

_start:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    bl read_file
    cbz x0, exit_error

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

// Clear winning bitmap
clear_winning:
    adrp x0, winning@PAGE
    add x0, x0, winning@PAGEOFF
    mov x1, #16             // 128 bytes / 8 = 16 iterations
    mov x2, #0
1:  str x2, [x0], #8
    subs x1, x1, #1
    b.ne 1b
    ret

// Set bit in winning bitmap for number in w0
set_winning:
    adrp x1, winning@PAGE
    add x1, x1, winning@PAGEOFF
    lsr x2, x0, #3          // byte index
    and x3, x0, #7          // bit index
    mov x4, #1
    lsl x4, x4, x3
    ldrb w5, [x1, x2]
    orr w5, w5, w4
    strb w5, [x1, x2]
    ret

// Check if number in w0 is in winning set, returns 1 if found
check_winning:
    adrp x1, winning@PAGE
    add x1, x1, winning@PAGEOFF
    lsr x2, x0, #3          // byte index
    and x3, x0, #7          // bit index
    mov x4, #1
    lsl x4, x4, x3
    ldrb w5, [x1, x2]
    and w0, w5, w4
    cbnz w0, 1f
    mov x0, #0
    ret
1:  mov x0, #1
    ret

// Parse number from buffer at x0, returns number in x0, advances x1 past number
// Input: x1 = pointer to buffer
// Output: x0 = parsed number, x1 = updated pointer
parse_number:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x1             // current position
    mov x0, #0              // result

    // Skip spaces
1:  ldrb w2, [x19]
    cmp w2, #' '
    b.ne 2f
    add x19, x19, #1
    b 1b

2:  // Check if digit
    ldrb w2, [x19]
    cmp w2, #'0'
    b.lt parse_done
    cmp w2, #'9'
    b.gt parse_done

    // Accumulate digits
3:  ldrb w2, [x19]
    cmp w2, #'0'
    b.lt parse_done
    cmp w2, #'9'
    b.gt parse_done

    mov x3, #10
    mul x0, x0, x3
    sub w2, w2, #'0'
    add x0, x0, x2
    add x19, x19, #1
    b 3b

parse_done:
    mov x1, x19
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Skip to colon in line, returns pointer after colon
// Input: x1 = pointer
// Output: x1 = pointer after colon
skip_to_colon:
1:  ldrb w0, [x1]
    cbz w0, 2f
    cmp w0, #':'
    b.eq 2f
    add x1, x1, #1
    b 1b
2:  add x1, x1, #1          // skip colon
    ret

// Skip to pipe in line, returns pointer after pipe
// Input: x1 = pointer
// Output: x1 = pointer after pipe
skip_to_pipe:
1:  ldrb w0, [x1]
    cbz w0, 2f
    cmp w0, #'|'
    b.eq 2f
    add x1, x1, #1
    b 1b
2:  add x1, x1, #1          // skip pipe
    ret

// Check if at end of line or end of section
// Input: x1 = pointer
// Output: x0 = 1 if at end/pipe/newline
is_end_of_section:
    ldrb w0, [x1]
    cbz w0, 1f
    cmp w0, #'\n'
    b.eq 1f
    cmp w0, #'|'
    b.eq 1f
    mov x0, #0
    ret
1:  mov x0, #1
    ret

// Process one card, returns match count in x0
// Input: x1 = pointer to start of card line
// Output: x0 = match count, x1 = pointer to next line
process_card:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x1             // current position
    mov x21, #0             // match count

    // Clear winning bitmap
    bl clear_winning

    // Skip "Card XXX:"
    mov x1, x19
    bl skip_to_colon
    mov x19, x1

    // Parse winning numbers until |
parse_winning_loop:
    // Skip spaces
1:  ldrb w0, [x19]
    cmp w0, #' '
    b.ne 2f
    add x19, x19, #1
    b 1b

2:  // Check for |
    ldrb w0, [x19]
    cmp w0, #'|'
    b.eq winning_done

    // Parse number
    mov x1, x19
    bl parse_number
    mov x20, x0             // save number
    mov x19, x1             // update position

    // Set in winning bitmap
    mov x0, x20
    bl set_winning
    b parse_winning_loop

winning_done:
    add x19, x19, #1        // skip |

    // Parse your numbers until newline
parse_your_loop:
    // Skip spaces
1:  ldrb w0, [x19]
    cmp w0, #' '
    b.ne 2f
    add x19, x19, #1
    b 1b

2:  // Check for newline or end
    ldrb w0, [x19]
    cbz w0, your_done
    cmp w0, #'\n'
    b.eq your_done

    // Parse number
    mov x1, x19
    bl parse_number
    mov x20, x0             // save number
    mov x19, x1             // update position

    // Check if in winning set
    mov x0, x20
    bl check_winning
    cbz x0, parse_your_loop
    add x21, x21, #1        // increment match count
    b parse_your_loop

your_done:
    // Skip newline if present
    ldrb w0, [x19]
    cmp w0, #'\n'
    b.ne 1f
    add x19, x19, #1
1:
    mov x0, x21             // return match count
    mov x1, x19             // return updated position

    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Solve Part 1 - Score each card with 2^(matches-1)
solve_part1:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, #0             // total score
    mov x20, #0             // card index

    adrp x21, buffer@PAGE
    add x21, x21, buffer@PAGEOFF

    // Also store matches for part 2
    adrp x22, matches@PAGE
    add x22, x22, matches@PAGEOFF

part1_loop:
    // Check if at end of file
    ldrb w0, [x21]
    cbz w0, part1_done

    // Process card
    mov x1, x21
    bl process_card
    mov x21, x1             // update position

    // Store match count for part 2
    str x0, [x22, x20, lsl #3]
    add x20, x20, #1

    // Calculate score: 2^(matches-1) if matches > 0
    cbz x0, part1_loop      // no matches, no score

    sub x0, x0, #1          // matches - 1
    mov x1, #1
    lsl x1, x1, x0          // 2^(matches-1)
    add x19, x19, x1
    b part1_loop

part1_done:
    // Save card count
    adrp x0, card_count@PAGE
    add x0, x0, card_count@PAGEOFF
    str x20, [x0]

    mov x0, x19
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Solve Part 2 - Cascade card copies
solve_part2:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    adrp x19, card_count@PAGE
    add x19, x19, card_count@PAGEOFF
    ldr x19, [x19]          // number of cards

    adrp x20, matches@PAGE
    add x20, x20, matches@PAGEOFF

    adrp x21, copies@PAGE
    add x21, x21, copies@PAGEOFF

    // Initialize all copy counts to 1
    mov x22, #0
init_copies:
    cmp x22, x19
    b.ge copies_initialized
    mov x0, #1
    str x0, [x21, x22, lsl #3]
    add x22, x22, #1
    b init_copies

copies_initialized:
    // Process each card
    mov x22, #0             // current card index

cascade_loop:
    cmp x22, x19
    b.ge cascade_done

    // Get match count for this card
    ldr x23, [x20, x22, lsl #3]     // matches for card i
    ldr x24, [x21, x22, lsl #3]     // copies of card i

    // Add copies to next M cards
    cbz x23, next_card

    mov x0, #1              // j = 1
add_copies_loop:
    cmp x0, x23
    b.gt next_card

    // Calculate target card index
    add x1, x22, x0         // target = i + j
    cmp x1, x19
    b.ge next_card          // don't go past end

    // Add current card's copies to target
    ldr x2, [x21, x1, lsl #3]
    add x2, x2, x24
    str x2, [x21, x1, lsl #3]

    add x0, x0, #1
    b add_copies_loop

next_card:
    add x22, x22, #1
    b cascade_loop

cascade_done:
    // Sum all copies
    mov x0, #0              // total
    mov x22, #0             // index

sum_copies:
    cmp x22, x19
    b.ge sum_done
    ldr x1, [x21, x22, lsl #3]
    add x0, x0, x1
    add x22, x22, #1
    b sum_copies

sum_done:
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
