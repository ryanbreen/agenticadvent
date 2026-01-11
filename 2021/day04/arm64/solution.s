// ARM64 Assembly solution for AoC 2021 Day 4 - Giant Squid (Bingo)
// macOS ARM64 syscalls

.global _start
.align 2

.equ STDOUT, 1
.equ MAX_NUMBERS, 128        // Max bingo numbers to draw
.equ MAX_BOARDS, 128         // Max bingo boards
.equ BOARD_SIZE, 25          // 5x5 = 25 cells per board

.data
filename: .asciz "../input.txt"
part1_msg: .asciz "Part 1: "
part2_msg: .asciz "Part 2: "
newline: .asciz "\n"

.align 3
file_buffer: .skip 32768
draw_numbers: .skip MAX_NUMBERS * 4     // Numbers to draw (32-bit each)
draw_count: .skip 8                      // How many numbers to draw
boards: .skip MAX_BOARDS * BOARD_SIZE * 4    // Board values (32-bit each)
marked: .skip MAX_BOARDS * BOARD_SIZE        // Marked cells (1 byte each)
board_won: .skip MAX_BOARDS                  // Which boards have won
board_count: .skip 8                         // How many boards
output_buffer: .skip 32

.text
_start:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Open file
    mov x16, #5             // open syscall
    adrp x0, filename@PAGE
    add x0, x0, filename@PAGEOFF
    mov x1, #0              // O_RDONLY
    mov x2, #0
    svc #0x80
    cmp x0, #0
    b.lt exit_error
    mov x19, x0             // Save fd

    // Read file
    mov x16, #3             // read syscall
    mov x0, x19
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #32768
    svc #0x80
    mov x20, x0             // Save bytes read

    // Close file
    mov x16, #6             // close syscall
    mov x0, x19
    svc #0x80

    // Parse input
    adrp x0, file_buffer@PAGE
    add x0, x0, file_buffer@PAGEOFF
    mov x1, x20
    bl parse_input

    // Part 1: Find first winning board
    bl part1
    mov x21, x0             // Save Part 1 result

    // Print Part 1
    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_str
    mov x0, x21
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Part 2: Find last winning board
    bl part2
    mov x22, x0             // Save Part 2 result

    // Print Part 2
    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_str
    mov x0, x22
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_str

    // Exit success
    mov x0, #0
    mov x16, #1
    svc #0x80

exit_error:
    mov x0, #1
    mov x16, #1
    svc #0x80

// Parse input: first line = comma-separated numbers, then boards
// x0 = buffer, x1 = length
parse_input:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    mov x19, x0             // Buffer pointer
    add x20, x19, x1        // Buffer end

    // Parse draw numbers (first line, comma-separated)
    adrp x21, draw_numbers@PAGE
    add x21, x21, draw_numbers@PAGEOFF
    mov x22, #0             // Draw count

parse_draw_numbers:
    mov x23, #0             // Current number
parse_draw_digit:
    cmp x19, x20
    b.ge parse_draw_done
    ldrb w24, [x19]

    // Check for comma or newline (end of number)
    cmp w24, #','
    b.eq store_draw_number
    cmp w24, #'\n'
    b.eq store_draw_number_end
    cmp w24, #'\r'
    b.eq skip_cr

    // Must be a digit
    cmp w24, #'0'
    b.lt parse_draw_done
    cmp w24, #'9'
    b.gt parse_draw_done

    // Accumulate digit: number = number * 10 + digit
    mov x25, #10
    mul x23, x23, x25
    sub w24, w24, #'0'
    add x23, x23, x24
    add x19, x19, #1
    b parse_draw_digit

skip_cr:
    add x19, x19, #1
    b parse_draw_digit

store_draw_number:
    str w23, [x21, x22, lsl #2]
    add x22, x22, #1
    mov x23, #0
    add x19, x19, #1
    b parse_draw_digit

store_draw_number_end:
    str w23, [x21, x22, lsl #2]
    add x22, x22, #1
    add x19, x19, #1

parse_draw_done:
    adrp x0, draw_count@PAGE
    add x0, x0, draw_count@PAGEOFF
    str x22, [x0]

    // Parse boards (5x5 grids separated by blank lines)
    adrp x21, boards@PAGE
    add x21, x21, boards@PAGEOFF
    mov x22, #0             // Board count
    mov x23, #0             // Cell count within current board

parse_boards_loop:
    cmp x19, x20
    b.ge parse_boards_done

    // Skip whitespace and newlines
    ldrb w24, [x19]
    cmp w24, #'\n'
    b.eq skip_whitespace
    cmp w24, #'\r'
    b.eq skip_whitespace
    cmp w24, #' '
    b.eq skip_whitespace
    cmp w24, #0
    b.eq parse_boards_done

    // Check if digit
    cmp w24, #'0'
    b.lt skip_whitespace
    cmp w24, #'9'
    b.gt skip_whitespace

    // Parse number
    mov x25, #0             // Current number
parse_board_number:
    cmp x19, x20
    b.ge store_board_number
    ldrb w24, [x19]
    cmp w24, #'0'
    b.lt store_board_number
    cmp w24, #'9'
    b.gt store_board_number

    mov x26, #10
    mul x25, x25, x26
    sub w24, w24, #'0'
    add x25, x25, x24
    add x19, x19, #1
    b parse_board_number

store_board_number:
    // Calculate offset: board_count * 25 + cell_count
    mov x26, #BOARD_SIZE
    mul x26, x22, x26
    add x26, x26, x23
    str w25, [x21, x26, lsl #2]
    add x23, x23, #1

    // Check if board is complete
    cmp x23, #BOARD_SIZE
    b.lt parse_boards_loop

    // Board complete, move to next
    add x22, x22, #1
    mov x23, #0
    b parse_boards_loop

skip_whitespace:
    add x19, x19, #1
    b parse_boards_loop

parse_boards_done:
    adrp x0, board_count@PAGE
    add x0, x0, board_count@PAGEOFF
    str x22, [x0]

    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Clear all marked cells
clear_marked:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    adrp x0, marked@PAGE
    add x0, x0, marked@PAGEOFF
    adrp x1, board_count@PAGE
    add x1, x1, board_count@PAGEOFF
    ldr x1, [x1]
    mov x2, #BOARD_SIZE
    mul x1, x1, x2          // Total cells
    mov x2, #0

clear_loop:
    cmp x2, x1
    b.ge clear_done
    strb wzr, [x0, x2]
    add x2, x2, #1
    b clear_loop

clear_done:
    ldp x29, x30, [sp], #16
    ret

// Clear board_won array
clear_won:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    adrp x0, board_won@PAGE
    add x0, x0, board_won@PAGEOFF
    adrp x1, board_count@PAGE
    add x1, x1, board_count@PAGEOFF
    ldr x1, [x1]
    mov x2, #0

clear_won_loop:
    cmp x2, x1
    b.ge clear_won_done
    strb wzr, [x0, x2]
    add x2, x2, #1
    b clear_won_loop

clear_won_done:
    ldp x29, x30, [sp], #16
    ret

// Mark a number on a specific board
// x0 = board index, x1 = number to mark
mark_number_on_board:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0             // Board index
    mov x20, x1             // Number to mark

    adrp x21, boards@PAGE
    add x21, x21, boards@PAGEOFF
    adrp x22, marked@PAGE
    add x22, x22, marked@PAGEOFF

    // Calculate base offset for this board
    mov x0, #BOARD_SIZE
    mul x0, x19, x0         // board_offset = board_index * 25

    // Search all 25 cells
    mov x1, #0              // Cell index
mark_search_loop:
    cmp x1, #BOARD_SIZE
    b.ge mark_not_found

    add x2, x0, x1          // Global cell index
    ldr w3, [x21, x2, lsl #2]   // Board value at this cell
    cmp w3, w20
    b.ne mark_next_cell

    // Found! Mark this cell
    mov w4, #1
    strb w4, [x22, x2]
    b mark_done

mark_next_cell:
    add x1, x1, #1
    b mark_search_loop

mark_not_found:
mark_done:
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Check if a board has won
// x0 = board index
// Returns 1 in x0 if won, 0 otherwise
check_winner:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!

    mov x19, x0             // Board index

    adrp x20, marked@PAGE
    add x20, x20, marked@PAGEOFF

    // Calculate base offset
    mov x0, #BOARD_SIZE
    mul x21, x19, x0        // board_offset = board_index * 25

    // Check rows
    mov x0, #0              // Row index
check_row_loop:
    cmp x0, #5
    b.ge check_cols

    // Check if all 5 cells in this row are marked
    mov x1, #0              // Column index
    mov x2, #5              // Count of marked cells
check_row_cell:
    cmp x1, #5
    b.ge check_row_result

    // Cell index = row * 5 + col
    mov x3, #5
    mul x3, x0, x3
    add x3, x3, x1
    add x3, x3, x21         // Global cell index
    ldrb w4, [x20, x3]
    cbz w4, row_not_complete
    add x1, x1, #1
    b check_row_cell

row_not_complete:
    add x0, x0, #1
    b check_row_loop

check_row_result:
    // All 5 cells marked, winner!
    mov x0, #1
    b check_done

check_cols:
    // Check columns
    mov x0, #0              // Column index
check_col_loop:
    cmp x0, #5
    b.ge no_winner

    // Check if all 5 cells in this column are marked
    mov x1, #0              // Row index
check_col_cell:
    cmp x1, #5
    b.ge check_col_result

    // Cell index = row * 5 + col
    mov x3, #5
    mul x3, x1, x3
    add x3, x3, x0
    add x3, x3, x21         // Global cell index
    ldrb w4, [x20, x3]
    cbz w4, col_not_complete
    add x1, x1, #1
    b check_col_cell

col_not_complete:
    add x0, x0, #1
    b check_col_loop

check_col_result:
    // All 5 cells marked, winner!
    mov x0, #1
    b check_done

no_winner:
    mov x0, #0

check_done:
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Calculate score for a board
// x0 = board index, x1 = last called number
// Returns score in x0
calculate_score:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0             // Board index
    mov x20, x1             // Last number

    adrp x21, boards@PAGE
    add x21, x21, boards@PAGEOFF
    adrp x22, marked@PAGE
    add x22, x22, marked@PAGEOFF

    // Calculate base offset
    mov x0, #BOARD_SIZE
    mul x23, x19, x0        // board_offset = board_index * 25

    // Sum unmarked cells
    mov x24, #0             // Sum
    mov x0, #0              // Cell index
sum_loop:
    cmp x0, #BOARD_SIZE
    b.ge sum_done

    add x1, x23, x0         // Global cell index
    ldrb w2, [x22, x1]      // Marked flag
    cbnz w2, sum_next       // Skip if marked

    ldr w3, [x21, x1, lsl #2]   // Board value
    add x24, x24, x3

sum_next:
    add x0, x0, #1
    b sum_loop

sum_done:
    mul x0, x24, x20        // score = sum * last_number

    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 1: Find first winning board and return score
part1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!

    // Clear marked array
    bl clear_marked

    adrp x19, draw_numbers@PAGE
    add x19, x19, draw_numbers@PAGEOFF
    adrp x20, draw_count@PAGE
    add x20, x20, draw_count@PAGEOFF
    ldr x20, [x20]
    adrp x21, board_count@PAGE
    add x21, x21, board_count@PAGEOFF
    ldr x21, [x21]

    mov x22, #0             // Draw index
part1_draw_loop:
    cmp x22, x20
    b.ge part1_no_winner

    ldr w23, [x19, x22, lsl #2]   // Current number

    // Mark this number on all boards
    mov x24, #0             // Board index
part1_mark_loop:
    cmp x24, x21
    b.ge part1_check_winners

    mov x0, x24
    mov x1, x23
    bl mark_number_on_board

    add x24, x24, #1
    b part1_mark_loop

part1_check_winners:
    // Check all boards for winner
    mov x24, #0
part1_winner_loop:
    cmp x24, x21
    b.ge part1_next_draw

    mov x0, x24
    bl check_winner
    cbz x0, part1_next_board

    // Winner found!
    mov x0, x24
    mov x1, x23
    bl calculate_score
    b part1_done

part1_next_board:
    add x24, x24, #1
    b part1_winner_loop

part1_next_draw:
    add x22, x22, #1
    b part1_draw_loop

part1_no_winner:
    mov x0, #0

part1_done:
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Part 2: Find last winning board and return score
part2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    // Clear marked array and won array
    bl clear_marked
    bl clear_won

    adrp x19, draw_numbers@PAGE
    add x19, x19, draw_numbers@PAGEOFF
    adrp x20, draw_count@PAGE
    add x20, x20, draw_count@PAGEOFF
    ldr x20, [x20]
    adrp x21, board_count@PAGE
    add x21, x21, board_count@PAGEOFF
    ldr x21, [x21]
    adrp x25, board_won@PAGE
    add x25, x25, board_won@PAGEOFF

    mov x22, #0             // Draw index
    mov x26, #0             // Last score
part2_draw_loop:
    cmp x22, x20
    b.ge part2_done

    ldr w23, [x19, x22, lsl #2]   // Current number

    // Mark this number on all boards
    mov x24, #0             // Board index
part2_mark_loop:
    cmp x24, x21
    b.ge part2_check_winners

    mov x0, x24
    mov x1, x23
    bl mark_number_on_board

    add x24, x24, #1
    b part2_mark_loop

part2_check_winners:
    // Check all boards for new winners
    mov x24, #0
part2_winner_loop:
    cmp x24, x21
    b.ge part2_next_draw

    // Skip if already won
    ldrb w27, [x25, x24]
    cbnz w27, part2_next_board

    mov x0, x24
    bl check_winner
    cbz x0, part2_next_board

    // New winner! Mark as won and save score
    mov w27, #1
    strb w27, [x25, x24]

    mov x0, x24
    mov x1, x23
    bl calculate_score
    mov x26, x0             // Save as last score

part2_next_board:
    add x24, x24, #1
    b part2_winner_loop

part2_next_draw:
    add x22, x22, #1
    b part2_draw_loop

part2_done:
    mov x0, x26             // Return last score

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print null-terminated string
// x0 = string pointer
print_str:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    mov x20, #0
ps_len:
    ldrb w1, [x19, x20]
    cbz w1, ps_write
    add x20, x20, #1
    b ps_len
ps_write:
    mov x16, #4             // write syscall
    mov x0, #STDOUT
    mov x1, x19
    mov x2, x20
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// Print decimal number
// x0 = number to print
print_number:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x19, x20, [sp, #-16]!

    adrp x19, output_buffer@PAGE
    add x19, x19, output_buffer@PAGEOFF
    add x19, x19, #31       // Start at end of buffer
    mov w1, #0
    strb w1, [x19]          // Null terminator
    mov x20, x0
    mov x2, #10

pn_loop:
    udiv x3, x20, x2
    msub x4, x3, x2, x20    // remainder = x20 - (x3 * 10)
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
