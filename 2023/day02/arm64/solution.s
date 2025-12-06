.global _main
.align 4

// Constants for Part 1
.equ MAX_RED, 12
.equ MAX_GREEN, 13
.equ MAX_BLUE, 14

.data
filename:       .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "\nPart 2: "
newline:        .asciz "\n"

.bss
.align 4
file_buffer:    .skip 16384      // Buffer for file contents
output_buffer:  .skip 32         // Buffer for number output

.text
_main:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Open file
    mov     x16, #5                 // open syscall
    adrp    x0, filename@PAGE
    add     x0, x0, filename@PAGEOFF
    mov     x1, #0                  // O_RDONLY
    mov     x2, #0
    svc     #0x80
    cmp     x0, #0
    b.lt    exit_error
    mov     x19, x0                 // Save file descriptor

    // Read file
    mov     x16, #3                 // read syscall
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #16384
    svc     #0x80
    mov     x20, x0                 // Save file size

    // Close file
    mov     x16, #6                 // close syscall
    mov     x0, x19
    svc     #0x80

    // Process the file
    adrp    x0, file_buffer@PAGE
    add     x0, x0, file_buffer@PAGEOFF
    mov     x1, x20                 // Size of buffer
    bl      process_games

    // Print "Part 1: "
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    bl      print_str

    // Print Part 1 result
    mov     x0, x21
    bl      print_number

    // Print "Part 2: "
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    bl      print_str

    // Print Part 2 result
    mov     x0, x22
    bl      print_number

    // Print newline
    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_str

    // Exit successfully
    mov     x0, #0
    ldp     x29, x30, [sp], #16
    mov     x16, #1                 // exit syscall
    svc     #0x80

exit_error:
    mov     x0, #1
    ldp     x29, x30, [sp], #16
    mov     x16, #1
    svc     #0x80

// Print null-terminated string
// x0 = string pointer
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0                 // Save string pointer

    // Find string length
    mov     x1, x0
strlen_loop:
    ldrb    w2, [x1], #1
    cbnz    w2, strlen_loop
    sub     x20, x1, x19
    sub     x20, x20, #1            // Length (excluding null)

    // Write to stdout
    mov     x16, #4                 // write syscall
    mov     x0, #1                  // stdout
    mov     x1, x19
    mov     x2, x20
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Process all games
// x0 = buffer pointer, x1 = buffer size
// Returns: x21 = part1 sum, x22 = part2 sum
process_games:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!

    mov     x19, x0                 // Buffer pointer
    add     x20, x0, x1             // End of buffer
    mov     x21, #0                 // Part 1 sum
    mov     x22, #0                 // Part 2 sum

process_loop:
    cmp     x19, x20
    b.ge    process_done

    // Find end of line
    mov     x0, x19
    mov     x1, x20
    bl      find_newline
    mov     x23, x0                 // End of current line

    // Process this line
    mov     x0, x19                 // Line start
    mov     x1, x23                 // Line end
    bl      process_line

    mov     x24, x0                 // Game ID
    mov     x25, x1                 // Is possible (1/0)
    mov     x26, x2                 // Power

    // Add to Part 1 if possible
    cmp     x25, #1
    b.ne    skip_part1
    add     x21, x21, x24

skip_part1:
    // Add power to Part 2
    add     x22, x22, x26

    // Move to next line
    add     x19, x23, #1
    b       process_loop

process_done:
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Find newline in buffer
// x0 = start, x1 = end
// Returns: x0 = position of newline (or end)
find_newline:
    mov     x2, x0
find_nl_loop:
    cmp     x2, x1
    b.ge    find_nl_done
    ldrb    w3, [x2]
    cmp     w3, #'\n'
    b.eq    find_nl_done
    add     x2, x2, #1
    b       find_nl_loop
find_nl_done:
    mov     x0, x2
    ret

// Process a single line
// x0 = line start, x1 = line end
// Returns: x0 = game ID, x1 = is_possible, x2 = power
process_line:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!

    mov     x19, x0                 // Line start
    mov     x20, x1                 // Line end

    // Skip "Game "
    add     x19, x19, #5

    // Parse game ID
    mov     x0, x19
    mov     x1, x20
    bl      parse_number
    mov     x21, x0                 // Game ID
    mov     x19, x1                 // Updated position

    // Skip ": "
    add     x19, x19, #2

    // Initialize mins for Part 2
    mov     x22, #0                 // min_red
    mov     x23, #0                 // min_green
    mov     x24, #0                 // min_blue
    mov     x25, #1                 // is_possible

    // Process all draws
parse_draws:
    cmp     x19, x20
    b.ge    draws_done

    // Parse a number
    mov     x0, x19
    mov     x1, x20
    bl      parse_number
    mov     x26, x0                 // Count
    mov     x19, x1                 // Position after number

    // Skip space
    add     x19, x19, #1

    // Check color
    ldrb    w0, [x19]
    cmp     w0, #'r'                // red
    b.eq    handle_red
    cmp     w0, #'g'                // green
    b.eq    handle_green
    cmp     w0, #'b'                // blue
    b.eq    handle_blue
    b       skip_color

handle_red:
    // Update min_red
    cmp     x26, x22
    csel    x22, x26, x22, gt
    // Check if possible
    cmp     x26, #MAX_RED
    b.le    red_ok
    mov     x25, #0                 // Not possible
red_ok:
    add     x19, x19, #3            // Skip "red"
    b       skip_color

handle_green:
    // Update min_green
    cmp     x26, x23
    csel    x23, x26, x23, gt
    // Check if possible
    cmp     x26, #MAX_GREEN
    b.le    green_ok
    mov     x25, #0                 // Not possible
green_ok:
    add     x19, x19, #5            // Skip "green"
    b       skip_color

handle_blue:
    // Update min_blue
    cmp     x26, x24
    csel    x24, x26, x24, gt
    // Check if possible
    cmp     x26, #MAX_BLUE
    b.le    blue_ok
    mov     x25, #0                 // Not possible
blue_ok:
    add     x19, x19, #4            // Skip "blue"

skip_color:
    // Skip comma/semicolon and spaces
skip_sep_loop:
    cmp     x19, x20
    b.ge    draws_done
    ldrb    w0, [x19]
    cmp     w0, #','
    b.eq    skip_sep_char
    cmp     w0, #';'
    b.eq    skip_sep_char
    cmp     w0, #' '
    b.eq    skip_sep_char
    b       parse_draws

skip_sep_char:
    add     x19, x19, #1
    b       skip_sep_loop

draws_done:
    // Calculate power (min_red * min_green * min_blue)
    mul     x0, x22, x23
    mul     x0, x0, x24

    mov     x2, x0                  // Power
    mov     x1, x25                 // Is possible
    mov     x0, x21                 // Game ID

    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Parse a number from string
// x0 = start position, x1 = end position
// Returns: x0 = number, x1 = position after number
parse_number:
    mov     x2, #0                  // Result
    mov     x3, x0                  // Current position

parse_num_loop:
    cmp     x3, x1
    b.ge    parse_num_done
    ldrb    w4, [x3]
    cmp     w4, #'0'
    b.lt    parse_num_done
    cmp     w4, #'9'
    b.gt    parse_num_done

    // Digit found: result = result * 10 + (digit - '0')
    sub     w4, w4, #'0'
    mov     x5, #10
    mul     x2, x2, x5
    add     x2, x2, x4

    add     x3, x3, #1
    b       parse_num_loop

parse_num_done:
    mov     x0, x2
    mov     x1, x3
    ret

// Print a number
// x0 = number to print
print_number:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0                 // Number to print
    adrp    x20, output_buffer@PAGE
    add     x20, x20, output_buffer@PAGEOFF

    // Convert number to string (reverse)
    mov     x1, x20
    mov     x2, #0                  // Digit count

convert_loop:
    mov     x3, #10
    udiv    x4, x19, x3             // x4 = number / 10
    msub    x5, x4, x3, x19         // x5 = number % 10
    add     x5, x5, #'0'            // Convert to ASCII
    strb    w5, [x1], #1
    add     x2, x2, #1
    mov     x19, x4
    cbnz    x19, convert_loop

    // Reverse the string
    mov     x3, x20                 // Start
    sub     x4, x1, #1              // End
reverse_loop:
    cmp     x3, x4
    b.ge    reverse_done
    ldrb    w5, [x3]
    ldrb    w6, [x4]
    strb    w6, [x3]
    strb    w5, [x4]
    add     x3, x3, #1
    sub     x4, x4, #1
    b       reverse_loop

reverse_done:
    // Print the string
    mov     x16, #4                 // write syscall
    mov     x0, #1                  // stdout
    mov     x1, x20                 // Buffer
    // x2 already contains length
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
