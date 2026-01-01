// Day 18: Lavaduct Lagoon - ARM64 Assembly for macOS
// Uses Shoelace formula + Pick's theorem for polygon area calculation

.global _start
.align 4

// Constants
.equ BUFFER_SIZE, 65536
.equ MAX_VERTICES, 1024

.data
input_file:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"

.bss
.align 8
buffer:         .skip BUFFER_SIZE
num_buffer:     .skip 32
// Vertex storage for Part 1 and Part 2
vertices_r:     .skip MAX_VERTICES * 8    // Row coordinates (64-bit)
vertices_c:     .skip MAX_VERTICES * 8    // Column coordinates (64-bit)

.text

//=============================================================================
// _start - Entry point
//=============================================================================
_start:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Open input file
    adrp    x0, input_file@PAGE
    add     x0, x0, input_file@PAGEOFF
    mov     x1, #0              // O_RDONLY
    mov     x16, #5             // open syscall
    svc     #0x80

    cmp     x0, #0
    b.lt    exit_error
    mov     x19, x0             // Save file descriptor

    // Read file into buffer
    mov     x0, x19
    adrp    x1, buffer@PAGE
    add     x1, x1, buffer@PAGEOFF
    mov     x2, #BUFFER_SIZE
    mov     x16, #3             // read syscall
    svc     #0x80

    mov     x20, x0             // Save bytes read

    // Close file
    mov     x0, x19
    mov     x16, #6             // close syscall
    svc     #0x80

    // Null-terminate buffer
    adrp    x0, buffer@PAGE
    add     x0, x0, buffer@PAGEOFF
    strb    wzr, [x0, x20]

    // =========== Part 1 ===========
    adrp    x0, part1_msg@PAGE
    add     x0, x0, part1_msg@PAGEOFF
    bl      print_string

    adrp    x0, buffer@PAGE
    add     x0, x0, buffer@PAGEOFF
    bl      solve_part1
    mov     x0, x0
    bl      print_number

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // =========== Part 2 ===========
    adrp    x0, part2_msg@PAGE
    add     x0, x0, part2_msg@PAGEOFF
    bl      print_string

    adrp    x0, buffer@PAGE
    add     x0, x0, buffer@PAGEOFF
    bl      solve_part2
    mov     x0, x0
    bl      print_number

    adrp    x0, newline@PAGE
    add     x0, x0, newline@PAGEOFF
    bl      print_string

    // Exit successfully
    mov     x0, #0
    mov     x16, #1             // exit syscall
    svc     #0x80

exit_error:
    mov     x0, #1
    mov     x16, #1
    svc     #0x80

//=============================================================================
// solve_part1 - Parse Part 1 instructions and calculate area
// Input: x0 = buffer pointer
// Output: x0 = area
//=============================================================================
solve_part1:
    stp     x29, x30, [sp, #-96]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    mov     x19, x0             // Buffer pointer
    mov     x20, #0             // Vertex count
    mov     x21, #0             // Current row
    mov     x22, #0             // Current col
    mov     x23, #0             // Perimeter

    // Store initial vertex (0, 0)
    adrp    x24, vertices_r@PAGE
    add     x24, x24, vertices_r@PAGEOFF
    adrp    x25, vertices_c@PAGE
    add     x25, x25, vertices_c@PAGEOFF

    str     x21, [x24]
    str     x22, [x25]
    add     x20, x20, #1

parse_loop_p1:
    // Skip whitespace
    ldrb    w0, [x19]
    cbz     w0, parse_done_p1
    cmp     w0, #' '
    b.eq    skip_ws_p1
    cmp     w0, #'\n'
    b.eq    skip_ws_p1
    cmp     w0, #'\t'
    b.eq    skip_ws_p1
    cmp     w0, #'\r'
    b.eq    skip_ws_p1
    b       read_direction_p1

skip_ws_p1:
    add     x19, x19, #1
    b       parse_loop_p1

read_direction_p1:
    // Read direction character (R, D, L, U)
    ldrb    w26, [x19]
    add     x19, x19, #1

    // Skip space
    add     x19, x19, #1

    // Parse distance number
    mov     x27, #0             // Distance accumulator
parse_num_p1:
    ldrb    w0, [x19]
    cmp     w0, #'0'
    b.lt    num_done_p1
    cmp     w0, #'9'
    b.gt    num_done_p1
    sub     w0, w0, #'0'
    mov     x1, #10
    mul     x27, x27, x1
    add     x27, x27, x0
    add     x19, x19, #1
    b       parse_num_p1

num_done_p1:
    // Add distance to perimeter
    add     x23, x23, x27

    // Calculate new position based on direction
    // R: col += distance, D: row += distance
    // L: col -= distance, U: row -= distance
    cmp     w26, #'R'
    b.eq    move_right_p1
    cmp     w26, #'D'
    b.eq    move_down_p1
    cmp     w26, #'L'
    b.eq    move_left_p1
    cmp     w26, #'U'
    b.eq    move_up_p1
    b       skip_to_newline_p1

move_right_p1:
    add     x22, x22, x27
    b       store_vertex_p1
move_down_p1:
    add     x21, x21, x27
    b       store_vertex_p1
move_left_p1:
    sub     x22, x22, x27
    b       store_vertex_p1
move_up_p1:
    sub     x21, x21, x27
    b       store_vertex_p1

store_vertex_p1:
    // Store vertex
    lsl     x0, x20, #3
    str     x21, [x24, x0]
    str     x22, [x25, x0]
    add     x20, x20, #1

skip_to_newline_p1:
    // Skip to end of line (past the hex code)
    ldrb    w0, [x19]
    cbz     w0, parse_done_p1
    cmp     w0, #'\n'
    b.eq    next_line_p1
    add     x19, x19, #1
    b       skip_to_newline_p1

next_line_p1:
    add     x19, x19, #1
    b       parse_loop_p1

parse_done_p1:
    // Calculate area using Shoelace formula
    // x20 = number of vertices
    // x23 = perimeter
    mov     x0, x20
    mov     x1, x23
    bl      calculate_area

    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x25, x26, [sp, #64]
    ldp     x27, x28, [sp, #80]
    ldp     x29, x30, [sp], #96
    ret

//=============================================================================
// solve_part2 - Parse Part 2 instructions (decode hex) and calculate area
// Input: x0 = buffer pointer
// Output: x0 = area
//=============================================================================
solve_part2:
    stp     x29, x30, [sp, #-96]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    mov     x19, x0             // Buffer pointer
    mov     x20, #0             // Vertex count
    mov     x21, #0             // Current row
    mov     x22, #0             // Current col
    mov     x23, #0             // Perimeter

    // Store initial vertex (0, 0)
    adrp    x24, vertices_r@PAGE
    add     x24, x24, vertices_r@PAGEOFF
    adrp    x25, vertices_c@PAGE
    add     x25, x25, vertices_c@PAGEOFF

    str     x21, [x24]
    str     x22, [x25]
    add     x20, x20, #1

parse_loop_p2:
    // Find (#
    ldrb    w0, [x19]
    cbz     w0, parse_done_p2
    cmp     w0, #'('
    b.ne    next_char_p2

    // Check for #
    ldrb    w0, [x19, #1]
    cmp     w0, #'#'
    b.ne    next_char_p2

    // Found (#, parse 6 hex digits
    add     x19, x19, #2        // Skip (#

    // Parse first 5 hex digits as distance
    mov     x27, #0             // Distance accumulator
    mov     x28, #5             // Counter
parse_hex_dist:
    ldrb    w0, [x19]
    add     x19, x19, #1

    // Convert hex digit to value
    cmp     w0, #'9'
    b.le    hex_digit
    // It's a-f
    sub     w0, w0, #'a'
    add     w0, w0, #10
    b       add_hex
hex_digit:
    sub     w0, w0, #'0'
add_hex:
    lsl     x27, x27, #4
    add     x27, x27, x0
    subs    x28, x28, #1
    b.ne    parse_hex_dist

    // 6th hex digit is direction (0=R, 1=D, 2=L, 3=U)
    ldrb    w26, [x19]
    add     x19, x19, #1
    sub     w26, w26, #'0'

    // Add distance to perimeter
    add     x23, x23, x27

    // Move based on direction
    cmp     w26, #0
    b.eq    move_right_p2
    cmp     w26, #1
    b.eq    move_down_p2
    cmp     w26, #2
    b.eq    move_left_p2
    cmp     w26, #3
    b.eq    move_up_p2
    b       parse_loop_p2

move_right_p2:
    add     x22, x22, x27
    b       store_vertex_p2
move_down_p2:
    add     x21, x21, x27
    b       store_vertex_p2
move_left_p2:
    sub     x22, x22, x27
    b       store_vertex_p2
move_up_p2:
    sub     x21, x21, x27
    b       store_vertex_p2

store_vertex_p2:
    lsl     x0, x20, #3
    str     x21, [x24, x0]
    str     x22, [x25, x0]
    add     x20, x20, #1
    b       parse_loop_p2

next_char_p2:
    add     x19, x19, #1
    b       parse_loop_p2

parse_done_p2:
    // Calculate area
    mov     x0, x20
    mov     x1, x23
    bl      calculate_area

    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x25, x26, [sp, #64]
    ldp     x27, x28, [sp, #80]
    ldp     x29, x30, [sp], #96
    ret

//=============================================================================
// calculate_area - Calculate area using Shoelace + Pick's theorem
// Input: x0 = vertex count, x1 = perimeter
// Output: x0 = total area (shoelace_area + perimeter/2 + 1)
//=============================================================================
calculate_area:
    stp     x29, x30, [sp, #-80]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]

    mov     x19, x0             // Vertex count
    mov     x20, x1             // Perimeter

    adrp    x21, vertices_r@PAGE
    add     x21, x21, vertices_r@PAGEOFF
    adrp    x22, vertices_c@PAGE
    add     x22, x22, vertices_c@PAGEOFF

    // Shoelace formula
    // area = sum of (vertices[i].x * vertices[j].y - vertices[j].x * vertices[i].y)
    // where j = (i + 1) % n
    mov     x23, #0             // Area accumulator (signed)
    mov     x24, #0             // i = 0

shoelace_loop:
    cmp     x24, x19
    b.ge    shoelace_done

    // j = (i + 1) % n
    add     x25, x24, #1
    udiv    x26, x25, x19
    msub    x25, x26, x19, x25

    // Load vertices[i] and vertices[j]
    lsl     x0, x24, #3
    ldr     x1, [x21, x0]       // vertices[i].r
    ldr     x2, [x22, x0]       // vertices[i].c

    lsl     x0, x25, #3
    ldr     x3, [x21, x0]       // vertices[j].r
    ldr     x4, [x22, x0]       // vertices[j].c

    // area += vertices[i].r * vertices[j].c
    mul     x0, x1, x4
    add     x23, x23, x0

    // area -= vertices[j].r * vertices[i].c
    mul     x0, x3, x2
    sub     x23, x23, x0

    add     x24, x24, #1
    b       shoelace_loop

shoelace_done:
    // Take absolute value and divide by 2
    cmp     x23, #0
    b.ge    positive_area
    neg     x23, x23
positive_area:
    lsr     x23, x23, #1        // area / 2

    // Total = area + perimeter/2 + 1
    lsr     x0, x20, #1         // perimeter / 2
    add     x0, x0, x23
    add     x0, x0, #1

    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x25, x26, [sp, #64]
    ldp     x29, x30, [sp], #80
    ret

//=============================================================================
// print_string - Print null-terminated string
// Input: x0 = string pointer
//=============================================================================
print_string:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, x0

    // Find string length
    mov     x2, #0
strlen_loop:
    ldrb    w1, [x19, x2]
    cbz     w1, strlen_done
    add     x2, x2, #1
    b       strlen_loop
strlen_done:

    // Write string
    mov     x0, #1              // stdout
    mov     x1, x19
    mov     x16, #4             // write syscall
    svc     #0x80

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

//=============================================================================
// print_number - Print 64-bit unsigned number
// Input: x0 = number to print
//=============================================================================
print_number:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    str     x21, [sp, #32]

    mov     x19, x0             // Number to print

    // Handle zero specially
    cbnz    x19, not_zero
    adrp    x1, num_buffer@PAGE
    add     x1, x1, num_buffer@PAGEOFF
    mov     w0, #'0'
    strb    w0, [x1]
    mov     x0, #1              // stdout
    mov     x2, #1              // length
    mov     x16, #4             // write
    svc     #0x80
    b       print_num_done

not_zero:
    // Convert number to string (reverse order)
    adrp    x20, num_buffer@PAGE
    add     x20, x20, num_buffer@PAGEOFF
    add     x20, x20, #30       // End of buffer
    mov     x21, #0             // Digit count
    mov     x1, #10

convert_loop:
    cbz     x19, convert_done
    udiv    x2, x19, x1
    msub    x3, x2, x1, x19     // x3 = x19 % 10
    add     w3, w3, #'0'
    sub     x20, x20, #1
    strb    w3, [x20]
    add     x21, x21, #1
    mov     x19, x2
    b       convert_loop

convert_done:
    // Print the string
    mov     x0, #1              // stdout
    mov     x1, x20
    mov     x2, x21
    mov     x16, #4             // write
    svc     #0x80

print_num_done:
    ldr     x21, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret
