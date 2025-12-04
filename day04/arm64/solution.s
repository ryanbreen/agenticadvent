// Day 4: Printing Department - ARM64 Assembly for macOS
// Part 1: Count rolls with fewer than 4 adjacent rolls
// Part 2: Count total rolls removed by iterative removal

.global _main
.align 4

// Constants
.equ O_RDONLY, 0
.equ BUFFER_SIZE, 65536
.equ MAX_ROWS, 200
.equ MAX_COLS, 200

.section __DATA,__data
.align 3
input_path: .asciz "../input.txt"
part1_fmt: .asciz "Part 1: %lld\n"
part2_fmt: .asciz "Part 2: %lld\n"

// Direction offsets for 8 neighbors (row, col)
directions:
    .byte -1, -1    // NW
    .byte -1,  0    // N
    .byte -1,  1    // NE
    .byte  0, -1    // W
    .byte  0,  1    // E
    .byte  1, -1    // SW
    .byte  1,  0    // S
    .byte  1,  1    // SE

.section __DATA,__bss
.align 3
buffer: .space BUFFER_SIZE
grid: .space MAX_ROWS * MAX_COLS
grid2: .space MAX_ROWS * MAX_COLS

.section __TEXT,__text

// Main function
_main:
    // Prologue
    sub sp, sp, #112
    stp x29, x30, [sp, #96]
    add x29, sp, #96
    stp x19, x20, [sp, #80]
    stp x21, x22, [sp, #64]
    stp x23, x24, [sp, #48]
    stp x25, x26, [sp, #32]
    stp x27, x28, [sp, #16]

    // Open file
    adrp x0, input_path@PAGE
    add x0, x0, input_path@PAGEOFF
    mov w1, #O_RDONLY
    bl _open
    sxtw x19, w0            // x19 = fd

    cmp x19, #0
    b.lt exit_error

    // Read file into buffer
    adrp x20, buffer@PAGE
    add x20, x20, buffer@PAGEOFF
    mov x0, x19
    mov x1, x20
    mov x2, #BUFFER_SIZE
    bl _read
    mov x21, x0             // x21 = bytes read

    // Close file
    mov x0, x19
    bl _close

    cmp x21, #0
    b.le exit_error

    // Null-terminate buffer
    strb wzr, [x20, x21]

    // Parse grid
    // x20 = buffer ptr
    // x22 = grid ptr
    // x23 = rows
    // x24 = cols (width of first row)
    // x25 = current column
    adrp x22, grid@PAGE
    add x22, x22, grid@PAGEOFF
    mov x23, #0             // row count
    mov x24, #0             // cols (not set yet)
    mov x25, #0             // current col in row
    mov x26, x22            // grid write ptr

parse_loop:
    ldrb w0, [x20], #1
    cbz w0, parse_done

    cmp w0, #'\n'
    b.eq parse_newline
    cmp w0, #'\r'
    b.eq parse_loop         // skip CR

    // Store character
    strb w0, [x26], #1
    add x25, x25, #1
    b parse_loop

parse_newline:
    // If this is the first row, set cols
    cbz x24, set_cols
    b next_row

set_cols:
    mov x24, x25

next_row:
    // Pad row to cols length if needed
    cmp x25, x24
    b.ge row_complete
    mov w0, #'.'
    strb w0, [x26], #1
    add x25, x25, #1
    b next_row

row_complete:
    add x23, x23, #1
    mov x25, #0
    b parse_loop

parse_done:
    // Handle last row if no newline at end
    cbz x25, grid_ready
    // Pad last row
    cmp x25, x24
    b.ge last_row_done
    mov w0, #'.'
    strb w0, [x26], #1
    add x25, x25, #1
    b parse_done

last_row_done:
    add x23, x23, #1

grid_ready:
    // Now we have grid in memory, x23 = rows, x24 = cols

    // ===== Part 1 =====
    // Count rolls with < 4 adjacent rolls
    mov x25, #0             // part1 answer

    mov x19, #0             // row counter
part1_row_loop:
    cmp x19, x23
    b.ge part1_done

    mov x20, #0             // col counter
part1_col_loop:
    cmp x20, x24
    b.ge part1_next_row

    // Get grid[r][c]
    mul x0, x19, x24
    add x0, x0, x20
    ldrb w1, [x22, x0]

    cmp w1, #'@'
    b.ne part1_next_col

    // Count adjacent rolls
    mov x26, #0             // adjacent count
    adrp x27, directions@PAGE
    add x27, x27, directions@PAGEOFF
    mov x28, #0             // direction index

part1_check_dirs:
    cmp x28, #8
    b.ge part1_check_accessible

    // Load direction offset
    lsl x0, x28, #1
    ldrsb x1, [x27, x0]     // dr
    add x0, x0, #1
    ldrsb x2, [x27, x0]     // dc

    // Calculate neighbor position
    add x3, x19, x1         // nr
    add x4, x20, x2         // nc

    // Check bounds
    cmp x3, #0
    b.lt part1_next_dir
    cmp x3, x23
    b.ge part1_next_dir
    cmp x4, #0
    b.lt part1_next_dir
    cmp x4, x24
    b.ge part1_next_dir

    // Get neighbor value
    mul x5, x3, x24
    add x5, x5, x4
    ldrb w6, [x22, x5]

    cmp w6, #'@'
    b.ne part1_next_dir
    add x26, x26, #1

part1_next_dir:
    add x28, x28, #1
    b part1_check_dirs

part1_check_accessible:
    cmp x26, #4
    b.ge part1_next_col
    add x25, x25, #1

part1_next_col:
    add x20, x20, #1
    b part1_col_loop

part1_next_row:
    add x19, x19, #1
    b part1_row_loop

part1_done:
    // Print Part 1
    str x25, [sp]
    adrp x0, part1_fmt@PAGE
    add x0, x0, part1_fmt@PAGEOFF
    bl _printf

    // ===== Part 2 =====
    // Copy grid to grid2 for mutation
    adrp x26, grid2@PAGE
    add x26, x26, grid2@PAGEOFF
    mul x0, x23, x24        // total cells
    mov x1, #0
copy_grid:
    cmp x1, x0
    b.ge copy_done
    ldrb w2, [x22, x1]
    strb w2, [x26, x1]
    add x1, x1, #1
    b copy_grid

copy_done:
    mov x25, #0             // total removed

part2_outer_loop:
    // Find all removable rolls
    // Use stack to store removable positions
    mov x19, sp
    mov x27, #0             // count of removable

    mov x20, #0             // row
part2_find_row:
    cmp x20, x23
    b.ge part2_remove_all

    mov x21, #0             // col
part2_find_col:
    cmp x21, x24
    b.ge part2_find_next_row

    // Get grid2[r][c]
    mul x0, x20, x24
    add x0, x0, x21
    ldrb w1, [x26, x0]

    cmp w1, #'@'
    b.ne part2_find_next_col

    // Count adjacent rolls
    mov x28, #0             // adjacent count
    adrp x8, directions@PAGE
    add x8, x8, directions@PAGEOFF
    mov x9, #0              // direction index

part2_check_dirs:
    cmp x9, #8
    b.ge part2_check_removable

    // Load direction offset
    lsl x0, x9, #1
    ldrsb x1, [x8, x0]      // dr
    add x0, x0, #1
    ldrsb x2, [x8, x0]      // dc

    // Calculate neighbor position
    add x3, x20, x1         // nr
    add x4, x21, x2         // nc

    // Check bounds
    cmp x3, #0
    b.lt part2_next_dir
    cmp x3, x23
    b.ge part2_next_dir
    cmp x4, #0
    b.lt part2_next_dir
    cmp x4, x24
    b.ge part2_next_dir

    // Get neighbor value
    mul x5, x3, x24
    add x5, x5, x4
    ldrb w6, [x26, x5]

    cmp w6, #'@'
    b.ne part2_next_dir
    add x28, x28, #1

part2_next_dir:
    add x9, x9, #1
    b part2_check_dirs

part2_check_removable:
    cmp x28, #4
    b.ge part2_find_next_col

    // Store position (we'll use the grid itself to mark)
    mul x0, x20, x24
    add x0, x0, x21
    mov w1, #'X'            // Mark as to-be-removed
    strb w1, [x26, x0]
    add x27, x27, #1

part2_find_next_col:
    add x21, x21, #1
    b part2_find_col

part2_find_next_row:
    add x20, x20, #1
    b part2_find_row

part2_remove_all:
    // If nothing marked, we're done
    cbz x27, part2_done

    // Remove all marked cells
    mul x0, x23, x24
    mov x1, #0
remove_marked:
    cmp x1, x0
    b.ge removed_batch
    ldrb w2, [x26, x1]
    cmp w2, #'X'
    b.ne skip_remove
    mov w2, #'.'
    strb w2, [x26, x1]
skip_remove:
    add x1, x1, #1
    b remove_marked

removed_batch:
    add x25, x25, x27
    b part2_outer_loop

part2_done:
    // Print Part 2
    str x25, [sp]
    adrp x0, part2_fmt@PAGE
    add x0, x0, part2_fmt@PAGEOFF
    bl _printf

    mov x0, #0
    b exit_program

exit_error:
    mov x0, #1

exit_program:
    // Epilogue
    ldp x27, x28, [sp, #16]
    ldp x25, x26, [sp, #32]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #64]
    ldp x19, x20, [sp, #80]
    ldp x29, x30, [sp, #96]
    add sp, sp, #112
    bl _exit
