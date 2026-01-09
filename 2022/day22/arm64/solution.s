// Day 22: Monkey Map - ARM64 Assembly Solution for macOS
// Navigate a 2D map with wrapping (Part 1: flat, Part 2: cube)

.global _main
.align 4

// Constants
.equ BUFFER_SIZE, 65536
.equ MAX_WIDTH, 200
.equ MAX_HEIGHT, 250
.equ MAX_INSTRUCTIONS, 8192
.equ FACE_SIZE, 50

// Read-only data section
.section __TEXT,__cstring
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: "
newline:        .asciz "\n"
input_path:     .asciz "../input.txt"

// Constant data in __DATA,__const
.section __DATA,__const
.align 8
dr_array:       .quad 0, 1, 0, -1
dc_array:       .quad 1, 0, -1, 0

// Writable data in __DATA,__data
.section __DATA,__data
.align 8
grid_width:     .quad 0
grid_height:    .quad 0
num_instructions: .quad 0

// Zero-initialized writable data
.zerofill __DATA,__bss,buffer,BUFFER_SIZE,3
.zerofill __DATA,__bss,grid,MAX_WIDTH*MAX_HEIGHT,3
.zerofill __DATA,__bss,instructions,MAX_INSTRUCTIONS*8,3
.zerofill __DATA,__bss,num_buf,32,3

.text

_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Open input file
    adrp x0, input_path@PAGE
    add x0, x0, input_path@PAGEOFF
    mov x1, #0                  // O_RDONLY
    mov x2, #0
    mov x16, #5                 // open syscall
    svc #0x80

    cmp x0, #0
    b.le exit_error
    mov x19, x0

    // Read file
    mov x0, x19
    adrp x1, buffer@PAGE
    add x1, x1, buffer@PAGEOFF
    mov x2, #BUFFER_SIZE
    mov x16, #3                 // read syscall
    svc #0x80

    // Close file
    mov x0, x19
    mov x16, #6                 // close syscall
    svc #0x80

    // Parse input
    bl parse_input

    // Part 1
    bl solve_part1
    mov x19, x0

    adrp x0, part1_msg@PAGE
    add x0, x0, part1_msg@PAGEOFF
    bl print_string
    mov x0, x19
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    // Part 2
    bl solve_part2
    mov x19, x0

    adrp x0, part2_msg@PAGE
    add x0, x0, part2_msg@PAGEOFF
    bl print_string
    mov x0, x19
    bl print_number
    adrp x0, newline@PAGE
    add x0, x0, newline@PAGEOFF
    bl print_string

    mov x0, #0
    ldp x29, x30, [sp], #16
    ret

exit_error:
    mov x0, #1
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// parse_input: Parse the grid and instructions
// ============================================================
parse_input:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    adrp x19, buffer@PAGE
    add x19, x19, buffer@PAGEOFF
    adrp x20, grid@PAGE
    add x20, x20, grid@PAGEOFF

    // Fill grid with spaces
    mov x0, x20
    mov x1, #MAX_WIDTH
    mov x2, #MAX_HEIGHT
    mul x1, x1, x2
fill_grid:
    cbz x1, fill_done
    mov w2, #' '
    strb w2, [x0], #1
    sub x1, x1, #1
    b fill_grid
fill_done:

    mov x21, #0                 // row count
    mov x22, #0                 // max width
    mov x23, #0                 // current column
    mov x24, x19                // current buffer position

parse_grid_loop:
    ldrb w0, [x24]
    cmp w0, #10
    b.ne not_newline

    ldrb w1, [x24, #1]
    cmp w1, #10
    b.eq end_grid_section

    cmp x23, x22
    csel x22, x23, x22, gt
    add x21, x21, #1
    mov x23, #0
    add x24, x24, #1
    b parse_grid_loop

not_newline:
    mov x25, x21
    mov x26, #MAX_WIDTH
    mul x25, x25, x26
    add x25, x25, x23
    strb w0, [x20, x25]
    add x23, x23, #1
    add x24, x24, #1
    b parse_grid_loop

end_grid_section:
    cmp x23, #0
    b.eq skip_last_row_check
    cmp x23, x22
    csel x22, x23, x22, gt
    add x21, x21, #1
skip_last_row_check:

    adrp x0, grid_width@PAGE
    add x0, x0, grid_width@PAGEOFF
    str x22, [x0]

    adrp x0, grid_height@PAGE
    add x0, x0, grid_height@PAGEOFF
    str x21, [x0]

    add x24, x24, #2

    adrp x25, instructions@PAGE
    add x25, x25, instructions@PAGEOFF
    mov x26, #0

parse_instr_loop:
    ldrb w0, [x24]
    cbz w0, end_parse
    cmp w0, #10
    b.eq end_parse
    cmp w0, #13
    b.eq end_parse

    cmp w0, #'0'
    b.lt check_turn
    cmp w0, #'9'
    b.gt check_turn

    mov x27, #0
parse_num:
    ldrb w0, [x24]
    cmp w0, #'0'
    b.lt store_num
    cmp w0, #'9'
    b.gt store_num
    mov x1, #10
    mul x27, x27, x1
    sub w0, w0, #'0'
    add x27, x27, x0
    add x24, x24, #1
    b parse_num

store_num:
    str x27, [x25, x26, lsl #3]
    add x26, x26, #1
    b parse_instr_loop

check_turn:
    cmp w0, #'R'
    b.eq turn_right
    cmp w0, #'L'
    b.eq turn_left
    add x24, x24, #1
    b parse_instr_loop

turn_right:
    mov x27, #1
    neg x27, x27
    str x27, [x25, x26, lsl #3]
    add x26, x26, #1
    add x24, x24, #1
    b parse_instr_loop

turn_left:
    mov x27, #2
    neg x27, x27
    str x27, [x25, x26, lsl #3]
    add x26, x26, #1
    add x24, x24, #1
    b parse_instr_loop

end_parse:
    adrp x0, num_instructions@PAGE
    add x0, x0, num_instructions@PAGEOFF
    str x26, [x0]

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// get_grid_char: Get character at (row, col)
// x0 = row, x1 = col
// Returns char in w0
// ============================================================
get_grid_char:
    cmp x0, #0
    b.lt ggc_return_space

    adrp x2, grid_height@PAGE
    add x2, x2, grid_height@PAGEOFF
    ldr x2, [x2]
    cmp x0, x2
    b.ge ggc_return_space

    cmp x1, #0
    b.lt ggc_return_space

    adrp x2, grid_width@PAGE
    add x2, x2, grid_width@PAGEOFF
    ldr x2, [x2]
    cmp x1, x2
    b.ge ggc_return_space

    mov x2, #MAX_WIDTH
    mul x2, x0, x2
    add x2, x2, x1

    adrp x9, grid@PAGE
    add x9, x9, grid@PAGEOFF
    ldrb w0, [x9, x2]
    ret

ggc_return_space:
    mov w0, #' '
    ret

// ============================================================
// find_start: Find starting position
// Returns: x0 = col
// ============================================================
find_start:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, #0

fs_loop:
    mov x0, #0
    mov x1, x19
    bl get_grid_char
    cmp w0, #'.'
    b.eq fs_found
    add x19, x19, #1
    b fs_loop

fs_found:
    mov x0, x19
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// solve_part1: Part 1 with flat wrapping
// ============================================================
solve_part1:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    bl find_start
    mov x19, #0                 // row
    mov x20, x0                 // col
    mov x21, #0                 // facing

    adrp x22, grid_height@PAGE
    add x22, x22, grid_height@PAGEOFF
    ldr x22, [x22]

    adrp x23, grid_width@PAGE
    add x23, x23, grid_width@PAGEOFF
    ldr x23, [x23]

    adrp x24, num_instructions@PAGE
    add x24, x24, num_instructions@PAGEOFF
    ldr x24, [x24]

    adrp x25, instructions@PAGE
    add x25, x25, instructions@PAGEOFF

    mov x26, #0

sp1_instr_loop:
    cmp x26, x24
    b.ge sp1_done

    ldr x27, [x25, x26, lsl #3]
    add x26, x26, #1

    cmp x27, #0
    b.lt sp1_turn

    mov x28, x27

sp1_move_loop:
    cbz x28, sp1_instr_loop

    adrp x0, dr_array@PAGE
    add x0, x0, dr_array@PAGEOFF
    ldr x10, [x0, x21, lsl #3]

    adrp x0, dc_array@PAGE
    add x0, x0, dc_array@PAGEOFF
    ldr x11, [x0, x21, lsl #3]

    add x12, x19, x10
    add x13, x20, x11

    cmp x21, #0
    b.ne sp1_cf1

    cmp x13, x23
    b.ge sp1_wr
    mov x0, x12
    mov x1, x13
    bl get_grid_char
    cmp w0, #' '
    b.eq sp1_wr
    b sp1_wall

sp1_wr:
    mov x13, #0
sp1_wr_l:
    mov x0, x12
    mov x1, x13
    bl get_grid_char
    cmp w0, #' '
    b.ne sp1_wall
    add x13, x13, #1
    b sp1_wr_l

sp1_cf1:
    cmp x21, #1
    b.ne sp1_cf2

    cmp x12, x22
    b.ge sp1_wd
    mov x0, x12
    mov x1, x13
    bl get_grid_char
    cmp w0, #' '
    b.eq sp1_wd
    b sp1_wall

sp1_wd:
    mov x12, #0
sp1_wd_l:
    mov x0, x12
    mov x1, x13
    bl get_grid_char
    cmp w0, #' '
    b.ne sp1_wall
    add x12, x12, #1
    b sp1_wd_l

sp1_cf2:
    cmp x21, #2
    b.ne sp1_cf3

    cmp x13, #0
    b.lt sp1_wl
    mov x0, x12
    mov x1, x13
    bl get_grid_char
    cmp w0, #' '
    b.eq sp1_wl
    b sp1_wall

sp1_wl:
    sub x13, x23, #1
sp1_wl_l:
    mov x0, x12
    mov x1, x13
    bl get_grid_char
    cmp w0, #' '
    b.ne sp1_wall
    sub x13, x13, #1
    b sp1_wl_l

sp1_cf3:
    cmp x12, #0
    b.lt sp1_wu
    mov x0, x12
    mov x1, x13
    bl get_grid_char
    cmp w0, #' '
    b.eq sp1_wu
    b sp1_wall

sp1_wu:
    sub x12, x22, #1
sp1_wu_l:
    mov x0, x12
    mov x1, x13
    bl get_grid_char
    cmp w0, #' '
    b.ne sp1_wall
    sub x12, x12, #1
    b sp1_wu_l

sp1_wall:
    mov x0, x12
    mov x1, x13
    bl get_grid_char
    cmp w0, #'#'
    b.eq sp1_instr_loop

    mov x19, x12
    mov x20, x13
    sub x28, x28, #1
    b sp1_move_loop

sp1_turn:
    mov x0, #1
    neg x0, x0
    cmp x27, x0
    b.ne sp1_tl

    add x21, x21, #1
    and x21, x21, #3
    b sp1_instr_loop

sp1_tl:
    add x21, x21, #3
    and x21, x21, #3
    b sp1_instr_loop

sp1_done:
    add x0, x19, #1
    mov x1, #1000
    mul x0, x0, x1
    add x1, x20, #1
    lsl x1, x1, #2
    add x0, x0, x1
    add x0, x0, x21

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// get_cube_face: Determine face and local coords
// x0 = row, x1 = col
// Returns: x0 = face, x1 = local_r, x2 = local_c
// ============================================================
get_cube_face:
    mov x3, #FACE_SIZE
    udiv x4, x0, x3
    udiv x5, x1, x3
    msub x6, x4, x3, x0
    msub x7, x5, x3, x1

    cmp x4, #0
    b.ne gcf_r1
    cmp x5, #1
    b.ne gcf_02
    mov x0, #1
    b gcf_found
gcf_02:
    cmp x5, #2
    b.ne gcf_inv
    mov x0, #2
    b gcf_found

gcf_r1:
    cmp x4, #1
    b.ne gcf_r2
    cmp x5, #1
    b.ne gcf_inv
    mov x0, #3
    b gcf_found

gcf_r2:
    cmp x4, #2
    b.ne gcf_r3
    cmp x5, #0
    b.ne gcf_21
    mov x0, #4
    b gcf_found
gcf_21:
    cmp x5, #1
    b.ne gcf_inv
    mov x0, #5
    b gcf_found

gcf_r3:
    cmp x4, #3
    b.ne gcf_inv
    cmp x5, #0
    b.ne gcf_inv
    mov x0, #6
    b gcf_found

gcf_inv:
    mov x0, #0
    sub x0, x0, #1

gcf_found:
    mov x1, x6
    mov x2, x7
    ret

// ============================================================
// wrap_cube: Handle cube wrapping
// x0 = row, x1 = col, x2 = facing
// Returns: x0 = new_row, x1 = new_col, x2 = new_facing
// ============================================================
wrap_cube:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!

    mov x19, x0
    mov x20, x1
    mov x21, x2

    bl get_cube_face
    mov x22, x0
    mov x23, x1
    mov x24, x2

    cmp x22, #1
    b.ne wc_f2

    cmp x21, #3
    b.ne wc_f1l
    mov x0, #FACE_SIZE
    mov x1, #3
    mul x0, x0, x1
    add x0, x0, x24
    mov x1, #0
    mov x2, #0
    b wc_done

wc_f1l:
    cmp x21, #2
    b.ne wc_unch
    mov x0, #FACE_SIZE
    mov x1, #3
    mul x0, x0, x1
    sub x0, x0, #1
    sub x0, x0, x23
    mov x1, #0
    mov x2, #0
    b wc_done

wc_f2:
    cmp x22, #2
    b.ne wc_f3

    cmp x21, #0
    b.ne wc_f2d
    mov x0, #FACE_SIZE
    mov x1, #3
    mul x0, x0, x1
    sub x0, x0, #1
    sub x0, x0, x23
    mov x1, #FACE_SIZE
    lsl x1, x1, #1
    sub x1, x1, #1
    mov x2, #2
    b wc_done

wc_f2d:
    cmp x21, #1
    b.ne wc_f2u
    mov x0, #FACE_SIZE
    add x0, x0, x24
    mov x1, #FACE_SIZE
    lsl x1, x1, #1
    sub x1, x1, #1
    mov x2, #2
    b wc_done

wc_f2u:
    cmp x21, #3
    b.ne wc_unch
    mov x0, #FACE_SIZE
    lsl x0, x0, #2
    sub x0, x0, #1
    mov x1, x24
    mov x2, #3
    b wc_done

wc_f3:
    cmp x22, #3
    b.ne wc_f4

    cmp x21, #0
    b.ne wc_f3l
    mov x0, #FACE_SIZE
    sub x0, x0, #1
    mov x1, #FACE_SIZE
    lsl x1, x1, #1
    add x1, x1, x23
    mov x2, #3
    b wc_done

wc_f3l:
    cmp x21, #2
    b.ne wc_unch
    mov x0, #FACE_SIZE
    lsl x0, x0, #1
    mov x1, x23
    mov x2, #1
    b wc_done

wc_f4:
    cmp x22, #4
    b.ne wc_f5

    cmp x21, #3
    b.ne wc_f4l
    mov x0, #FACE_SIZE
    add x0, x0, x24
    mov x1, #FACE_SIZE
    mov x2, #0
    b wc_done

wc_f4l:
    cmp x21, #2
    b.ne wc_unch
    mov x0, #FACE_SIZE
    sub x0, x0, #1
    sub x0, x0, x23
    mov x1, #FACE_SIZE
    mov x2, #0
    b wc_done

wc_f5:
    cmp x22, #5
    b.ne wc_f6

    cmp x21, #0
    b.ne wc_f5d
    mov x0, #FACE_SIZE
    sub x0, x0, #1
    sub x0, x0, x23
    mov x1, #FACE_SIZE
    mov x3, #3
    mul x1, x1, x3
    sub x1, x1, #1
    mov x2, #2
    b wc_done

wc_f5d:
    cmp x21, #1
    b.ne wc_unch
    mov x0, #FACE_SIZE
    mov x3, #3
    mul x0, x0, x3
    add x0, x0, x24
    mov x1, #FACE_SIZE
    sub x1, x1, #1
    mov x2, #2
    b wc_done

wc_f6:
    cmp x22, #6
    b.ne wc_unch

    cmp x21, #0
    b.ne wc_f6d
    mov x0, #FACE_SIZE
    mov x3, #3
    mul x0, x0, x3
    sub x0, x0, #1
    mov x1, #FACE_SIZE
    add x1, x1, x23
    mov x2, #3
    b wc_done

wc_f6d:
    cmp x21, #1
    b.ne wc_f6l
    mov x0, #0
    mov x1, #FACE_SIZE
    lsl x1, x1, #1
    add x1, x1, x24
    mov x2, #1
    b wc_done

wc_f6l:
    cmp x21, #2
    b.ne wc_unch
    mov x0, #0
    mov x1, #FACE_SIZE
    add x1, x1, x23
    mov x2, #1
    b wc_done

wc_unch:
    mov x0, x19
    mov x1, x20
    mov x2, x21

wc_done:
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// solve_part2: Part 2 with cube wrapping
// ============================================================
solve_part2:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    stp x25, x26, [sp, #-16]!
    stp x27, x28, [sp, #-16]!

    bl find_start
    mov x19, #0
    mov x20, x0
    mov x21, #0

    adrp x22, grid_height@PAGE
    add x22, x22, grid_height@PAGEOFF
    ldr x22, [x22]

    adrp x23, grid_width@PAGE
    add x23, x23, grid_width@PAGEOFF
    ldr x23, [x23]

    adrp x24, num_instructions@PAGE
    add x24, x24, num_instructions@PAGEOFF
    ldr x24, [x24]

    adrp x25, instructions@PAGE
    add x25, x25, instructions@PAGEOFF

    mov x26, #0

sp2_instr_loop:
    cmp x26, x24
    b.ge sp2_done

    ldr x27, [x25, x26, lsl #3]
    add x26, x26, #1

    cmp x27, #0
    b.lt sp2_turn

    mov x28, x27

sp2_move_loop:
    cbz x28, sp2_instr_loop

    adrp x0, dr_array@PAGE
    add x0, x0, dr_array@PAGEOFF
    ldr x10, [x0, x21, lsl #3]

    adrp x0, dc_array@PAGE
    add x0, x0, dc_array@PAGEOFF
    ldr x11, [x0, x21, lsl #3]

    add x12, x19, x10
    add x13, x20, x11
    mov x14, x21

    mov x15, #0

    cmp x12, #0
    b.lt sp2_snw
    cmp x12, x22
    b.ge sp2_snw
    cmp x13, #0
    b.lt sp2_snw
    cmp x13, x23
    b.ge sp2_snw

    mov x0, x12
    mov x1, x13
    bl get_grid_char
    cmp w0, #' '
    b.eq sp2_snw
    b sp2_swc

sp2_snw:
    mov x15, #1

sp2_swc:
    cbz x15, sp2_wall

    mov x0, x19
    mov x1, x20
    mov x2, x21
    bl wrap_cube
    mov x12, x0
    mov x13, x1
    mov x14, x2

sp2_wall:
    mov x0, x12
    mov x1, x13
    bl get_grid_char
    cmp w0, #'#'
    b.eq sp2_instr_loop

    mov x19, x12
    mov x20, x13
    mov x21, x14
    sub x28, x28, #1
    b sp2_move_loop

sp2_turn:
    mov x0, #1
    neg x0, x0
    cmp x27, x0
    b.ne sp2_tl

    add x21, x21, #1
    and x21, x21, #3
    b sp2_instr_loop

sp2_tl:
    add x21, x21, #3
    and x21, x21, #3
    b sp2_instr_loop

sp2_done:
    add x0, x19, #1
    mov x1, #1000
    mul x0, x0, x1
    add x1, x20, #1
    lsl x1, x1, #2
    add x0, x0, x1
    add x0, x0, x21

    ldp x27, x28, [sp], #16
    ldp x25, x26, [sp], #16
    ldp x23, x24, [sp], #16
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// print_string: Print null-terminated string
// ============================================================
print_string:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    mov x19, x0
    mov x20, #0
ps_len:
    ldrb w1, [x19, x20]
    cbz w1, ps_done
    add x20, x20, #1
    b ps_len

ps_done:
    mov x0, #1
    mov x1, x19
    mov x2, x20
    mov x16, #4
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret

// ============================================================
// print_number: Print integer
// ============================================================
print_number:
    stp x29, x30, [sp, #-16]!
    stp x19, x20, [sp, #-16]!

    adrp x19, num_buf@PAGE
    add x19, x19, num_buf@PAGEOFF

    mov x1, x0
    add x0, x19, #20
    mov x2, #0

    cbz x1, pn_zero

pn_loop:
    cbz x1, pn_print
    mov x3, #10
    udiv x4, x1, x3
    msub x5, x4, x3, x1
    add w5, w5, #'0'
    sub x0, x0, #1
    strb w5, [x0]
    add x2, x2, #1
    mov x1, x4
    b pn_loop

pn_zero:
    mov w3, #'0'
    sub x0, x0, #1
    strb w3, [x0]
    mov x2, #1

pn_print:
    mov x1, x0
    mov x0, #1
    mov x16, #4
    svc #0x80

    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret
