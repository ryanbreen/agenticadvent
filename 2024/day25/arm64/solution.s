// Day 25: Code Chronicle - ARM64 Assembly for macOS
// Parse lock and key schematics and count fitting pairs

.global _start
.align 4

// macOS syscall numbers
.equ SYS_EXIT, 0x2000001
.equ SYS_READ, 0x2000003
.equ SYS_WRITE, 0x2000004
.equ SYS_OPEN, 0x2000005

// File flags
.equ O_RDONLY, 0x0000

// Constants
.equ MAX_SCHEMATICS, 500
.equ SCHEMATIC_SIZE, 5      // 5 heights per schematic
.equ BUFFER_SIZE, 32768

.data
input_path:     .asciz "../input.txt"
part1_msg:      .asciz "Part 1: "
part2_msg:      .asciz "Part 2: Merry Christmas!\n"
newline:        .asciz "\n"

.bss
.align 4
file_buffer:    .space BUFFER_SIZE
locks:          .space MAX_SCHEMATICS * SCHEMATIC_SIZE * 4    // int arrays
keys:           .space MAX_SCHEMATICS * SCHEMATIC_SIZE * 4
lock_count:     .space 4
key_count:      .space 4

.text

_start:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Open input file
    movz    x16, #0x0005
    movk    x16, #0x0200, lsl #16   // SYS_OPEN = 0x2000005
    adrp    x0, input_path@PAGE
    add     x0, x0, input_path@PAGEOFF
    mov     x1, #O_RDONLY
    mov     x2, #0
    svc     #0

    cmp     x0, #0
    b.lt    exit_error
    mov     x19, x0                 // Save fd in x19

    // Read file into buffer
    movz    x16, #0x0003
    movk    x16, #0x0200, lsl #16   // SYS_READ = 0x2000003
    mov     x0, x19                 // fd
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #BUFFER_SIZE
    svc     #0

    cmp     x0, #0
    b.le    exit_error
    mov     x20, x0                 // Save file size in x20

    // Parse input into locks and keys
    adrp    x0, file_buffer@PAGE
    add     x0, x0, file_buffer@PAGEOFF
    mov     x1, x20                 // file size
    bl      parse_input

    // Part 1: Count fitting pairs
    bl      part1
    mov     x21, x0                 // Save result

    // Print "Part 1: "
    movz    x16, #0x0004
    movk    x16, #0x0200, lsl #16   // SYS_WRITE = 0x2000004
    mov     x0, #1
    adrp    x1, part1_msg@PAGE
    add     x1, x1, part1_msg@PAGEOFF
    mov     x2, #8
    svc     #0

    // Print result
    mov     x0, x21
    bl      print_number

    // Print newline
    movz    x16, #0x0004
    movk    x16, #0x0200, lsl #16   // SYS_WRITE = 0x2000004
    mov     x0, #1
    adrp    x1, newline@PAGE
    add     x1, x1, newline@PAGEOFF
    mov     x2, #1
    svc     #0

    // Print Part 2 message
    movz    x16, #0x0004
    movk    x16, #0x0200, lsl #16   // SYS_WRITE = 0x2000004
    mov     x0, #1
    adrp    x1, part2_msg@PAGE
    add     x1, x1, part2_msg@PAGEOFF
    mov     x2, #26
    svc     #0

    // Exit success
    movz    x16, #0x0001
    movk    x16, #0x0200, lsl #16   // SYS_EXIT = 0x2000001
    mov     x0, #0
    svc     #0

exit_error:
    movz    x16, #0x0001
    movk    x16, #0x0200, lsl #16   // SYS_EXIT = 0x2000001
    mov     x1, #1
    svc     #0

// parse_input: Parse file buffer into locks and keys
// x0: buffer pointer
// x1: buffer size
parse_input:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    stp     x25, x26, [sp, #-16]!
    stp     x27, x28, [sp, #-16]!
    mov     x29, sp

    mov     x19, x0                 // buffer start
    mov     x20, x1                 // buffer size
    add     x20, x19, x20           // buffer end
    mov     x21, #0                 // lock count
    mov     x22, #0                 // key count
    mov     x23, x19                // current position

parse_loop:
    // Check if we have space for a schematic (at least 42 bytes)
    add     x0, x23, #42
    cmp     x0, x20
    b.gt    parse_done

    // Check if current position starts a schematic
    ldrb    w0, [x23]
    cmp     w0, #'#'
    b.eq    parse_lock
    cmp     w0, #'.'
    b.eq    parse_key

    // Skip to next character
    add     x23, x23, #1
    b       parse_loop

parse_lock:
    // Verify this is a lock (top row all #)
    mov     x24, x23
    mov     x0, #0
verify_lock_top:
    cmp     x0, #5
    b.ge    is_lock
    ldrb    w1, [x24, x0]
    cmp     w1, #'#'
    b.ne    skip_schematic
    add     x0, x0, #1
    b       verify_lock_top

is_lock:
    // Calculate lock heights
    adrp    x25, locks@PAGE
    add     x25, x25, locks@PAGEOFF
    mov     x0, #SCHEMATIC_SIZE * 4
    mul     x0, x21, x0             // offset = lock_count * 5 * 4
    add     x25, x25, x0            // x25 = &locks[lock_count]

    // For each column (0-4)
    mov     x26, #0
lock_col_loop:
    cmp     x26, #5
    b.ge    lock_done

    // Count # from row 1 to 6 in this column
    mov     x0, #0                  // height counter
    mov     x1, #1                  // row counter
    mov     x24, x23                // reset position to start
    add     x24, x24, #6            // skip to row 1 (past first line + newline)

lock_row_loop:
    cmp     x1, #7
    b.ge    lock_col_done
    ldrb    w2, [x24, x26]
    cmp     w2, #'#'
    b.ne    lock_col_done
    add     x0, x0, #1
    add     x1, x1, #1
    add     x24, x24, #6            // next row (5 chars + newline)
    b       lock_row_loop

lock_col_done:
    str     w0, [x25, x26, lsl #2]  // locks[lock_count][col] = height
    add     x26, x26, #1
    b       lock_col_loop

lock_done:
    add     x21, x21, #1            // lock_count++
    add     x23, x23, #43           // skip 7 lines (42 bytes) + blank line (1 byte)
    b       parse_loop

parse_key:
    // Verify this is a key (top row all .)
    mov     x24, x23
    mov     x0, #0
verify_key_top:
    cmp     x0, #5
    b.ge    is_key
    ldrb    w1, [x24, x0]
    cmp     w1, #'.'
    b.ne    skip_schematic
    add     x0, x0, #1
    b       verify_key_top

is_key:
    // Calculate key heights
    adrp    x25, keys@PAGE
    add     x25, x25, keys@PAGEOFF
    mov     x0, #SCHEMATIC_SIZE * 4
    mul     x0, x22, x0             // offset = key_count * 5 * 4
    add     x25, x25, x0            // x25 = &keys[key_count]

    // For each column (0-4)
    mov     x26, #0
key_col_loop:
    cmp     x26, #5
    b.ge    key_done

    // Count # from row 5 down to 0 in this column
    mov     x0, #0                  // height counter
    mov     x1, #5                  // row counter (start from row 5)
    mov     x24, x23                // reset position to start
    add     x24, x24, #30           // skip to row 5 (5 lines * 6)

key_row_loop:
    cmp     x1, #0
    b.lt    key_col_done
    ldrb    w2, [x24, x26]
    cmp     w2, #'#'
    b.ne    key_col_done
    add     x0, x0, #1
    sub     x1, x1, #1
    sub     x24, x24, #6            // previous row
    b       key_row_loop

key_col_done:
    str     w0, [x25, x26, lsl #2]  // keys[key_count][col] = height
    add     x26, x26, #1
    b       key_col_loop

key_done:
    add     x22, x22, #1            // key_count++
    add     x23, x23, #43           // skip 7 lines (42 bytes) + blank line (1 byte)
    b       parse_loop

skip_schematic:
    add     x23, x23, #1
    b       parse_loop

parse_done:
    // Store counts
    adrp    x0, lock_count@PAGE
    add     x0, x0, lock_count@PAGEOFF
    str     w21, [x0]
    adrp    x0, key_count@PAGE
    add     x0, x0, key_count@PAGEOFF
    str     w22, [x0]

    mov     sp, x29
    ldp     x27, x28, [sp], #16
    ldp     x25, x26, [sp], #16
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// part1: Count fitting lock/key pairs
// Returns: count in x0
part1:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!
    mov     x29, sp

    mov     x19, #0                 // total count

    // Load lock_count and key_count
    adrp    x0, lock_count@PAGE
    add     x0, x0, lock_count@PAGEOFF
    ldr     w20, [x0]
    adrp    x0, key_count@PAGE
    add     x0, x0, key_count@PAGEOFF
    ldr     w21, [x0]

    mov     x22, #0                 // lock index
lock_loop:
    cmp     x22, x20
    b.ge    part1_done

    mov     x23, #0                 // key index
key_loop:
    cmp     x23, x21
    b.ge    next_lock

    // Check if this lock/key pair fits
    mov     x24, #0                 // column index

check_columns:
    cmp     x24, #5
    b.ge    pair_fits

    // Get lock[lock_idx][col]
    adrp    x0, locks@PAGE
    add     x0, x0, locks@PAGEOFF
    mov     x1, #SCHEMATIC_SIZE * 4
    mul     x1, x22, x1
    add     x0, x0, x1
    ldr     w1, [x0, x24, lsl #2]

    // Get key[key_idx][col]
    adrp    x0, keys@PAGE
    add     x0, x0, keys@PAGEOFF
    mov     x2, #SCHEMATIC_SIZE * 4
    mul     x2, x23, x2
    add     x0, x0, x2
    ldr     w2, [x0, x24, lsl #2]

    // Check if lock[col] + key[col] > 5
    add     w1, w1, w2
    cmp     w1, #5
    b.gt    pair_no_fit

    add     x24, x24, #1
    b       check_columns

pair_fits:
    add     x19, x19, #1            // count++

pair_no_fit:
    add     x23, x23, #1
    b       key_loop

next_lock:
    add     x22, x22, #1
    b       lock_loop

part1_done:
    mov     x0, x19

    mov     sp, x29
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// print_number: Print a number to stdout
// x0: number to print
print_number:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    mov     x29, sp

    mov     x19, x0
    sub     sp, sp, #32
    mov     x20, sp
    add     x21, x20, #31
    strb    wzr, [x21]
    sub     x21, x21, #1

    cmp     x19, #0
    b.ne    convert_loop
    mov     w0, #'0'
    strb    w0, [x21]
    sub     x21, x21, #1
    b       print_loop

convert_loop:
    cmp     x19, #0
    b.le    print_loop

    mov     x0, x19
    mov     x1, #10
    udiv    x2, x0, x1
    msub    x3, x2, x1, x0
    add     w3, w3, #'0'
    strb    w3, [x21]
    sub     x21, x21, #1
    mov     x19, x2
    b       convert_loop

print_loop:
    add     x21, x21, #1
    mov     x0, #1
    mov     x1, x21
    mov     x2, sp
    add     x2, x2, #31
    sub     x2, x2, x21
    movz    x16, #0x0004
    movk    x16, #0x0200, lsl #16   // SYS_WRITE = 0x2000004
    svc     #0

    add     sp, sp, #32
    mov     sp, x29
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
