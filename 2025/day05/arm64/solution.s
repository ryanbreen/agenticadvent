// Day 5: Cafeteria - ARM64 Assembly for macOS
// Part 1: Count available IDs that fall within any range
// Part 2: Count total unique IDs covered by all ranges (merge overlapping)

.global _main
.align 4

// Constants
.equ O_RDONLY, 0
.equ BUFFER_SIZE, 131072
.equ MAX_RANGES, 256
.equ MAX_IDS, 1024

.section __DATA,__data
.align 3
input_path: .asciz "../input.txt"
part1_fmt: .asciz "Part 1: %lld\n"
part2_fmt: .asciz "Part 2: %lld\n"

.section __DATA,__bss
.align 3
buffer: .space BUFFER_SIZE
ranges: .space MAX_RANGES * 16      // Array of (start, end) pairs - 8 bytes each
ingredient_ids: .space MAX_IDS * 8   // Array of ingredient IDs

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
    mov w19, w0             // x19 = fd

    // Check for error
    cmp w19, #0
    b.lt exit_error

    // Get buffer address into x20
    adrp x20, buffer@PAGE
    add x20, x20, buffer@PAGEOFF

    // Read file into buffer
    sxtw x0, w19            // fd
    mov x1, x20             // buffer
    mov x2, #BUFFER_SIZE
    bl _read
    mov x21, x0             // x21 = bytes read

    // Close file
    sxtw x0, w19
    bl _close

    // Check if read failed
    cmp x21, #0
    b.le exit_error

    // Null-terminate the buffer
    strb wzr, [x20, x21]

    // ===== Parse Input =====
    adrp x22, ranges@PAGE
    add x22, x22, ranges@PAGEOFF
    mov x23, #0             // range count

    adrp x24, ingredient_ids@PAGE
    add x24, x24, ingredient_ids@PAGEOFF
    mov x25, #0             // ingredient count

    mov x1, x20             // current buffer position
    mov x10, #0             // past_blank flag

parse_loop:
    // Skip whitespace
skip_ws:
    ldrb w2, [x1]
    cbz w2, parse_done
    cmp w2, #' '
    b.eq skip_ws_inc
    cmp w2, #'\t'
    b.eq skip_ws_inc
    cmp w2, #'\r'
    b.eq skip_ws_inc
    cmp w2, #'\n'
    b.ne skip_ws_done

    // Found newline - check if we're starting a blank line
    add x3, x1, #1
    ldrb w4, [x3]
    cmp w4, #'\n'
    b.eq found_blank
    cmp w4, #0
    b.eq parse_done

skip_ws_inc:
    add x1, x1, #1
    b skip_ws

found_blank:
    mov x10, #1             // set past_blank flag
    add x1, x1, #2          // skip both newlines
    b skip_ws

skip_ws_done:
    // Check if end of input
    cbz w2, parse_done

    // Parse number inline
    mov x0, #0              // result
parse_num:
    ldrb w2, [x1]
    sub w2, w2, #'0'
    cmp w2, #9
    b.hi parse_num_done
    mov x3, #10
    mul x0, x0, x3
    uxtb x2, w2
    add x0, x0, x2
    add x1, x1, #1
    b parse_num

parse_num_done:
    cbz x10, parse_range      // if not past_blank, parsing range
    // past_blank: this is an ingredient ID
    lsl x2, x25, #3
    str x0, [x24, x2]
    add x25, x25, #1
    b parse_loop

parse_range:
    // x0 contains start of range
    mov x26, x0

    // Skip the '-'
    ldrb w2, [x1]
    cmp w2, #'-'
    b.ne exit_error
    add x1, x1, #1

    // Parse end number
    mov x0, #0
parse_end:
    ldrb w2, [x1]
    sub w2, w2, #'0'
    cmp w2, #9
    b.hi parse_end_done
    mov x3, #10
    mul x0, x0, x3
    uxtb x2, w2
    add x0, x0, x2
    add x1, x1, #1
    b parse_end

parse_end_done:
    // Store range
    lsl x2, x23, #4
    str x26, [x22, x2]
    add x2, x2, #8
    str x0, [x22, x2]
    add x23, x23, #1
    b parse_loop

parse_done:
    // ===== Part 1 =====
    mov x26, #0             // fresh_count
    mov x27, #0             // ingredient index

part1_loop:
    cmp x27, x25
    b.ge part1_done

    // Load ingredient ID
    lsl x2, x27, #3
    ldr x0, [x24, x2]

    // Check against all ranges
    mov x1, #0              // range index

part1_range_check:
    cmp x1, x23
    b.ge part1_not_fresh

    // Load range
    lsl x2, x1, #4
    add x2, x22, x2
    ldr x3, [x2]            // start
    ldr x4, [x2, #8]        // end

    // Check if start <= ID <= end
    cmp x0, x3
    b.lt part1_next_range
    cmp x0, x4
    b.le part1_is_fresh

part1_next_range:
    add x1, x1, #1
    b part1_range_check

part1_is_fresh:
    add x26, x26, #1

part1_not_fresh:
    add x27, x27, #1
    b part1_loop

part1_done:
    // Print Part 1
    str x26, [sp]
    adrp x0, part1_fmt@PAGE
    add x0, x0, part1_fmt@PAGEOFF
    bl _printf

    // ===== Part 2 =====
    // Bubble sort ranges by start
    cmp x23, #1
    b.le sort_done

    sub x0, x23, #1

sort_outer:
    cbz x0, sort_done
    mov x1, #0

sort_inner:
    cmp x1, x0
    b.ge sort_outer_next

    // Load ranges[i]
    lsl x2, x1, #4
    add x2, x22, x2
    ldr x3, [x2]
    ldr x4, [x2, #8]

    // Load ranges[i+1]
    add x5, x1, #1
    lsl x5, x5, #4
    add x5, x22, x5
    ldr x6, [x5]
    ldr x7, [x5, #8]

    // If start[i] > start[i+1], swap
    cmp x3, x6
    b.le sort_no_swap

    str x6, [x2]
    str x7, [x2, #8]
    str x3, [x5]
    str x4, [x5, #8]

sort_no_swap:
    add x1, x1, #1
    b sort_inner

sort_outer_next:
    sub x0, x0, #1
    b sort_outer

sort_done:
    // Merge overlapping ranges
    cbz x23, part2_count

    ldr x0, [x22]
    ldr x1, [x22, #8]
    str x0, [x22]
    str x1, [x22, #8]
    mov x26, #1
    mov x27, #1

merge_loop:
    cmp x27, x23
    b.ge merge_done

    // Load current range
    lsl x2, x27, #4
    add x2, x22, x2
    ldr x3, [x2]
    ldr x4, [x2, #8]

    // Load last merged range
    sub x5, x26, #1
    lsl x5, x5, #4
    add x5, x22, x5
    ldr x6, [x5]
    ldr x8, [x5, #8]

    // Check if current.start <= merged_last.end + 1
    add x9, x8, #1
    cmp x3, x9
    b.gt merge_new_range

    // Merge: update end if needed
    cmp x8, x4
    b.ge merge_next
    str x4, [x5, #8]
    b merge_next

merge_new_range:
    lsl x6, x26, #4
    add x6, x22, x6
    str x3, [x6]
    str x4, [x6, #8]
    add x26, x26, #1

merge_next:
    add x27, x27, #1
    b merge_loop

merge_done:
part2_count:
    mov x27, #0
    mov x0, #0

count_loop:
    cmp x0, x26
    b.ge count_done

    lsl x1, x0, #4
    add x1, x22, x1
    ldr x2, [x1]
    ldr x3, [x1, #8]

    sub x4, x3, x2
    add x4, x4, #1
    add x27, x27, x4

    add x0, x0, #1
    b count_loop

count_done:
    // Print Part 2
    str x27, [sp]
    adrp x0, part2_fmt@PAGE
    add x0, x0, part2_fmt@PAGEOFF
    bl _printf

    mov x0, #0
    b exit

exit_error:
    mov x0, #1

exit:
    // Epilogue
    ldp x27, x28, [sp, #16]
    ldp x25, x26, [sp, #32]
    ldp x23, x24, [sp, #48]
    ldp x21, x22, [sp, #64]
    ldp x19, x20, [sp, #80]
    ldp x29, x30, [sp, #96]
    add sp, sp, #112
    ret
