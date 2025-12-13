// Advent of Code 2024 - Day 11: Plutonian Pebbles
// ARM64 Assembly for macOS
//
// Algorithm: Memoized recursion to count stones after N blinks
// Rules: 0→1, even digits→split, otherwise→×2024

.global _start
.align 4

// Hash table for memoization - using a smaller, more manageable size
.equ HASH_CAPACITY, 50000
.equ ENTRY_SIZE, 24  // value(8) + blinks(8) + result(8)

.data
input_file: .asciz "../input.txt"
msg_part1: .asciz "Part 1: "
msg_part2: .asciz "Part 2: "
msg_newline: .asciz "\n"

.bss
.align 16
file_buffer: .space 4096
stones_array: .space 800
hash_table: .space 1200000  // HASH_CAPACITY * ENTRY_SIZE
powers_of_10: .space 160    // 20 entries of 8 bytes each

.text

_start:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Initialize powers of 10
    bl      init_powers_of_10

    // Read input
    bl      read_input
    mov     x19, x0             // stone count

    // Part 1: 25 blinks
    bl      clear_hash
    mov     x20, #0             // total
    adrp    x21, stones_array@PAGE
    add     x21, x21, stones_array@PAGEOFF
    mov     x22, #0             // index

.L_part1_loop:
    cmp     x22, x19
    b.ge    .L_part1_done
    ldr     x0, [x21, x22, lsl #3]
    mov     x1, #25
    bl      count_stones
    add     x20, x20, x0
    add     x22, x22, #1
    b       .L_part1_loop

.L_part1_done:
    adrp    x0, msg_part1@PAGE
    add     x0, x0, msg_part1@PAGEOFF
    bl      print_str
    mov     x0, x20
    bl      print_num
    adrp    x0, msg_newline@PAGE
    add     x0, x0, msg_newline@PAGEOFF
    bl      print_str

    // Part 2: 75 blinks
    bl      clear_hash
    mov     x20, #0             // total
    mov     x22, #0             // index

.L_part2_loop:
    cmp     x22, x19
    b.ge    .L_part2_done
    ldr     x0, [x21, x22, lsl #3]
    mov     x1, #75
    bl      count_stones
    add     x20, x20, x0
    add     x22, x22, #1
    b       .L_part2_loop

.L_part2_done:
    adrp    x0, msg_part2@PAGE
    add     x0, x0, msg_part2@PAGEOFF
    bl      print_str
    mov     x0, x20
    bl      print_num
    adrp    x0, msg_newline@PAGE
    add     x0, x0, msg_newline@PAGEOFF
    bl      print_str

    mov     x0, #0
    mov     x16, #1
    svc     #0x80

// Initialize powers of 10 table
init_powers_of_10:
    adrp    x0, powers_of_10@PAGE
    add     x0, x0, powers_of_10@PAGEOFF
    mov     x1, #1
    mov     x2, #0
.L_init_pow10:
    cmp     x2, #20
    b.ge    .L_init_pow10_done
    str     x1, [x0, x2, lsl #3]
    mov     x3, #10
    mul     x1, x1, x3
    add     x2, x2, #1
    b       .L_init_pow10
.L_init_pow10_done:
    ret

// Read and parse input
read_input:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    // Open file
    mov     x16, #5
    adrp    x0, input_file@PAGE
    add     x0, x0, input_file@PAGEOFF
    mov     x1, #0
    mov     x2, #0
    svc     #0x80
    mov     x19, x0

    // Read file
    mov     x16, #3
    mov     x0, x19
    adrp    x1, file_buffer@PAGE
    add     x1, x1, file_buffer@PAGEOFF
    mov     x2, #4096
    svc     #0x80

    // Close file
    mov     x16, #6
    mov     x0, x19
    svc     #0x80

    // Parse numbers
    adrp    x19, file_buffer@PAGE
    add     x19, x19, file_buffer@PAGEOFF
    adrp    x21, stones_array@PAGE
    add     x21, x21, stones_array@PAGEOFF
    mov     x22, #0

.L_parse:
    ldrb    w0, [x19]
    cbz     w0, .L_parse_done
    cmp     w0, #'0'
    b.lt    .L_parse_skip
    cmp     w0, #'9'
    b.gt    .L_parse_skip

    // Parse number
    mov     x0, #0
.L_parse_digit:
    ldrb    w1, [x19]
    cmp     w1, #'0'
    b.lt    .L_parse_store
    cmp     w1, #'9'
    b.gt    .L_parse_store
    mov     x2, #10
    mul     x0, x0, x2
    sub     w1, w1, #'0'
    add     x0, x0, x1
    add     x19, x19, #1
    b       .L_parse_digit

.L_parse_store:
    str     x0, [x21, x22, lsl #3]
    add     x22, x22, #1
    b       .L_parse

.L_parse_skip:
    add     x19, x19, #1
    b       .L_parse

.L_parse_done:
    mov     x0, x22
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Count digits
// x0 = number
// Returns: x0 = digit count
count_digits:
    cbz     x0, .L_cd_zero
    mov     x1, #0
    mov     x2, x0
.L_cd_loop:
    cbz     x2, .L_cd_done
    add     x1, x1, #1
    mov     x3, #10
    udiv    x2, x2, x3
    b       .L_cd_loop
.L_cd_zero:
    mov     x1, #1
.L_cd_done:
    mov     x0, x1
    ret

// Split number at midpoint
// x0 = value, x1 = num_digits
// Returns: x0 = left, x1 = right
split_number:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0
    lsr     x20, x1, #1         // mid = num_digits / 2

    // Get 10^mid from table
    adrp    x2, powers_of_10@PAGE
    add     x2, x2, powers_of_10@PAGEOFF
    ldr     x2, [x2, x20, lsl #3]

    udiv    x0, x19, x2         // left = value / divisor
    msub    x1, x0, x2, x19     // right = value - left * divisor

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Hash function
// x0 = value, x1 = blinks
// Returns: x0 = hash
hash_func:
    // Mix value and blinks
    eor     x2, x0, x1
    mov     x3, #31
    mul     x0, x0, x3
    add     x0, x0, x1
    eor     x0, x0, x2, lsr #16

    // Modulo HASH_CAPACITY using multiplication trick
    mov     x2, #50000
    udiv    x3, x0, x2
    msub    x0, x3, x2, x0
    ret

// Clear hash table
clear_hash:
    adrp    x0, hash_table@PAGE
    add     x0, x0, hash_table@PAGEOFF
    mov     x1, #50000
    mov     x2, #0
.L_clear_loop:
    cmp     x2, x1
    b.ge    .L_clear_done
    mov     x3, #ENTRY_SIZE
    mul     x4, x2, x3
    add     x4, x0, x4
    mov     x5, #-1
    str     x5, [x4, #8]        // Mark as empty
    add     x2, x2, #1
    b       .L_clear_loop
.L_clear_done:
    ret

// Hash lookup
// x0 = value, x1 = blinks
// Returns: x0 = result (or -1 if not found)
hash_get:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!

    mov     x19, x0
    mov     x20, x1
    bl      hash_func
    mov     x21, x0

    adrp    x22, hash_table@PAGE
    add     x22, x22, hash_table@PAGEOFF

    mov     x0, #0              // probe count
.L_get_probe:
    cmp     x0, #50             // max probes
    b.ge    .L_get_not_found

    mov     x1, #ENTRY_SIZE
    mul     x2, x21, x1
    add     x2, x22, x2

    ldr     x3, [x2, #8]        // blinks field
    cmn     x3, #1
    b.eq    .L_get_not_found

    ldr     x4, [x2]            // value field
    cmp     x4, x19
    b.ne    .L_get_next
    cmp     x3, x20
    b.ne    .L_get_next

    ldr     x0, [x2, #16]       // result
    b       .L_get_done

.L_get_next:
    add     x21, x21, #1
    mov     x1, #50000
    cmp     x21, x1
    csel    x21, xzr, x21, ge
    add     x0, x0, #1
    b       .L_get_probe

.L_get_not_found:
    mov     x0, #-1

.L_get_done:
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Hash insert
// x0 = value, x1 = blinks, x2 = result
hash_put:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    str     x23, [sp, #-16]!

    mov     x19, x0
    mov     x20, x1
    mov     x23, x2
    bl      hash_func
    mov     x21, x0

    adrp    x22, hash_table@PAGE
    add     x22, x22, hash_table@PAGEOFF

    mov     x0, #0
.L_put_probe:
    cmp     x0, #50
    b.ge    .L_put_done

    mov     x1, #ENTRY_SIZE
    mul     x2, x21, x1
    add     x2, x22, x2

    ldr     x3, [x2, #8]
    cmn     x3, #1
    b.eq    .L_put_insert

    ldr     x4, [x2]
    cmp     x4, x19
    b.ne    .L_put_next
    cmp     x3, x20
    b.eq    .L_put_insert

.L_put_next:
    add     x21, x21, #1
    mov     x1, #50000
    cmp     x21, x1
    csel    x21, xzr, x21, ge
    add     x0, x0, #1
    b       .L_put_probe

.L_put_insert:
    str     x19, [x2]
    str     x20, [x2, #8]
    str     x23, [x2, #16]

.L_put_done:
    ldr     x23, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Count stones (memoized recursive)
// x0 = value, x1 = blinks
// Returns: x0 = count
count_stones:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp
    stp     x19, x20, [sp, #-16]!
    stp     x21, x22, [sp, #-16]!
    stp     x23, x24, [sp, #-16]!

    mov     x19, x0
    mov     x20, x1

    // Base case
    cbnz    x20, .L_cs_check_memo
    mov     x0, #1
    b       .L_cs_done

.L_cs_check_memo:
    mov     x0, x19
    mov     x1, x20
    bl      hash_get
    cmp     x0, #-1
    b.ne    .L_cs_done

    // Rule 1: 0 → 1
    cbnz    x19, .L_cs_rule2
    mov     x0, #1
    sub     x1, x20, #1
    bl      count_stones
    mov     x21, x0
    b       .L_cs_memoize

.L_cs_rule2:
    // Count digits
    mov     x0, x19
    bl      count_digits
    mov     x21, x0

    // Rule 2: even digits → split
    tst     x21, #1
    b.ne    .L_cs_rule3

    mov     x0, x19
    mov     x1, x21
    bl      split_number
    mov     x22, x0
    mov     x23, x1

    mov     x0, x22
    sub     x1, x20, #1
    bl      count_stones
    mov     x24, x0

    mov     x0, x23
    sub     x1, x20, #1
    bl      count_stones
    add     x21, x24, x0
    b       .L_cs_memoize

.L_cs_rule3:
    // Rule 3: multiply by 2024
    mov     x21, #2024
    mul     x21, x19, x21
    mov     x0, x21
    sub     x1, x20, #1
    bl      count_stones
    mov     x21, x0

.L_cs_memoize:
    mov     x0, x19
    mov     x1, x20
    mov     x2, x21
    bl      hash_put
    mov     x0, x21

.L_cs_done:
    ldp     x23, x24, [sp], #16
    ldp     x21, x22, [sp], #16
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Print null-terminated string
// x0 = pointer to string
print_str:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!

    mov     x19, x0

    // Find length
    mov     x20, #0
.L_strlen:
    ldrb    w1, [x19, x20]
    cbz     w1, .L_write
    add     x20, x20, #1
    b       .L_strlen

.L_write:
    // Write to stdout
    mov     x0, #1              // stdout
    mov     x1, x19             // buffer
    mov     x2, x20             // length
    mov     x16, #4             // write syscall
    svc     #0x80

    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret

// Print number
// x0 = number to print
print_num:
    stp     x29, x30, [sp, #-16]!
    stp     x19, x20, [sp, #-16]!
    sub     sp, sp, #32

    mov     x19, x0
    add     x20, sp, #31
    strb    wzr, [x20]

    // Handle zero case
    cbnz    x19, .L_pn_convert
    sub     x20, x20, #1
    mov     w0, #'0'
    strb    w0, [x20]
    b       .L_pn_print

.L_pn_convert:
    cbz     x19, .L_pn_print
    mov     x1, #10
    udiv    x2, x19, x1
    msub    x3, x2, x1, x19
    add     w3, w3, #'0'
    sub     x20, x20, #1
    strb    w3, [x20]
    mov     x19, x2
    b       .L_pn_convert

.L_pn_print:
    mov     x0, x20
    bl      print_str

    add     sp, sp, #32
    ldp     x19, x20, [sp], #16
    ldp     x29, x30, [sp], #16
    ret
