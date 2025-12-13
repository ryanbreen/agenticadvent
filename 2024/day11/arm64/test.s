.global _start
.data
input_file: .asciz "../input.txt"
fmt_num: .asciz "%lld "
fmt_nl: .asciz "\n"
.bss
file_buffer: .space 4096
stones: .space 800
.text
_start:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    
    // Open
    mov x16, #5
    adrp x0, input_file@PAGE
    add x0, x0, input_file@PAGEOFF
    mov x1, #0
    svc #0x80
    mov x19, x0
    
    // Read
    mov x16, #3
    mov x0, x19
    adrp x1, file_buffer@PAGE
    add x1, x1, file_buffer@PAGEOFF
    mov x2, #4096
    svc #0x80
    
    // Close
    mov x16, #6
    mov x0, x19
    svc #0x80
    
    // Parse
    adrp x19, file_buffer@PAGE
    add x19, x19, file_buffer@PAGEOFF
    adrp x20, stones@PAGE
    add x20, x20, stones@PAGEOFF
    mov x21, #0
    
.Lparse:
    ldrb w0, [x19]
    cbz w0, .Lprint
    cmp w0, #'0'
    b.lt .Lskip
    cmp w0, #'9'
    b.gt .Lskip
    
    mov x0, #0
.Ldigit:
    ldrb w1, [x19]
    cmp w1, #'0'
    b.lt .Lstore
    cmp w1, #'9'
    b.gt .Lstore
    mov x2, #10
    mul x0, x0, x2
    sub w1, w1, #'0'
    add x0, x0, x1
    add x19, x19, #1
    b .Ldigit
    
.Lstore:
    str x0, [x20, x21, lsl #3]
    add x21, x21, #1
    b .Lparse
    
.Lskip:
    add x19, x19, #1
    b .Lparse
    
.Lprint:
    mov x22, #0
.Lprint_loop:
    cmp x22, x21
    b.ge .Ldone
    adrp x0, fmt_num@PAGE
    add x0, x0, fmt_num@PAGEOFF
    ldr x1, [x20, x22, lsl #3]
    bl _printf
    add x22, x22, #1
    b .Lprint_loop
    
.Ldone:
    adrp x0, fmt_nl@PAGE
    add x0, x0, fmt_nl@PAGEOFF
    bl _printf
    mov x0, #0
    mov x16, #1
    svc #0x80
