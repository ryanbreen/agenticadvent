// Day 20: Pulse Propagation - ARM64 Assembly (macOS)
// Simplified implementation for debugging

.global _main
.align 4

.equ MAX_MODULES, 64
.equ MAX_NAME, 16
.equ MAX_DEST, 8
.equ MAX_INP, 16
.equ BUFFER_SIZE, 8192
.equ QUEUE_SIZE, 20000

// Types
.equ T_NONE, 0
.equ T_BROAD, 1
.equ T_FLIP, 2
.equ T_CONJ, 3

// Simpler module: 512 bytes each for safety
// 0-7: type
// 8-23: name
// 24-31: state
// 32-39: ndest
// 40-103: dests (8 * 8)
// 104-111: ninp
// 112-239: inp_idx (16 * 8)
// 240-367: inp_mem (16 * 8)
.equ M_TYPE, 0
.equ M_NAME, 8
.equ M_STATE, 24
.equ M_NDEST, 32
.equ M_DEST, 40
.equ M_NINP, 104
.equ M_INP, 112
.equ M_MEM, 240
.equ M_SIZE, 512

.data
path:       .asciz "../input.txt"
fmt1:       .asciz "Part 1: %lld\n"
fmt2:       .asciz "Part 2: %lld\n"

.bss
.align 4
buf:        .skip BUFFER_SIZE
mods:       .skip MAX_MODULES * M_SIZE
nmod:       .skip 8
que:        .skip QUEUE_SIZE * 24
qhead:      .skip 8
qtail:      .skip 8
bcast:      .skip 8
rxfeed:     .skip 8
watch:      .skip MAX_INP * 8
nwatch:     .skip 8
cycles:     .skip MAX_INP * 8
found:      .skip MAX_INP * 8
tmp:        .skip 64

.text

_main:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    bl      readfile
    bl      parse
    bl      initconj

    bl      resetall
    bl      part1
    str     x0, [sp, #-16]!     // variadic arg on stack
    adrp    x0, fmt1@PAGE
    add     x0, x0, fmt1@PAGEOFF
    bl      _printf
    add     sp, sp, #16

    bl      resetall
    bl      part2
    str     x0, [sp, #-16]!     // variadic arg on stack
    adrp    x0, fmt2@PAGE
    add     x0, x0, fmt2@PAGEOFF
    bl      _printf
    add     sp, sp, #16

    mov     x0, #0
    ldp     x29, x30, [sp], #16
    ret

readfile:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    adrp    x0, path@PAGE
    add     x0, x0, path@PAGEOFF
    mov     x1, #0
    bl      _open
    mov     x19, x0

    adrp    x1, buf@PAGE
    add     x1, x1, buf@PAGEOFF
    mov     x2, #BUFFER_SIZE
    bl      _read

    adrp    x1, buf@PAGE
    add     x1, x1, buf@PAGEOFF
    strb    wzr, [x1, x0]

    mov     x0, x19
    bl      _close

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// Hash a name - simple sum * position
hashname:
    mov     x1, #0
    mov     x2, #1
1:  ldrb    w3, [x0], #1
    cbz     w3, 2f
    madd    x1, x3, x2, x1
    add     x2, x2, #256
    b       1b
2:  mov     x0, x1
    ret

// Parse input
parse:
    stp     x29, x30, [sp, #-80]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]

    adrp    x19, buf@PAGE
    add     x19, x19, buf@PAGEOFF
    adrp    x20, mods@PAGE
    add     x20, x20, mods@PAGEOFF
    mov     x21, #0              // nmod

pline:
    ldrb    w0, [x19]
    cbz     w0, pdone
    cmp     w0, #'\n'
    b.eq    pskip
    cmp     w0, #'\r'
    b.eq    pskip

    // Get module pointer
    mov     x22, #M_SIZE
    mul     x22, x21, x22
    add     x22, x20, x22

    // Init
    str     xzr, [x22, #M_NINP]
    str     xzr, [x22, #M_NDEST]
    str     xzr, [x22, #M_STATE]

    // Type
    ldrb    w0, [x19]
    cmp     w0, #'%'
    b.eq    pflip
    cmp     w0, #'&'
    b.eq    pconj

    // Broadcaster
    mov     x0, #T_BROAD
    str     x0, [x22, #M_TYPE]
    adrp    x0, bcast@PAGE
    add     x0, x0, bcast@PAGEOFF
    str     x21, [x0]
    mov     x23, #0
    add     x24, x22, #M_NAME
3:  ldrb    w0, [x19, x23]
    cmp     w0, #' '
    b.eq    4f
    strb    w0, [x24, x23]
    add     x23, x23, #1
    cmp     x23, #11
    b.lt    3b
4:  strb    wzr, [x24, x23]
    add     x19, x19, x23
    b       ptoarrow

pflip:
    add     x19, x19, #1
    mov     x0, #T_FLIP
    str     x0, [x22, #M_TYPE]
    b       pname

pconj:
    add     x19, x19, #1
    mov     x0, #T_CONJ
    str     x0, [x22, #M_TYPE]

pname:
    mov     x23, #0
    add     x24, x22, #M_NAME
5:  ldrb    w0, [x19]
    cmp     w0, #' '
    b.eq    6f
    strb    w0, [x24, x23]
    add     x19, x19, #1
    add     x23, x23, #1
    b       5b
6:  strb    wzr, [x24, x23]

ptoarrow:
7:  ldrb    w0, [x19]
    cmp     w0, #'>'
    b.eq    8f
    add     x19, x19, #1
    b       7b
8:  add     x19, x19, #2    // skip "> "

pdest:
    ldrb    w0, [x19]
    cmp     w0, #' '
    b.eq    pskipd
    cmp     w0, #','
    b.eq    pskipd
    cmp     w0, #'\n'
    b.eq    pdestdone
    cmp     w0, #'\r'
    b.eq    pdestdone
    cbz     w0, pdestdone

    // Copy dest name to tmp
    adrp    x23, tmp@PAGE
    add     x23, x23, tmp@PAGEOFF
    mov     x24, #0
9:  ldrb    w0, [x19]
    cmp     w0, #','
    b.eq    10f
    cmp     w0, #' '
    b.eq    10f
    cmp     w0, #'\n'
    b.eq    10f
    cmp     w0, #'\r'
    b.eq    10f
    cbz     w0, 10f
    strb    w0, [x23, x24]
    add     x19, x19, #1
    add     x24, x24, #1
    b       9b
10: strb    wzr, [x23, x24]

    // Hash and store
    ldr     x24, [x22, #M_NDEST]
    cmp     x24, #MAX_DEST
    b.ge    pdest

    mov     x0, x23
    bl      hashname
    lsl     x1, x24, #3
    add     x1, x22, x1
    str     x0, [x1, #M_DEST]
    add     x24, x24, #1
    str     x24, [x22, #M_NDEST]
    b       pdest

pskipd:
    add     x19, x19, #1
    b       pdest

pdestdone:
    add     x21, x21, #1
    b       pline

pskip:
    add     x19, x19, #1
    b       pline

pdone:
    adrp    x0, nmod@PAGE
    add     x0, x0, nmod@PAGEOFF
    str     x21, [x0]

    bl      resolve

    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x25, x26, [sp, #64]
    ldp     x29, x30, [sp], #80
    ret

// Resolve hashes to indices
resolve:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    adrp    x19, mods@PAGE
    add     x19, x19, mods@PAGEOFF
    adrp    x0, nmod@PAGE
    add     x0, x0, nmod@PAGEOFF
    ldr     x20, [x0]

    mov     x21, #0
rout:
    cmp     x21, x20
    b.ge    rdone
    mov     x0, #M_SIZE
    mul     x0, x21, x0
    add     x22, x19, x0

    ldr     x1, [x22, #M_NDEST]
    mov     x2, #0
rin:
    cmp     x2, x1
    b.ge    rnext

    lsl     x3, x2, #3
    add     x3, x22, x3
    ldr     x23, [x3, #M_DEST]   // hash

    mov     x4, #0
    mov     x24, #-1
rfind:
    cmp     x4, x20
    b.ge    rstore

    mov     x5, #M_SIZE
    mul     x5, x4, x5
    add     x5, x19, x5
    add     x0, x5, #M_NAME
    stp     x1, x2, [sp, #-32]!
    stp     x3, x4, [sp, #16]
    bl      hashname
    ldp     x3, x4, [sp, #16]
    ldp     x1, x2, [sp], #32
    cmp     x0, x23
    b.ne    rnomatch
    mov     x24, x4
    b       rstore
rnomatch:
    add     x4, x4, #1
    b       rfind

rstore:
    lsl     x3, x2, #3
    add     x3, x22, x3
    str     x24, [x3, #M_DEST]
    add     x2, x2, #1
    b       rin

rnext:
    add     x21, x21, #1
    b       rout

rdone:
    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x29, x30, [sp], #64
    ret

// Init conjunction inputs
initconj:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    adrp    x19, mods@PAGE
    add     x19, x19, mods@PAGEOFF
    adrp    x0, nmod@PAGE
    add     x0, x0, nmod@PAGEOFF
    ldr     x20, [x0]

    mov     x21, #0
icout:
    cmp     x21, x20
    b.ge    icdone

    mov     x0, #M_SIZE
    mul     x0, x21, x0
    add     x22, x19, x0

    ldr     x1, [x22, #M_NDEST]
    mov     x2, #0
icdest:
    cmp     x2, x1
    b.ge    icnext

    lsl     x3, x2, #3
    add     x3, x22, x3
    ldr     x23, [x3, #M_DEST]

    cmp     x23, #0
    b.lt    icskip
    cmp     x23, x20
    b.ge    icskip

    mov     x0, #M_SIZE
    mul     x0, x23, x0
    add     x24, x19, x0

    ldr     x0, [x24, #M_TYPE]
    cmp     x0, #T_CONJ
    b.ne    icskip

    ldr     x3, [x24, #M_NINP]
    cmp     x3, #MAX_INP
    b.ge    icskip

    lsl     x4, x3, #3
    add     x5, x24, x4
    str     x21, [x5, #M_INP]
    str     xzr, [x5, #M_MEM]

    add     x3, x3, #1
    str     x3, [x24, #M_NINP]

icskip:
    add     x2, x2, #1
    b       icdest

icnext:
    add     x21, x21, #1
    b       icout

icdone:
    bl      findrx
    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x29, x30, [sp], #64
    ret

findrx:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    adrp    x19, mods@PAGE
    add     x19, x19, mods@PAGEOFF
    adrp    x0, nmod@PAGE
    add     x0, x0, nmod@PAGEOFF
    ldr     x20, [x0]

    mov     x21, #0
frxloop:
    cmp     x21, x20
    b.ge    frxnone

    mov     x0, #M_SIZE
    mul     x0, x21, x0
    add     x22, x19, x0

    ldr     x1, [x22, #M_NDEST]
    mov     x2, #0
frxdest:
    cmp     x2, x1
    b.ge    frxnext
    lsl     x3, x2, #3
    add     x3, x22, x3
    ldr     x4, [x3, #M_DEST]
    cmn     x4, #1
    b.eq    frxfound
    add     x2, x2, #1
    b       frxdest

frxnext:
    add     x21, x21, #1
    b       frxloop

frxfound:
    adrp    x0, rxfeed@PAGE
    add     x0, x0, rxfeed@PAGEOFF
    str     x21, [x0]

    ldr     x1, [x22, #M_NINP]
    adrp    x2, nwatch@PAGE
    add     x2, x2, nwatch@PAGEOFF
    str     x1, [x2]

    adrp    x2, watch@PAGE
    add     x2, x2, watch@PAGEOFF
    adrp    x3, cycles@PAGE
    add     x3, x3, cycles@PAGEOFF
    adrp    x4, found@PAGE
    add     x4, x4, found@PAGEOFF

    mov     x5, #0
frxcopy:
    cmp     x5, x1
    b.ge    frxdone
    lsl     x6, x5, #3
    add     x7, x22, x6
    ldr     x8, [x7, #M_INP]
    str     x8, [x2, x6]
    str     xzr, [x3, x6]
    str     xzr, [x4, x6]
    add     x5, x5, #1
    b       frxcopy

frxdone:
    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x29, x30, [sp], #48
    ret

frxnone:
    adrp    x0, rxfeed@PAGE
    add     x0, x0, rxfeed@PAGEOFF
    mov     x1, #-1
    str     x1, [x0]
    adrp    x0, nwatch@PAGE
    add     x0, x0, nwatch@PAGEOFF
    str     xzr, [x0]
    b       frxdone

resetall:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    adrp    x0, mods@PAGE
    add     x0, x0, mods@PAGEOFF
    adrp    x1, nmod@PAGE
    add     x1, x1, nmod@PAGEOFF
    ldr     x1, [x1]
    mov     x2, #0

rsloop:
    cmp     x2, x1
    b.ge    rsdone

    mov     x3, #M_SIZE
    mul     x3, x2, x3
    add     x3, x0, x3

    ldr     x4, [x3, #M_TYPE]
    cmp     x4, #T_FLIP
    b.ne    rsconj
    str     xzr, [x3, #M_STATE]
    b       rsnext

rsconj:
    cmp     x4, #T_CONJ
    b.ne    rsnext
    ldr     x5, [x3, #M_NINP]
    mov     x6, #0
rsmem:
    cmp     x6, x5
    b.ge    rsnext
    lsl     x7, x6, #3
    add     x7, x3, x7
    str     xzr, [x7, #M_MEM]
    add     x6, x6, #1
    b       rsmem

rsnext:
    add     x2, x2, #1
    b       rsloop

rsdone:
    adrp    x0, found@PAGE
    add     x0, x0, found@PAGEOFF
    mov     x1, #0
rscycles:
    cmp     x1, #MAX_INP
    b.ge    rsend
    lsl     x2, x1, #3
    str     xzr, [x0, x2]
    add     x1, x1, #1
    b       rscycles

rsend:
    ldp     x29, x30, [sp], #16
    ret

part1:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    mov     x19, #0     // low
    mov     x20, #0     // high
    mov     x21, #0     // press

p1loop:
    cmp     x21, #1000
    b.ge    p1done

    mov     x0, x21
    bl      simpress

    add     x19, x19, x0
    add     x20, x20, x1

    add     x21, x21, #1
    b       p1loop

p1done:
    mul     x0, x19, x20

    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x29, x30, [sp], #48
    ret

// Simulate one press
// x0 = press number (0-based)
// Returns x0=low, x1=high
simpress:
    stp     x29, x30, [sp, #-128]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    mov     x28, x0         // press number
    mov     x19, #0         // low
    mov     x20, #0         // high

    // Reset queue
    adrp    x21, que@PAGE
    add     x21, x21, que@PAGEOFF
    adrp    x0, qhead@PAGE
    add     x0, x0, qhead@PAGEOFF
    str     xzr, [x0]
    adrp    x0, qtail@PAGE
    add     x0, x0, qtail@PAGEOFF
    str     xzr, [x0]

    // Enqueue: -1, broadcaster, 0
    adrp    x1, bcast@PAGE
    add     x1, x1, bcast@PAGEOFF
    ldr     x1, [x1]
    mov     x0, #-1
    mov     x2, #0
    bl      enq

    adrp    x22, mods@PAGE
    add     x22, x22, mods@PAGEOFF
    adrp    x0, nmod@PAGE
    add     x0, x0, nmod@PAGEOFF
    ldr     x23, [x0]

simloop:
    // Dequeue
    adrp    x0, qhead@PAGE
    add     x0, x0, qhead@PAGEOFF
    ldr     x24, [x0]
    adrp    x1, qtail@PAGE
    add     x1, x1, qtail@PAGEOFF
    ldr     x25, [x1]
    cmp     x24, x25
    b.ge    simdone

    mov     x0, #24
    mul     x0, x24, x0
    add     x0, x21, x0
    ldr     x1, [x0]        // src
    ldr     x2, [x0, #8]    // dst
    ldr     x3, [x0, #16]   // pulse

    add     x24, x24, #1
    adrp    x0, qhead@PAGE
    add     x0, x0, qhead@PAGEOFF
    str     x24, [x0]

    // Count
    cbnz    x3, counthi
    add     x19, x19, #1
    b       countdone
counthi:
    add     x20, x20, #1
countdone:

    // Track cycles
    cbz     x3, notrack
    cmp     x1, #0
    b.lt    notrack

    adrp    x4, watch@PAGE
    add     x4, x4, watch@PAGEOFF
    adrp    x5, nwatch@PAGE
    add     x5, x5, nwatch@PAGEOFF
    ldr     x5, [x5]
    mov     x6, #0
trackloop:
    cmp     x6, x5
    b.ge    notrack
    lsl     x7, x6, #3
    ldr     x8, [x4, x7]
    cmp     x8, x1
    b.ne    tracknext

    adrp    x8, found@PAGE
    add     x8, x8, found@PAGEOFF
    ldr     x9, [x8, x7]
    cbnz    x9, notrack

    mov     x9, #1
    str     x9, [x8, x7]

    adrp    x8, cycles@PAGE
    add     x8, x8, cycles@PAGEOFF
    add     x9, x28, #1
    str     x9, [x8, x7]
    b       notrack

tracknext:
    add     x6, x6, #1
    b       trackloop

notrack:
    // Valid dest?
    cmp     x2, #0
    b.lt    simloop
    cmp     x2, x23
    b.ge    simloop

    // Get module
    mov     x0, #M_SIZE
    mul     x0, x2, x0
    add     x26, x22, x0

    ldr     x4, [x26, #M_TYPE]

    cmp     x4, #T_BROAD
    b.eq    dobroad
    cmp     x4, #T_FLIP
    b.eq    doflip
    cmp     x4, #T_CONJ
    b.eq    doconj
    b       simloop

dobroad:
    ldr     x5, [x26, #M_NDEST]
    mov     x6, #0
broadloop:
    cmp     x6, x5
    b.ge    simloop
    lsl     x7, x6, #3
    add     x7, x26, x7
    ldr     x8, [x7, #M_DEST]

    // Save and enqueue
    str     x1, [sp, #96]
    str     x2, [sp, #104]
    str     x3, [sp, #112]
    stp     x5, x6, [sp, #-16]!
    mov     x0, x2
    mov     x1, x8
    mov     x2, x3
    bl      enq
    ldp     x5, x6, [sp], #16
    ldr     x1, [sp, #96]
    ldr     x2, [sp, #104]
    ldr     x3, [sp, #112]

    add     x6, x6, #1
    b       broadloop

doflip:
    cbnz    x3, simloop
    ldr     x5, [x26, #M_STATE]
    eor     x5, x5, #1
    str     x5, [x26, #M_STATE]

    ldr     x6, [x26, #M_NDEST]
    mov     x7, #0
fliploop:
    cmp     x7, x6
    b.ge    simloop
    lsl     x8, x7, #3
    add     x8, x26, x8
    ldr     x9, [x8, #M_DEST]

    str     x1, [sp, #96]
    str     x2, [sp, #104]
    stp     x5, x6, [sp, #-16]!
    str     x7, [sp, #-16]!
    mov     x0, x2
    mov     x1, x9
    mov     x2, x5
    bl      enq
    ldr     x7, [sp], #16
    ldp     x5, x6, [sp], #16
    ldr     x1, [sp, #96]
    ldr     x2, [sp, #104]

    add     x7, x7, #1
    b       fliploop

doconj:
    // Update input mem
    ldr     x5, [x26, #M_NINP]
    mov     x6, #0
conjfind:
    cmp     x6, x5
    b.ge    conjcheck
    lsl     x7, x6, #3
    add     x8, x26, x7
    ldr     x9, [x8, #M_INP]
    cmp     x9, x1
    b.ne    conjnext
    str     x3, [x8, #M_MEM]
    b       conjcheck
conjnext:
    add     x6, x6, #1
    b       conjfind

conjcheck:
    // Check all high
    mov     x6, #0
    mov     x7, #1
conjall:
    cmp     x6, x5
    b.ge    conjsend
    lsl     x8, x6, #3
    add     x8, x26, x8
    ldr     x9, [x8, #M_MEM]
    cbnz    x9, conjhigh
    mov     x7, #0
conjhigh:
    add     x6, x6, #1
    b       conjall

conjsend:
    eor     x5, x7, #1      // output pulse

    ldr     x6, [x26, #M_NDEST]
    mov     x7, #0
conjloop:
    cmp     x7, x6
    b.ge    simloop
    lsl     x8, x7, #3
    add     x8, x26, x8
    ldr     x9, [x8, #M_DEST]

    str     x1, [sp, #96]
    str     x2, [sp, #104]
    stp     x5, x6, [sp, #-16]!
    str     x7, [sp, #-16]!
    mov     x0, x2
    mov     x1, x9
    mov     x2, x5
    bl      enq
    ldr     x7, [sp], #16
    ldp     x5, x6, [sp], #16
    ldr     x1, [sp, #96]
    ldr     x2, [sp, #104]

    add     x7, x7, #1
    b       conjloop

simdone:
    mov     x0, x19
    mov     x1, x20

    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x25, x26, [sp, #64]
    ldp     x27, x28, [sp, #80]
    ldp     x29, x30, [sp], #128
    ret

enq:
    adrp    x3, qtail@PAGE
    add     x3, x3, qtail@PAGEOFF
    ldr     x4, [x3]

    adrp    x5, que@PAGE
    add     x5, x5, que@PAGEOFF
    mov     x6, #24
    mul     x6, x4, x6
    add     x5, x5, x6

    str     x0, [x5]
    str     x1, [x5, #8]
    str     x2, [x5, #16]

    add     x4, x4, #1
    str     x4, [x3]
    ret

part2:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]

    adrp    x0, rxfeed@PAGE
    add     x0, x0, rxfeed@PAGEOFF
    ldr     x0, [x0]
    cmp     x0, #0
    b.lt    p2none

    adrp    x0, nwatch@PAGE
    add     x0, x0, nwatch@PAGEOFF
    ldr     x19, [x0]
    mov     x20, #0

p2loop:
    adrp    x0, found@PAGE
    add     x0, x0, found@PAGEOFF
    mov     x1, #0
    mov     x2, #0
p2count:
    cmp     x1, x19
    b.ge    p2check
    lsl     x3, x1, #3
    ldr     x4, [x0, x3]
    add     x2, x2, x4
    add     x1, x1, #1
    b       p2count
p2check:
    cmp     x2, x19
    b.ge    p2lcm

    mov     x0, x20
    bl      simpress

    add     x20, x20, #1
    b       p2loop

p2lcm:
    adrp    x21, cycles@PAGE
    add     x21, x21, cycles@PAGEOFF

    ldr     x0, [x21]
    mov     x1, #1
p2lcmloop:
    cmp     x1, x19
    b.ge    p2done
    lsl     x2, x1, #3
    ldr     x22, [x21, x2]

    str     x1, [sp, #-16]!
    mov     x1, x22
    bl      lcmfn
    ldr     x1, [sp], #16

    add     x1, x1, #1
    b       p2lcmloop

p2done:
    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x29, x30, [sp], #48
    ret

p2none:
    mov     x0, #0
    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x29, x30, [sp], #48
    ret

gcdfn:
    cbz     x1, gcddone
    udiv    x2, x0, x1
    msub    x0, x2, x1, x0
    mov     x3, x0
    mov     x0, x1
    mov     x1, x3
    b       gcdfn
gcddone:
    ret

lcmfn:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]

    mov     x19, x0
    mov     x20, x1
    bl      gcdfn
    mul     x1, x19, x20
    udiv    x0, x1, x0

    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret
