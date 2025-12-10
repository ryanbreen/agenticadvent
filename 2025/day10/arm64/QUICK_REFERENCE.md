# Quick Reference: Rational Arithmetic Library

## Data Structure

```
struct Rational {
    int64_t numerator;     // Offset 0
    int64_t denominator;   // Offset 8
}; // Total: 16 bytes
```

## Function Quick Reference

### Creation
```assembly
// rat_new(numerator, denominator, *output)
mov     x0, #3              // numerator
mov     x1, #4              // denominator
adrp    x2, my_rat@PAGE
add     x2, x2, my_rat@PAGEOFF
bl      rat_new
// Result: my_rat = 3/4
```

### Arithmetic
```assembly
// rat_add(a, b, result)
adrp    x0, rat_a@PAGE
add     x0, x0, rat_a@PAGEOFF
adrp    x1, rat_b@PAGE
add     x1, x1, rat_b@PAGEOFF
adrp    x2, result@PAGE
add     x2, x2, result@PAGEOFF
bl      rat_add

// rat_sub, rat_mul, rat_div - same signature
```

### Queries
```assembly
// rat_is_zero(a) -> 1 if zero, 0 otherwise
adrp    x0, my_rat@PAGE
add     x0, x0, my_rat@PAGEOFF
bl      rat_is_zero
// x0 = 1 or 0

// rat_is_negative(a) -> 1 if negative, 0 otherwise
// rat_is_positive(a) -> 1 if positive, 0 otherwise
// rat_is_integer(a) -> 1 if integer, 0 otherwise
```

### Comparison
```assembly
// rat_compare(a, b) -> -1, 0, or 1
adrp    x0, rat_a@PAGE
add     x0, x0, rat_a@PAGEOFF
adrp    x1, rat_b@PAGE
add     x1, x1, rat_b@PAGEOFF
bl      rat_compare
// x0 = -1 (a < b), 0 (a == b), 1 (a > b)
```

### Debugging
```assembly
// rat_print(a)
adrp    x0, my_rat@PAGE
add     x0, x0, my_rat@PAGEOFF
bl      rat_print
// Output: "3/4"
```

## Common Patterns

### Allocate Rationals on Stack
```assembly
sub     sp, sp, #32         // Allocate 2 rationals (16 bytes each)
mov     x0, #1
mov     x1, #2
mov     x2, sp              // First rational at sp
bl      rat_new

mov     x0, #3
mov     x1, #4
add     x2, sp, #16         // Second rational at sp+16
bl      rat_new
```

### Allocate Rationals in .bss
```assembly
.bss
    .align 8
my_rat: .space 16

.text
    mov     x0, #5
    mov     x1, #7
    adrp    x2, my_rat@PAGE
    add     x2, x2, my_rat@PAGEOFF
    bl      rat_new
```

### Chain Operations
```assembly
// Compute: (1/2 + 1/3) * 2/1

// Step 1: 1/2 + 1/3 -> temp
adrp    x0, rat_half@PAGE
add     x0, x0, rat_half@PAGEOFF
adrp    x1, rat_third@PAGE
add     x1, x1, rat_third@PAGEOFF
adrp    x2, temp@PAGE
add     x2, x2, temp@PAGEOFF
bl      rat_add

// Step 2: temp * 2/1 -> result
adrp    x0, temp@PAGE
add     x0, x0, temp@PAGEOFF
adrp    x1, rat_two@PAGE
add     x1, x1, rat_two@PAGEOFF
adrp    x2, result@PAGE
add     x2, x2, result@PAGEOFF
bl      rat_mul
```

### Copy Rational
```assembly
// Copy rat_a to rat_b
adrp    x0, rat_a@PAGE
add     x0, x0, rat_a@PAGEOFF
ldp     x1, x2, [x0]        // Load num, den

adrp    x0, rat_b@PAGE
add     x0, x0, rat_b@PAGEOFF
stp     x1, x2, [x0]        // Store num, den
```

### Load Macro (Convenience)
```assembly
.macro load_addr reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// Usage:
load_addr x0, my_rat
bl      rat_print
```

## Register Usage Conventions

### Inputs
- `x0-x7`: Arguments
- Pointers to rationals passed in x0, x1, x2, etc.

### Outputs
- `x0`: Return value (for queries) or status

### Preserved
- `x19-x28`: Callee-saved (preserved across calls)
- `x29`: Frame pointer
- `x30`: Link register

### Clobbered
- `x0-x18`: Can be modified by any function

## Memory Layout Examples

### Single Rational
```
Address  Content
0x1000:  numerator   (8 bytes)
0x1008:  denominator (8 bytes)
```

### Array of Rationals
```
rat[0]:  0x1000 - 0x100F
rat[1]:  0x1010 - 0x101F
rat[2]:  0x1020 - 0x102F
...
```

### Matrix (2x3 of rationals)
```
[0][0]:  0x1000 - 0x100F  (16 bytes)
[0][1]:  0x1010 - 0x101F
[0][2]:  0x1020 - 0x102F
[1][0]:  0x1030 - 0x103F
[1][1]:  0x1040 - 0x104F
[1][2]:  0x1050 - 0x105F
```

Access: `base + (row * n_cols + col) * 16`

## Common Mistakes

### 1. Forgetting to Save Registers
```assembly
// WRONG: x19 not saved
my_function:
    mov     x19, x0         // Clobbers caller's x19!
    // ...
    ret

// CORRECT:
my_function:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, x0         // Now safe
    // ...

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret
```

### 2. Stack Misalignment
```assembly
// WRONG: -20 is not multiple of 16
stp     x29, x30, [sp, #-20]!

// CORRECT:
stp     x29, x30, [sp, #-32]!
```

### 3. Using Result Before Normalization
```assembly
// WRONG: Manually building rational without normalization
adrp    x0, my_rat@PAGE
add     x0, x0, my_rat@PAGEOFF
mov     x1, #4
mov     x2, #6
stp     x1, x2, [x0]        // 4/6 not in lowest terms!

// CORRECT:
mov     x0, #4
mov     x1, #6
adrp    x2, my_rat@PAGE
add     x2, x2, my_rat@PAGEOFF
bl      rat_new             // Normalizes to 2/3
```

### 4. Forgetting Output Parameter
```assembly
// WRONG: Result overwrites input
adrp    x0, rat_a@PAGE
add     x0, x0, rat_a@PAGEOFF
adrp    x1, rat_b@PAGE
add     x1, x1, rat_b@PAGEOFF
mov     x2, x0              // Output = input!
bl      rat_add             // rat_a destroyed

// CORRECT: Use separate output
adrp    x2, result@PAGE
add     x2, x2, result@PAGEOFF
bl      rat_add             // rat_a preserved
```

## Build & Test

```bash
# Build library
make rational

# Run tests
./rational

# Build with your code
as your_code.s -o your_code.o
as rational.s -o rational_lib.o
ld your_code.o rational_lib.o -o your_program \
   -lSystem \
   -syslibroot `xcrun -sdk macosx --show-sdk-path` \
   -e _your_main \
   -arch arm64
```

## Including in Your Project

### Method 1: Direct Assembly
```assembly
.global _my_main

// Declare external functions
.extern rat_new
.extern rat_add
.extern rat_print

_my_main:
    // Your code here
```

Build:
```bash
as my_code.s -o my_code.o
as rational.s -o rational.o
ld my_code.o rational.o -o my_program -lSystem ...
```

### Method 2: Include Source
```assembly
.include "rational.s"

_my_main:
    // Your code here
```

Build:
```bash
as my_code.s -o my_code.o
ld my_code.o -o my_program -lSystem ...
```

## Debugging Tips

### Print Intermediate Results
```assembly
// After each operation:
adrp    x0, result@PAGE
add     x0, x0, result@PAGEOFF
bl      rat_print

mov     x0, #'\n'
bl      print_char
```

### Check Invariants
```assembly
// Verify denominator is positive
adrp    x0, my_rat@PAGE
add     x0, x0, my_rat@PAGEOFF
ldr     x1, [x0, #8]        // Load denominator
cmp     x1, #0
b.gt    .Lok
// Error: denominator not positive!
.Lok:
```

### Use LLDB
```bash
lldb ./my_program

# Set breakpoint
(lldb) b rat_add

# Run
(lldb) r

# Examine registers
(lldb) register read

# Examine memory
(lldb) memory read 0x...

# Step instruction
(lldb) si
```

## Performance Tips

### 1. Reuse Storage
```assembly
// Instead of allocating new rationals:
.bss
temp1: .space 16
temp2: .space 16
result: .space 16

// Reuse temp1, temp2 for intermediate results
```

### 2. Minimize Calls
```assembly
// If you just need to check for zero:
ldr     x0, [rat_ptr]       // Load numerator
cbz     x0, .Lis_zero       // Check directly

// Instead of:
mov     x0, rat_ptr
bl      rat_is_zero
cbz     x0, .Lis_zero
```

### 3. Batch Operations
```assembly
// Process multiple rationals in a loop
// to improve cache locality
mov     x19, #0             // Index
.Lloop:
    // Process rat[x19]
    add     x19, x19, #1
    cmp     x19, #count
    b.lt    .Lloop
```

## See Also

- `RATIONAL_LIB.md` - Detailed API documentation
- `IMPLEMENTATION_NOTES.md` - Design decisions and algorithms
- `README.md` - Project overview and getting started
- `matrix_example.s` - Complete working example
