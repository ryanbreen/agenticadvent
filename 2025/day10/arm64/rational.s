// ============================================================================
// Rational Arithmetic Library for ARM64 Assembly (macOS)
// ============================================================================
//
// PURPOSE:
// This library implements exact rational number arithmetic using 64-bit
// signed integers for numerator and denominator. It's designed to support
// Gaussian elimination and linear algebra operations that require exact
// arithmetic (no floating-point precision loss).
//
// REPRESENTATION:
// A rational number is stored as a pair of 64-bit signed integers:
//   Offset 0:  numerator   (int64_t)
//   Offset 8:  denominator (int64_t)
//   Total size: 16 bytes
//
// INVARIANTS:
// 1. Denominator is always positive (non-zero)
// 2. GCD(numerator, denominator) = 1 (always in lowest terms)
// 3. Sign is carried by the numerator only
//
// CALLING CONVENTION (ARM64 ABI):
// - x0-x7: argument registers
// - x0: return value
// - x9-x15: caller-saved (temporary)
// - x19-x28: callee-saved (must preserve)
// - x29: frame pointer
// - x30: link register (return address)
// - sp: stack pointer (must be 16-byte aligned)
//
// REGISTER USAGE PATTERNS:
// Most functions use:
//   x0, x1: input operand pointers or values
//   x2: output pointer (where applicable)
//   x19-x22: preserved working registers
//   x0: return value (status or result)
//
// ============================================================================

.global _test_rational_main
.global gcd
.global abs64
.global rat_normalize
.global rat_new
.global rat_add
.global rat_sub
.global rat_mul
.global rat_div
.global rat_neg
.global rat_is_zero
.global rat_is_negative
.global rat_is_positive
.global rat_is_integer
.global rat_to_int
.global rat_compare
.global rat_print
.global print_char
.global print_int64
.global print_string
.align 4

// Macro to load address (for position-independent code)
.macro load_addr reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm

// ============================================================================
// GCD: Greatest Common Divisor (Euclidean Algorithm)
// ============================================================================
// Computes GCD of two non-negative 64-bit integers using Euclidean algorithm.
// This is the foundation for simplifying fractions.
//
// Input:
//   x0 = a (non-negative integer)
//   x1 = b (non-negative integer)
// Output:
//   x0 = gcd(a, b)
// Preserves: x19-x28, x29, x30
// Clobbers: x0, x1
//
// Algorithm:
//   while b != 0:
//     temp = b
//     b = a % b
//     a = temp
//   return a
// ============================================================================
gcd:
    // Handle base cases
    cbnz    x1, .Lgcd_loop
    // b == 0, return a
    ret

.Lgcd_loop:
    // Compute a % b
    udiv    x2, x0, x1          // x2 = a / b
    msub    x2, x2, x1, x0      // x2 = a - (a/b)*b = a % b

    // Shift: a = b, b = remainder
    mov     x0, x1
    mov     x1, x2

    // Continue if b != 0
    cbnz    x1, .Lgcd_loop

    ret

// ============================================================================
// ABS: Absolute value
// ============================================================================
// Input:  x0 = signed integer
// Output: x0 = |x0|
// ============================================================================
abs64:
    cmp     x0, #0
    cneg    x0, x0, lt          // if x0 < 0, x0 = -x0
    ret

// ============================================================================
// rat_normalize: Normalize a rational number
// ============================================================================
// Ensures:
// 1. Denominator is positive
// 2. GCD(num, den) = 1 (lowest terms)
//
// Input:
//   x0 = pointer to rational (16 bytes: [numerator, denominator])
// Output:
//   rational at x0 is modified in place
// Preserves: x19-x28, x29
// Clobbers: x0-x6
// ============================================================================
rat_normalize:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    str     x21, [sp, #32]

    mov     x19, x0             // Save pointer to rational

    // Load numerator and denominator
    ldp     x20, x21, [x19]     // x20 = num, x21 = den

    // Check if denominator is zero (error condition)
    cbnz    x21, .Lnorm_den_ok
    // Denominator is zero - set to 1 to avoid division by zero
    mov     x21, #1
    str     x21, [x19, #8]
    b       .Lnorm_done

.Lnorm_den_ok:
    // Ensure denominator is positive
    cmp     x21, #0
    b.gt    .Lnorm_den_positive

    // Denominator is negative: negate both num and den
    neg     x20, x20
    neg     x21, x21

.Lnorm_den_positive:
    // Compute GCD of |num| and |den|
    mov     x0, x20
    bl      abs64
    mov     x1, x21
    bl      gcd                 // x0 = gcd(|num|, den)

    // Divide both by GCD
    mov     x1, x0              // x1 = gcd
    sdiv    x20, x20, x1        // num /= gcd
    sdiv    x21, x21, x1        // den /= gcd

    // Store normalized values
    stp     x20, x21, [x19]

.Lnorm_done:
    ldr     x21, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// ============================================================================
// rat_new: Create a new rational from numerator and denominator
// ============================================================================
// Creates a normalized rational number.
//
// Input:
//   x0 = numerator (int64_t)
//   x1 = denominator (int64_t)
//   x2 = pointer to output rational (16 bytes)
// Output:
//   Rational at x2 is initialized and normalized
// ============================================================================
rat_new:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, x2             // Save output pointer

    // Store numerator and denominator
    stp     x0, x1, [x19]

    // Normalize
    mov     x0, x19
    bl      rat_normalize

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// ============================================================================
// rat_add: Add two rationals
// ============================================================================
// Computes: result = a + b
//
// Algorithm:
//   a/b + c/d = (a*d + b*c) / (b*d)
//
// Input:
//   x0 = pointer to first rational (a)
//   x1 = pointer to second rational (b)
//   x2 = pointer to output rational (result)
// Output:
//   result = a + b (normalized)
// ============================================================================
rat_add:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x19, x2             // Save output pointer

    // Load a = x20/x21, b = x22/x23
    ldp     x20, x21, [x0]      // x20 = a_num, x21 = a_den
    ldp     x22, x23, [x1]      // x22 = b_num, x23 = b_den

    // Compute numerator: a_num * b_den + a_den * b_num
    mul     x0, x20, x23        // a_num * b_den
    mul     x1, x21, x22        // a_den * b_num
    add     x24, x0, x1         // result_num

    // Compute denominator: a_den * b_den
    mul     x1, x21, x23        // result_den

    // Create result rational
    mov     x0, x24             // numerator
    mov     x2, x19             // output pointer
    bl      rat_new

    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// ============================================================================
// rat_sub: Subtract two rationals
// ============================================================================
// Computes: result = a - b
//
// Input:
//   x0 = pointer to first rational (a)
//   x1 = pointer to second rational (b)
//   x2 = pointer to output rational (result)
// Output:
//   result = a - b (normalized)
// ============================================================================
rat_sub:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x19, x2             // Save output pointer

    // Load a = x20/x21, b = x22/x23
    ldp     x20, x21, [x0]      // x20 = a_num, x21 = a_den
    ldp     x22, x23, [x1]      // x22 = b_num, x23 = b_den

    // Compute numerator: a_num * b_den - a_den * b_num
    mul     x0, x20, x23        // a_num * b_den
    mul     x1, x21, x22        // a_den * b_num
    sub     x24, x0, x1         // result_num

    // Compute denominator: a_den * b_den
    mul     x1, x21, x23        // result_den

    // Create result rational
    mov     x0, x24             // numerator
    mov     x2, x19             // output pointer
    bl      rat_new

    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// ============================================================================
// rat_mul: Multiply two rationals
// ============================================================================
// Computes: result = a * b
//
// Algorithm:
//   (a/b) * (c/d) = (a*c) / (b*d)
//
// Input:
//   x0 = pointer to first rational (a)
//   x1 = pointer to second rational (b)
//   x2 = pointer to output rational (result)
// Output:
//   result = a * b (normalized)
// ============================================================================
rat_mul:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x19, x2             // Save output pointer

    // Load a = x20/x21, b = x22/x23
    ldp     x20, x21, [x0]      // x20 = a_num, x21 = a_den
    ldp     x22, x23, [x1]      // x22 = b_num, x23 = b_den

    // Compute numerator: a_num * b_num
    mul     x24, x20, x22

    // Compute denominator: a_den * b_den
    mul     x1, x21, x23

    // Create result rational
    mov     x0, x24             // numerator
    mov     x2, x19             // output pointer
    bl      rat_new

    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// ============================================================================
// rat_div: Divide two rationals
// ============================================================================
// Computes: result = a / b
//
// Algorithm:
//   (a/b) / (c/d) = (a/b) * (d/c) = (a*d) / (b*c)
//
// Input:
//   x0 = pointer to first rational (a)
//   x1 = pointer to second rational (b)
//   x2 = pointer to output rational (result)
// Output:
//   result = a / b (normalized)
// ============================================================================
rat_div:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]

    mov     x19, x2             // Save output pointer

    // Load a = x20/x21, b = x22/x23
    ldp     x20, x21, [x0]      // x20 = a_num, x21 = a_den
    ldp     x22, x23, [x1]      // x22 = b_num, x23 = b_den

    // Compute numerator: a_num * b_den
    mul     x24, x20, x23

    // Compute denominator: a_den * b_num
    mul     x1, x21, x22

    // Create result rational
    mov     x0, x24             // numerator
    mov     x2, x19             // output pointer
    bl      rat_new

    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// ============================================================================
// rat_neg: Negate a rational
// ============================================================================
// Computes: result = -a
//
// Input:
//   x0 = pointer to rational (a)
//   x1 = pointer to output rational (result)
// Output:
//   result = -a
// ============================================================================
rat_neg:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, x1             // Save output pointer

    // Load numerator and denominator
    ldp     x0, x1, [x0]        // x0 = num, x1 = den

    // Negate numerator
    neg     x0, x0

    // Create result rational
    mov     x2, x19             // output pointer
    bl      rat_new

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// ============================================================================
// rat_is_zero: Check if rational is zero
// ============================================================================
// Input:
//   x0 = pointer to rational
// Output:
//   x0 = 1 if zero, 0 otherwise
// ============================================================================
rat_is_zero:
    ldr     x0, [x0]            // Load numerator
    cmp     x0, #0
    cset    x0, eq              // Set x0 = 1 if equal, 0 otherwise
    ret

// ============================================================================
// rat_is_negative: Check if rational is negative
// ============================================================================
// Input:
//   x0 = pointer to rational
// Output:
//   x0 = 1 if negative, 0 otherwise
// ============================================================================
rat_is_negative:
    ldr     x0, [x0]            // Load numerator
    cmp     x0, #0
    cset    x0, lt              // Set x0 = 1 if less than, 0 otherwise
    ret

// ============================================================================
// rat_is_positive: Check if rational is positive
// ============================================================================
// Input:
//   x0 = pointer to rational
// Output:
//   x0 = 1 if positive, 0 otherwise
// ============================================================================
rat_is_positive:
    ldr     x0, [x0]            // Load numerator
    cmp     x0, #0
    cset    x0, gt              // Set x0 = 1 if greater than, 0 otherwise
    ret

// ============================================================================
// rat_is_integer: Check if rational is an integer
// ============================================================================
// A rational is an integer if denominator == 1 (after normalization)
//
// Input:
//   x0 = pointer to rational
// Output:
//   x0 = 1 if integer, 0 otherwise
// ============================================================================
rat_is_integer:
    ldr     x1, [x0, #8]        // Load denominator
    cmp     x1, #1
    cset    x0, eq              // Set x0 = 1 if equal, 0 otherwise
    ret

// ============================================================================
// rat_to_int: Convert rational to integer (if it is one)
// ============================================================================
// Input:
//   x0 = pointer to rational
// Output:
//   x0 = integer value (if denominator == 1)
//   x0 = 0 (if not an integer)
// ============================================================================
rat_to_int:
    ldp     x1, x2, [x0]        // x1 = num, x2 = den
    cmp     x2, #1
    csel    x0, x1, xzr, eq     // if den == 1, return num, else 0
    ret

// ============================================================================
// rat_compare: Compare two rationals
// ============================================================================
// Returns -1 if a < b, 0 if a == b, 1 if a > b
//
// Algorithm:
//   Compare a/b with c/d by cross-multiplying: a*d vs b*c
//
// Input:
//   x0 = pointer to first rational (a)
//   x1 = pointer to second rational (b)
// Output:
//   x0 = -1 if a < b, 0 if a == b, 1 if a > b
// ============================================================================
rat_compare:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp

    // Load a = x2/x3, b = x4/x5
    ldp     x2, x3, [x0]        // x2 = a_num, x3 = a_den
    ldp     x4, x5, [x1]        // x4 = b_num, x5 = b_den

    // Compute a_num * b_den
    mul     x6, x2, x5

    // Compute a_den * b_num
    mul     x7, x3, x4

    // Compare
    cmp     x6, x7
    cset    x0, gt              // x0 = 1 if x6 > x7
    cneg    x0, x0, lt          // x0 = -1 if x6 < x7

    ldp     x29, x30, [sp], #32
    ret

// ============================================================================
// rat_print: Print a rational number (for debugging)
// ============================================================================
// Prints rational in format "num/den" to stdout
//
// Input:
//   x0 = pointer to rational
// ============================================================================
rat_print:
    stp     x29, x30, [sp, #-48]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    str     x21, [sp, #32]

    mov     x19, x0             // Save pointer

    // Load numerator and denominator
    ldp     x20, x21, [x19]     // x20 = num, x21 = den

    // Print numerator
    mov     x0, x20
    bl      print_int64

    // Print '/'
    mov     x0, #'/'
    bl      print_char

    // Print denominator
    mov     x0, x21
    bl      print_int64

    ldr     x21, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #48
    ret

// ============================================================================
// print_char: Print a single character
// ============================================================================
print_char:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp

    // Store character on stack
    strb    w0, [sp, #16]

    // Write syscall
    mov     x0, #1              // stdout
    add     x1, sp, #16         // buffer (char on stack)
    mov     x2, #1              // length
    mov     w16, #4             // write syscall
    orr     w16, w16, #0x2000000
    svc     #0x80

    ldp     x29, x30, [sp], #32
    ret

// ============================================================================
// print_int64: Print a signed 64-bit integer
// ============================================================================
print_int64:
    stp     x29, x30, [sp, #-64]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    str     x21, [sp, #32]

    mov     x19, x0             // Save number

    // Handle negative numbers
    cmp     x19, #0
    b.ge    .Lprint_positive

    // Print '-'
    mov     x0, #'-'
    bl      print_char
    neg     x19, x19            // Make positive

.Lprint_positive:
    // Handle zero specially
    cbnz    x19, .Lprint_nonzero
    mov     x0, #'0'
    bl      print_char
    b       .Lprint_int_done

.Lprint_nonzero:
    // Use buffer on stack to build string in reverse
    add     x20, sp, #48        // Buffer position (16 bytes available)
    mov     x21, x20            // Save start

.Lprint_digit_loop:
    cbz     x19, .Lprint_reverse

    // Get digit: n % 10
    mov     x0, x19
    mov     x1, #10
    udiv    x2, x0, x1
    msub    x3, x2, x1, x0      // x3 = n % 10

    // Convert to ASCII and store
    add     w3, w3, #'0'
    strb    w3, [x20], #1

    // n /= 10
    mov     x19, x2
    b       .Lprint_digit_loop

.Lprint_reverse:
    // Now print characters in reverse order
    sub     x20, x20, #1        // Back to last character

.Lprint_rev_loop:
    cmp     x20, x21
    b.lt    .Lprint_int_done

    ldrb    w0, [x20]
    bl      print_char

    sub     x20, x20, #1
    b       .Lprint_rev_loop

.Lprint_int_done:
    ldr     x21, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #64
    ret

// ============================================================================
// print_string: Print a null-terminated string
// ============================================================================
print_string:
    stp     x29, x30, [sp, #-32]!
    mov     x29, sp
    str     x19, [sp, #16]

    mov     x19, x0             // Save string pointer

    // Calculate length
    mov     x1, x0
.Lstrlen_loop:
    ldrb    w2, [x1]
    cbz     w2, .Lstrlen_done
    add     x1, x1, #1
    b       .Lstrlen_loop

.Lstrlen_done:
    sub     x2, x1, x19         // x2 = length

    // Write syscall
    mov     x0, #1              // stdout
    mov     x1, x19             // buffer
    mov     w16, #4             // write syscall
    orr     w16, w16, #0x2000000
    svc     #0x80

    ldr     x19, [sp, #16]
    ldp     x29, x30, [sp], #32
    ret

// ============================================================================
// Data section for test harness
// ============================================================================
.data
    .align 4

test_header:
    .asciz "=== Rational Arithmetic Library Test ===\n"
test_gcd:
    .asciz "Testing GCD: "
test_rat_new:
    .asciz "Testing rat_new: "
test_rat_add:
    .asciz "Testing rat_add: "
test_rat_sub:
    .asciz "Testing rat_sub: "
test_rat_mul:
    .asciz "Testing rat_mul: "
test_rat_div:
    .asciz "Testing rat_div: "
test_rat_neg:
    .asciz "Testing rat_neg: "
test_rat_is_zero:
    .asciz "Testing rat_is_zero: "
test_rat_is_neg:
    .asciz "Testing rat_is_negative: "
test_rat_is_int:
    .asciz "Testing rat_is_integer: "
test_rat_compare:
    .asciz "Testing rat_compare: "
test_equals:
    .asciz " = "
test_newline:
    .asciz "\n"
test_plus:
    .asciz " + "
test_minus:
    .asciz " - "
test_times:
    .asciz " * "
test_divide:
    .asciz " / "
test_pass:
    .asciz "PASS\n"
test_fail:
    .asciz "FAIL\n"

.bss
    .align 8
rat_a:
    .space 16
rat_b:
    .space 16
rat_c:
    .space 16
rat_result:
    .space 16

// ============================================================================
// Test harness
// ============================================================================
.text

_test_rational_main:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // Print header
    load_addr x0, test_header
    bl      print_string

    // Test 1: GCD
    load_addr x0, test_gcd
    bl      print_string

    mov     x0, #48
    mov     x1, #18
    bl      gcd
    // Expected: 6
    cmp     x0, #6
    b.eq    .Ltest1_pass
    load_addr x0, test_fail
    bl      print_string
    b       .Ltest2
.Ltest1_pass:
    load_addr x0, test_pass
    bl      print_string

    // Test 2: rat_new (3/4)
.Ltest2:
    load_addr x0, test_rat_new
    bl      print_string

    mov     x0, #3
    mov     x1, #4
    load_addr x2, rat_a
    bl      rat_new

    load_addr x0, rat_a
    bl      rat_print
    load_addr x0, test_newline
    bl      print_string

    // Test 3: rat_add (1/2 + 1/3 = 5/6)
.Ltest3:
    load_addr x0, test_rat_add
    bl      print_string

    mov     x0, #1
    mov     x1, #2
    load_addr x2, rat_a
    bl      rat_new

    mov     x0, #1
    mov     x1, #3
    load_addr x2, rat_b
    bl      rat_new

    load_addr x0, rat_a
    bl      rat_print
    load_addr x0, test_plus
    bl      print_string
    load_addr x0, rat_b
    bl      rat_print
    load_addr x0, test_equals
    bl      print_string

    load_addr x0, rat_a
    load_addr x1, rat_b
    load_addr x2, rat_result
    bl      rat_add

    load_addr x0, rat_result
    bl      rat_print
    load_addr x0, test_newline
    bl      print_string

    // Test 4: rat_sub (3/4 - 1/2 = 1/4)
.Ltest4:
    load_addr x0, test_rat_sub
    bl      print_string

    mov     x0, #3
    mov     x1, #4
    load_addr x2, rat_a
    bl      rat_new

    mov     x0, #1
    mov     x1, #2
    load_addr x2, rat_b
    bl      rat_new

    load_addr x0, rat_a
    bl      rat_print
    load_addr x0, test_minus
    bl      print_string
    load_addr x0, rat_b
    bl      rat_print
    load_addr x0, test_equals
    bl      print_string

    load_addr x0, rat_a
    load_addr x1, rat_b
    load_addr x2, rat_result
    bl      rat_sub

    load_addr x0, rat_result
    bl      rat_print
    load_addr x0, test_newline
    bl      print_string

    // Test 5: rat_mul (2/3 * 3/4 = 1/2)
.Ltest5:
    load_addr x0, test_rat_mul
    bl      print_string

    mov     x0, #2
    mov     x1, #3
    load_addr x2, rat_a
    bl      rat_new

    mov     x0, #3
    mov     x1, #4
    load_addr x2, rat_b
    bl      rat_new

    load_addr x0, rat_a
    bl      rat_print
    load_addr x0, test_times
    bl      print_string
    load_addr x0, rat_b
    bl      rat_print
    load_addr x0, test_equals
    bl      print_string

    load_addr x0, rat_a
    load_addr x1, rat_b
    load_addr x2, rat_result
    bl      rat_mul

    load_addr x0, rat_result
    bl      rat_print
    load_addr x0, test_newline
    bl      print_string

    // Test 6: rat_div (1/2 / 1/3 = 3/2)
.Ltest6:
    load_addr x0, test_rat_div
    bl      print_string

    mov     x0, #1
    mov     x1, #2
    load_addr x2, rat_a
    bl      rat_new

    mov     x0, #1
    mov     x1, #3
    load_addr x2, rat_b
    bl      rat_new

    load_addr x0, rat_a
    bl      rat_print
    load_addr x0, test_divide
    bl      print_string
    load_addr x0, rat_b
    bl      rat_print
    load_addr x0, test_equals
    bl      print_string

    load_addr x0, rat_a
    load_addr x1, rat_b
    load_addr x2, rat_result
    bl      rat_div

    load_addr x0, rat_result
    bl      rat_print
    load_addr x0, test_newline
    bl      print_string

    // Test 7: rat_neg (-3/4)
.Ltest7:
    load_addr x0, test_rat_neg
    bl      print_string

    mov     x0, #3
    mov     x1, #4
    load_addr x2, rat_a
    bl      rat_new

    load_addr x0, rat_a
    load_addr x1, rat_result
    bl      rat_neg

    load_addr x0, rat_result
    bl      rat_print
    load_addr x0, test_newline
    bl      print_string

    // Test 8: rat_is_zero
.Ltest8:
    load_addr x0, test_rat_is_zero
    bl      print_string

    mov     x0, #0
    mov     x1, #1
    load_addr x2, rat_a
    bl      rat_new

    load_addr x0, rat_a
    bl      rat_is_zero

    cmp     x0, #1
    b.eq    .Ltest8_pass
    load_addr x0, test_fail
    bl      print_string
    b       .Ltest9
.Ltest8_pass:
    load_addr x0, test_pass
    bl      print_string

    // Test 9: rat_is_integer (4/1 is integer)
.Ltest9:
    load_addr x0, test_rat_is_int
    bl      print_string

    mov     x0, #4
    mov     x1, #1
    load_addr x2, rat_a
    bl      rat_new

    load_addr x0, rat_a
    bl      rat_is_integer

    cmp     x0, #1
    b.eq    .Ltest9_pass
    load_addr x0, test_fail
    bl      print_string
    b       .Ltest_done
.Ltest9_pass:
    load_addr x0, test_pass
    bl      print_string

.Ltest_done:
    // Exit
    mov     x0, #0
    mov     w16, #1
    orr     w16, w16, #0x2000000
    svc     #0x80
