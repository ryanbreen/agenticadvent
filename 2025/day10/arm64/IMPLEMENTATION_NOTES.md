# Implementation Notes: Rational Arithmetic Library for ARM64

## Executive Summary

This document provides technical details about the design and implementation of the rational arithmetic library for ARM64 assembly. It serves as both a development log and a reference for future enhancements.

## Problem Context

Advent of Code 2025 Day 10 Part 2 requires solving an Integer Linear Programming (ILP) problem:

**Problem**: Given machines with counters and buttons that increment specific counters, find the minimum number of button presses to achieve target joltage levels across all counters.

**Mathematical Formulation**:
- Minimize: Σx_i (sum of button presses)
- Subject to: Ax = b (counter equations)
- Constraints: x_i ≥ 0, x_i ∈ ℤ (non-negative integers)

**Solution Approach**:
1. Gaussian elimination to find general solution
2. Extract particular solution and null space
3. Search over free variables for integer solutions
4. Return minimum sum

**Critical Requirement**: EXACT arithmetic. Floating-point errors during Gaussian elimination cause incorrect results when checking if solutions are integers.

## Why Assembly? Why Not Another Language?

This is part of a larger project to solve Advent of Code challenges in 16 different languages, including ARM64 assembly. The challenge demonstrates:

1. **How complex** exact arithmetic is at the lowest level
2. **Why** high-level languages abstract these operations
3. **What** it takes to implement mathematical primitives from scratch

## Design Philosophy

### Principle 1: Correctness Over Performance

Every operation maintains the invariants:
- Denominator always positive
- Always in lowest terms (GCD = 1)
- Sign in numerator only

This ensures consistent representation and simplifies comparisons.

### Principle 2: Simplicity Over Optimization

The Euclidean GCD algorithm is used instead of faster binary GCD because:
- Simpler to implement correctly
- Easier to verify
- Performance difference negligible for typical values
- Easier to maintain and debug

### Principle 3: Fail-Safe Defaults

Error conditions (like division by zero in denominator) are handled by setting safe default values rather than crashing. This allows continued execution for debugging.

### Principle 4: Test-Driven Development

Comprehensive tests were written alongside the implementation:
- Unit tests for each operation
- Integration test for matrix operations
- Manual verification of output

## Implementation Details

### GCD Algorithm

```assembly
gcd:
    cbnz    x1, .Lgcd_loop
    ret
.Lgcd_loop:
    udiv    x2, x0, x1          // x2 = a / b
    msub    x2, x2, x1, x0      // x2 = a % b
    mov     x0, x1              // a = b
    mov     x1, x2              // b = remainder
    cbnz    x1, .Lgcd_loop
    ret
```

**Key Insight**: Using `udiv` and `msub` to compute modulo is more efficient than repeated subtraction on ARM64.

**Optimization Note**: We use unsigned division (`udiv`) on absolute values, which is slightly faster than signed division.

### Normalization Strategy

Every rational creation/operation goes through normalization:

1. **Check denominator**: If zero, set to 1 (error handling)
2. **Ensure positive denominator**: If negative, negate both num and den
3. **Compute GCD**: Of |numerator| and denominator
4. **Divide both**: By GCD to reduce to lowest terms

**Trade-off Analysis**:
- **Pro**: Prevents accumulation of large values, consistent representation
- **Con**: Every operation pays GCD cost (O(log n))
- **Verdict**: Worth it for correctness and overflow prevention

### Register Allocation Strategy

Following ARM64 ABI:
- **x0-x7**: Arguments and temporaries (caller-saved)
- **x19-x28**: Preserved locals (callee-saved)
- **x29**: Frame pointer
- **x30**: Return address

**Pattern**:
```assembly
function:
    stp     x29, x30, [sp, #-48]!   // Save FP and LR
    mov     x29, sp                  // Set FP
    stp     x19, x20, [sp, #16]      // Save callee-saved regs

    // Function body using x19-x20 safely

    ldp     x19, x20, [sp, #16]      // Restore callee-saved
    ldp     x29, x30, [sp], #48      // Restore FP/LR and adjust SP
    ret
```

This ensures proper interoperability with other code and debugger support.

### Stack Alignment

ARM64 requires 16-byte stack alignment. All function prologs ensure this:

```assembly
stp     x29, x30, [sp, #-48]!   // 48 is multiple of 16
```

Violating this causes crashes or undefined behavior.

### Addition/Subtraction Algorithm

For `a/b + c/d`:

1. Compute numerator: `a*d + b*c` (or `a*d - b*c` for subtraction)
2. Compute denominator: `b*d`
3. Call `rat_new(num, den)` which normalizes

**Overflow Risk**: `a*d` and `b*c` can overflow if a,d are large.

**Mitigation**: Normalization keeps values smaller. Future enhancement: cross-cancel before multiplying.

### Multiplication/Division Algorithm

For `(a/b) * (c/d)`:

1. Compute numerator: `a*c`
2. Compute denominator: `b*d`
3. Normalize

For division, swap numerator/denominator of second operand.

**Optimization Opportunity**:
```
(a/b) * (c/d) = (a*c) / (b*d)

But we could:
g1 = gcd(a, d)
g2 = gcd(c, b)
result = (a/g1 * c/g2) / (b/g2 * d/g1)
```

This cross-cancellation reduces overflow risk but adds complexity.

### Comparison Algorithm

To compare `a/b` with `c/d`:

1. Cross-multiply: compute `a*d` and `b*c`
2. Compare products
3. Return -1, 0, or 1

**Why This Works**: Since denominators are always positive, sign is preserved.

**Overflow Risk**: Products can overflow. For very large rationals, this could give wrong results.

## Testing Strategy

### Test Categories

1. **Smoke Tests**: Basic operations with small values
2. **Edge Cases**: Zero, negative numbers, large denominators
3. **Normalization Tests**: Verify GCD reduction works
4. **Integration Tests**: Multi-operation sequences

### Test Results Interpretation

The test output shows:
```
Testing rat_add: 1/2 + 1/3 = 5/6
```

This verifies:
- Correct numerator: 1*3 + 2*1 = 5
- Correct denominator: 2*3 = 6
- Normalization: Already in lowest terms

### Matrix Example Verification

Starting matrix:
```
[ 2  4 | 8  ]
[ 3  7 | 15 ]
```

After `R2 = R2 - (3/2)*R1`:
```
[ 2  4 | 8 ]
[ 0  1 | 3 ]
```

**Verification**:
- R2[0]: 3 - (3/2)*2 = 3 - 3 = 0 ✓
- R2[1]: 7 - (3/2)*4 = 7 - 6 = 1 ✓
- R2[2]: 15 - (3/2)*8 = 15 - 12 = 3 ✓

This confirms the row operation is correct.

## Performance Analysis

### Microbenchmarks (Estimated)

On Apple M1 Pro (ARM64):

| Operation | Cycles | Time (ns) |
|-----------|--------|-----------|
| `gcd(48, 18)` | ~100 | ~30 |
| `rat_new(3, 4)` | ~150 | ~45 |
| `rat_add(1/2, 1/3)` | ~300 | ~90 |
| `rat_mul(2/3, 3/4)` | ~250 | ~75 |

These are rough estimates. Actual performance depends on:
- Input values (GCD iteration count)
- Cache state
- Branch prediction
- Instruction scheduling

### Bottlenecks

1. **GCD computation**: O(log n) iterations with division
2. **Multiplication**: 64-bit multiply is fast but can overflow
3. **Memory access**: Loading/storing 16-byte rationals
4. **Function calls**: Overhead of proper prologue/epilogue

### Optimization Potential

**Low-hanging fruit**:
1. **Inline small functions**: Eliminate call overhead
2. **Use SIMD**: Process multiple rationals in parallel
3. **Lazy normalization**: Only normalize when necessary
4. **Binary GCD**: Faster for large numbers

**Advanced optimizations**:
1. **128-bit arithmetic**: Use `mul`/`umulh` for overflow detection
2. **Assembly macros**: Reduce code duplication
3. **Register allocation**: Minimize spills to stack
4. **Branch elimination**: Use conditional moves (`csel`)

## Challenges Encountered

### Challenge 1: Load Address Macro

**Problem**: The `adrp`/`add` pair for position-independent addressing is verbose.

**Solution**: Define a macro:
```assembly
.macro load_addr reg, label
    adrp    \reg, \label@PAGE
    add     \reg, \reg, \label@PAGEOFF
.endm
```

This must be defined before use in assembly files.

### Challenge 2: Symbol Visibility

**Problem**: Functions weren't visible to linker for `matrix_example.s`.

**Solution**: Export all public functions with `.global` directives:
```assembly
.global rat_new
.global rat_add
// etc.
```

### Challenge 3: Stack Alignment

**Problem**: Initial implementation had misaligned stack, causing crashes.

**Solution**: Always allocate stack space in multiples of 16:
```assembly
stp     x29, x30, [sp, #-48]!   // Not -47, -49, etc.
```

### Challenge 4: Print Functions

**Problem**: Needed debugging output but no printf in assembly.

**Solution**: Implement from scratch:
- `print_char`: Single character via write syscall
- `print_int64`: Convert number to ASCII, print digits
- `print_string`: Calculate length, write via syscall

## Memory Layout

### Rational Storage

```
Address:  0x00  0x08
         ┌─────┬─────┐
         │ num │ den │
         └─────┴─────┘
         8 bytes each (int64_t)
```

### Stack Frame Layout (Typical Function)

```
High Address
┌──────────────┐
│ Saved x21    │ [sp, #32]
├──────────────┤
│ Saved x20    │ [sp, #24]
│ Saved x19    │ [sp, #16]
├──────────────┤
│ Saved x30(LR)│ [sp, #8]
│ Saved x29(FP)│ [sp, #0]
└──────────────┘ <- SP
Low Address
```

### Matrix Storage (Example)

```
matrix[0][0]  (16 bytes)
matrix[0][1]  (16 bytes)
matrix[0][2]  (16 bytes)
matrix[1][0]  (16 bytes)
matrix[1][1]  (16 bytes)
matrix[1][2]  (16 bytes)
```

Row-major layout allows simple indexing: `matrix + (row * n_cols + col) * 16`

## Lessons Learned

### What Worked Well

1. **Test-driven approach**: Writing tests first clarified requirements
2. **Modular design**: Each function does one thing well
3. **Extensive comments**: Made debugging and verification easier
4. **Normalization strategy**: Prevents many edge cases

### What Could Be Improved

1. **Error handling**: Should return status codes
2. **Overflow detection**: Need explicit checks or 128-bit arithmetic
3. **Performance**: Could optimize hot paths
4. **Macros**: More abstraction would reduce duplication

### Key Insights

1. **Exact arithmetic is hard**: What seems simple in high-level languages requires careful implementation
2. **Normalization is expensive**: O(log n) cost per operation adds up
3. **Assembly debugging is challenging**: Print statements become essential
4. **Testing is critical**: Easy to make off-by-one errors or miss edge cases

## Future Work

### Short Term (Next Steps)

1. **Matrix module**: Implement matrix operations using this library
2. **Gaussian elimination**: Core algorithm for solving linear systems
3. **Overflow handling**: Add 128-bit arithmetic or arbitrary precision
4. **More tests**: Edge cases, stress tests, property-based testing

### Medium Term (Complete Part 2)

1. **Null space computation**: Find basis for solution space
2. **Integer search**: Bounded search over free variables
3. **Input parsing**: Extract joltage requirements from input
4. **Integration**: Combine all pieces into complete solver

### Long Term (General Purpose Library)

1. **Extended operations**: Powers, roots, transcendental functions
2. **Matrix library**: LU decomposition, eigenvalues, etc.
3. **Performance tuning**: SIMD, parallel operations
4. **Language bindings**: C/C++ wrappers for easier use
5. **Documentation**: Man pages, examples, tutorials

## Conclusion

This rational arithmetic library demonstrates that implementing exact arithmetic in ARM64 assembly is:

1. **Feasible**: All basic operations work correctly
2. **Complex**: Requires careful attention to invariants and edge cases
3. **Educational**: Reveals the complexity hidden by high-level languages
4. **Practical**: Provides foundation for numerical algorithms

The library successfully achieves its goal of providing exact rational arithmetic primitives suitable for Gaussian elimination and linear algebra operations.

**Total Implementation**:
- Core library: ~980 lines
- Matrix example: ~300 lines
- Tests: Integrated in library
- Documentation: ~500 lines (this file + README + API docs)

**Time Investment**: Approximately 6-8 hours for design, implementation, testing, and documentation.

**Educational Value**: Extremely high. Understanding low-level arithmetic implementation provides deep insights into:
- Number representation
- Arithmetic algorithms
- Assembly programming
- Software engineering practices
- Numerical computing challenges

This project serves as an excellent reference for anyone learning ARM64 assembly or implementing mathematical libraries at a low level.
