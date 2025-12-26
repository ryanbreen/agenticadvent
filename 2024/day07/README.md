# Day 7: Bridge Repair

## Problem Summary

Engineers need help calibrating their equipment for a rope bridge repair. Young elephants have stolen all the operators from their calibration equations, and we need to determine which test values can be produced by placing operators between the given numbers.

**Input Format:** Each line contains a target value followed by a colon, then a list of numbers:
```
190: 10 19
3267: 81 40 27
292: 11 6 16 20
```

**Key Constraint:** Operators are evaluated **left-to-right**, not by standard precedence rules.

## Part 1: Addition and Multiplication

Using only `+` (add) and `*` (multiply) operators, determine which equations can be made true. Return the sum of target values for all solvable equations.

Example: `3267: 81 40 27` can be solved as:
- `81 + 40 * 27 = 121 * 27 = 3267` (left-to-right evaluation)
- `81 * 40 + 27 = 3240 + 27 = 3267`

## Part 2: Concatenation Operator

A third operator `||` (concatenation) is introduced, which joins digits:
- `12 || 345 = 12345`

Now more equations become solvable:
- `156: 15 6` → `15 || 6 = 156`
- `7290: 6 8 6 15` → `6 * 8 || 6 * 15 = 48 || 6 * 15 = 486 * 15 = 7290`

## Algorithmic Approach

### Brute Force Enumeration

For `n` numbers, there are `n-1` operator positions. The algorithm tries all possible operator combinations:

- **Part 1:** 2 operators → `2^(n-1)` combinations per equation
- **Part 2:** 3 operators → `3^(n-1)` combinations per equation

### Implementation Strategy

1. **Parse Input:** Extract target value and number list from each line
2. **Generate Combinations:** Use base-k counting (k=2 for Part 1, k=3 for Part 2)
3. **Evaluate Left-to-Right:** Process operators sequentially without precedence
4. **Early Termination:** Return true as soon as any combination matches target

### Concatenation Implementation

Two approaches:
1. **String-based:** Convert to strings, concatenate, convert back
2. **Mathematical:** `concat(a, b) = a * 10^(digits(b)) + b`

The mathematical approach is faster but requires digit counting.

### Complexity

- **Time:** O(E × k^N) where E = equations, k = operators, N = max numbers per equation
- **Space:** O(N) for operator array per equation

With ~850 equations and max ~12 numbers: up to 850 × 3^11 ≈ 150 million evaluations for Part 2.

## Programming Techniques Highlighted

- **Brute Force Search:** Exhaustive enumeration of operator combinations
- **Base-N Counting:** Using modulo/division to generate all k-ary combinations
- **Left-to-Right Evaluation:** Non-standard operator precedence
- **Large Integer Handling:** Results can exceed 32-bit limits

## Language-Specific Notes

### Performance Leaders
- **C (352ms):** Fast compiled code, efficient integer operations
- **Zig (363ms):** Modern systems language, comparable to C
- **Rust (476ms):** Safe systems programming with good performance
- **C++ (579ms):** Standard library overhead slightly slower than C

### Interpreted Languages
- **Common Lisp (4s):** Surprisingly fast for a dynamic language
- **PHP (6s):** Decent scripting performance
- **Python (9s):** Simple implementation, reasonable speed
- **Ruby (11s):** Slower but expressive
- **Perl (24s):** Heavy regex/string handling overhead

### JVM Languages
- **Java (1.5s):** JIT compilation helps, but high memory (1.3GB)
- **Clojure (17s):** Functional style adds overhead, massive memory use

### Special Cases
- **ARM64 Assembly (511ms):** Hand-optimized assembly, competitive with compiled languages
- **ColdFusion (43s):** Enterprise scripting, very slow for computation
- **Bash (>5 minutes):** Too slow for brute force due to interpreter overhead

## Benchmarks

| Language | Runtime (ms) | Memory (MB) |
|----------|-------------|-------------|
| C | 352 | 1.9 |
| Zig | 363 | 1.9 |
| Rust | 476 | 1.9 |
| ARM64 | 511 | 1.9 |
| C++ | 579 | 1.9 |
| Java | 1,486 | 1,274 |
| Node.js | 3,094 | 65 |
| Go | 3,416 | 85 |
| Common Lisp | 4,060 | 130 |
| PHP | 6,063 | 117 |
| Python | 9,385 | 16 |
| Ruby | 11,487 | 28 |
| Clojure | 17,571 | 1,320 |
| Perl | 24,006 | 196 |
| ColdFusion | 42,807 | 1,304 |
| Bash | >300,000* | ~10 |

\* Bash Part 1 takes ~5 minutes, Part 2 takes ~2+ hours. Too slow for practical benchmarking.

## Optimization Opportunities

1. **Pruning:** If intermediate result exceeds target, stop (all ops only increase values)
2. **Skip Part 2 for Part 1 matches:** If equation solved with +/*, don't recheck with ||
3. **Parallel processing:** Each equation is independent
4. **Memoization:** Cache intermediate results (limited benefit due to operator variations)

## Answers

- **Part 1:** 1038838357795
- **Part 2:** 254136560217241
