# Day 19: Aplenty

## Problem Summary

The Elves are organizing machine parts at Gear Island. Each part has four ratings: **x** (extremely cool), **m** (musical), **a** (aerodynamic), and **s** (shiny). Parts are processed through a series of workflows that ultimately **accept** or **reject** each part.

**Input format**: Workflows followed by a blank line, then parts. Workflows look like `px{a<2006:qkq,m>2090:A,rfg}` and parts look like `{x=787,m=2655,a=1222,s=2876}`.

## Part 1: Process Individual Parts

Each workflow contains rules with conditions (e.g., `x>10:dest`) and a default destination. Process each part starting from the `in` workflow:
- Evaluate rules in order until one matches
- Follow the destination (another workflow, `A` for accept, or `R` for reject)

**Goal**: Sum `x+m+a+s` for all accepted parts.

## Part 2: Count All Possible Combinations

Instead of processing individual parts, determine how many distinct combinations of ratings (each 1-4000) would be accepted.

**Goal**: Count all valid (x, m, a, s) combinations that lead to acceptance.

## Algorithmic Approach

### Part 1: Direct Simulation

Simple workflow traversal:
1. Parse workflows into a map of rules
2. For each part, start at "in" and follow rules until reaching A or R
3. Sum ratings of accepted parts

**Complexity**: O(P × W × R) where P = parts, W = average workflow chain length, R = average rules per workflow

### Part 2: Range Splitting

The key insight is that we can track **ranges** of values instead of individual combinations. Since there are 4000^4 ≈ 256 trillion possible combinations, we can't enumerate them directly.

**Algorithm**:
1. Start with ranges [1, 4000] for all four attributes
2. Process workflows recursively:
   - For conditional rules like `x<1000`:
     - **Split** the range: [1, 999] satisfies the condition, [1000, 4000] doesn't
     - Recursively process the matching part at the destination
     - Continue with the non-matching part to the next rule
   - For default rules: recursively process at the destination
3. At "A": return the product of all range sizes (number of combinations)
4. At "R": return 0

**Example**: If we reach acceptance with ranges x=[1,2000], m=[1,4000], a=[500,1000], s=[1,4000]:
- Combinations = 2000 × 4000 × 501 × 4000 = 16,032,000,000,000

**Complexity**: O(W × R) where each rule potentially splits ranges, but total states are bounded

### Key Data Structures

- **Workflows**: Map from name to list of rules
- **Rule**: (attribute, operator, value, destination) or just destination for default
- **Ranges**: Four (min, max) pairs, one per attribute

## Programming Techniques

1. **Parsing**: Regular expressions or string splitting for workflows and parts
2. **Recursion**: Natural fit for range splitting with branching paths
3. **Range Arithmetic**: Splitting ranges at condition boundaries
4. **Large Numbers**: Part 2 result (~143 trillion) requires 64-bit integers

## Language Notes

- **Fast performers**: ARM64, Zig, Rust, C, C++ - simple data structures, efficient recursion
- **64-bit requirement**: All languages need to handle Part 2's large result
  - PHP: Uses GMP for arbitrary precision
  - Bash: Uses `bc` for large number arithmetic
  - ColdFusion: Uses `precisionEvaluate()` for BigDecimal
- **Bash is slow**: The range-splitting recursion with string manipulation and `bc` calls makes Bash exceptionally slow on this problem (~8 seconds)

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64       | 7.6          | 1.9         |
| Zig         | 7.7          | 1.9         |
| Rust        | 8.3          | 1.9         |
| C           | 8.5          | 1.9         |
| C++         | 8.5          | 1.9         |
| Go          | 9.5          | 5.0         |
| Perl        | 17.9         | 5.7         |
| Python      | 34.8         | 16.3        |
| Node.js     | 56.2         | 42.2        |
| Common Lisp | 64.8         | 55.1        |
| PHP         | 70.1         | 26.9        |
| Java        | 74.9         | 49.7        |
| Ruby        | 77.4         | 29.2        |
| Clojure     | 493.9        | 134.6       |
| ColdFusion  | 2,927.9      | 1,089.4     |
| Bash        | 8,241.7      | 7.2         |

## Answers

- Part 1: **432788**
- Part 2: **142863718918201**
