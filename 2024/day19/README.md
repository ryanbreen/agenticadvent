# Day 19: Linen Layout

## Problem Summary

You're at an onsen (Japanese hot spring) and need to help arrange towels. Each towel has a pattern of colored stripes (white, blue, black, red, green), and you need to determine which design patterns can be created by concatenating available towel patterns.

**Part 1:** Count how many designs can be formed by combining the available towel patterns (each pattern can be reused unlimited times).

**Part 2:** Sum the total number of different ways each design can be formed from the towel patterns.

### Input Format
- First line: comma-separated list of available towel patterns
- Blank line
- Remaining lines: design patterns to check

## Algorithmic Approach

### Part 1: Feasibility Check

This is a classic **string segmentation** problem, which can be solved using dynamic programming.

**Key Insight:** A design can be formed at position `i` if:
1. Some towel pattern matches at position `i`, AND
2. The remaining substring (after the pattern) can also be formed

**DP Formulation:**
- `dp[i]` = true if `design[i:]` can be formed from patterns
- Base case: `dp[len] = true` (empty suffix is always achievable)
- Transition: `dp[i] = true` if any pattern matches at `i` and `dp[i + pattern_length]` is true

### Part 2: Counting Ways

Instead of boolean reachability, we count the number of ways.

**DP Formulation:**
- `dp[i]` = number of ways to form `design[i:]`
- Base case: `dp[len] = 1` (one way to form empty string)
- Transition: `dp[i] = sum(dp[i + plen])` for all patterns matching at position `i`

### Complexity

**Time Complexity:** O(D × L × P × M)
- D = number of designs
- L = average design length
- P = number of patterns
- M = average pattern length (for string comparison)

**Space Complexity:** O(L)
- Memoization table size is proportional to design length

## Programming Techniques Highlighted

### Data Structures
- **Hash Map / Dictionary**: For memoization of DP states
- **String slicing/comparison**: Core operation for pattern matching

### Concepts
- **Dynamic Programming**: Classic bottom-up or top-down with memoization
- **String Segmentation**: Classic DP problem (similar to Word Break)
- **Memoization**: Avoiding redundant computation

### Mathematical Properties
- Part 2 can produce very large numbers (trillions), requiring 64-bit integers
- The number of ways grows exponentially in the worst case

## Language-Specific Notes

### Fast Performers
- **ARM64 Assembly (24ms)**: Hand-optimized bottom-up DP with efficient string matching
- **Zig (28ms)**: Excellent hash map performance, minimal overhead
- **C++ (41ms)**: `unordered_map` provides fast memoization

### Memory Considerations
- **Java (301MB)**: JVM overhead plus HashMap with boxed integers
- **Clojure (833MB)**: JVM-based with immutable data structures
- **ColdFusion (1GB+)**: Engine overhead

### Big Integer Handling
Part 2 produces values up to ~10^15, which requires:
- **C/C++/Rust/Zig/Go**: Native 64-bit integers (`int64_t`, `u64`, `int64`)
- **Java**: `long` type
- **JavaScript**: `BigInt` (for safety, though `Number` works for this input)
- **Python/Ruby**: Native arbitrary precision
- **Bash**: `bc` for arbitrary precision arithmetic
- **PHP/Perl**: Native integers handle this range

### Performance Notes
- **Bash (250+ seconds)**: Shell scripting overhead makes this impractical
- **Ruby (1.2s)**: Hash operations and method dispatch overhead
- **Scripting languages**: 10-100x slower than compiled languages

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 24.2         | 1.9         |
| Zig         | 27.6         | 1.9         |
| C++         | 41.2         | 1.9         |
| C           | 50.4         | 1.9         |
| Go          | 54.3         | 5.9         |
| Rust        | 59.1         | 1.9         |
| Node.js     | 202.9        | 50.0        |
| Java        | 223.6        | 301.1       |
| Common Lisp | 278.1        | 48.3        |
| PHP         | 630.6        | 24.5        |
| Python      | 817.7        | 18.4        |
| Clojure     | 849.8        | 833.1       |
| Perl        | 880.2        | 20.8        |
| Ruby        | 1,224.2      | 28.8        |
| ColdFusion  | 6,043.4      | 1,064.8     |
| Bash        | 250,593.4    | 7.8         |

## Answers

- **Part 1:** 327
- **Part 2:** 772696486795255
