# Day 8: Haunted Wasteland

## Problem Summary

You're navigating a desert using a map of labeled nodes and left/right (L/R) instructions. The map is a directed graph where each node has exactly two outgoing edges: one for "left" and one for "right".

**Input format:**
- Line 1: A string of L/R instructions (e.g., "LRLRLR...")
- Lines 3+: Node definitions in format `AAA = (BBB, CCC)` where BBB is the left neighbor and CCC is the right neighbor

## Part 1: Single Path Navigation

**Task:** Start at node `AAA` and follow the L/R instructions (cycling through them as needed) until you reach node `ZZZ`. Count the steps required.

**Algorithm:**
1. Parse the network into a hash map for O(1) lookups
2. Start at `AAA`, follow instructions using modulo to cycle
3. Count steps until reaching `ZZZ`

**Complexity:** O(S) where S is the number of steps to reach ZZZ

## Part 2: Parallel Ghost Navigation

**Task:** Start at ALL nodes ending with 'A' simultaneously. Follow the same L/R instructions for all current positions at once. Count steps until ALL positions end with 'Z' at the same time.

### Key Insight

Brute-force simulation would take astronomical time. The crucial insight is that the puzzle input has a special structure:

1. Each starting node (ending in 'A') reaches exactly one ending node (ending in 'Z')
2. The path from each 'A' to its 'Z' forms a regular cycle
3. The cycle length from each 'A' to its 'Z' is the same as the cycle length from 'Z' back to itself

This means we can find when each ghost reaches a 'Z' node independently, then compute when they ALL reach 'Z' nodes simultaneously using the **Least Common Multiple (LCM)**.

**Algorithm:**
1. Find all starting nodes (ending in 'A')
2. For each starting node, find the cycle length to reach a 'Z' node
3. Compute LCM of all cycle lengths

**Why LCM works:** If ghost A reaches 'Z' every 3 steps and ghost B reaches 'Z' every 4 steps, they both reach 'Z' at step 12 (LCM(3,4) = 12).

**Complexity:** O(N × L) where N is number of starting nodes and L is average cycle length

### Data Structures Used

- **Hash Map/Dictionary:** For O(1) node lookups by name
- **Arrays/Tuples:** For storing left/right neighbors
- **BigInt (some languages):** LCM result exceeds 32-bit integers

## Language Notes

### Systems Languages (ARM64, C, C++, Rust, Zig)
- Fastest implementations at 6-9ms
- Direct memory access and efficient hash maps
- ARM64 uses base-36 encoding to pack 3-char node names into integers

### Scripting Languages (Python, Ruby, Perl, PHP)
- 35-65ms range
- Python's built-in `gcd` function in `math` module
- Ruby has native `lcm` method on integers
- Perl requires explicit BigInt handling via `use bigint`

### JVM Languages (Java, Clojure)
- Java: 52ms, straightforward HashMap implementation
- Clojure: 420ms, functional approach with `reduce` for LCM

### Special Cases

**Big Integer Handling:**
The LCM result (9,606,140,307,013) exceeds 32-bit integer limits:
- JavaScript: Uses `BigInt` type with `n` suffix
- Java: `long` type suffices (64-bit)
- PHP: GMP functions (`gmp_gcd`, `gmp_mul`, `gmp_div_q`)
- Perl: `use bigint` pragma
- Bash: `bc` for arbitrary precision arithmetic

**ARM64 Assembly:**
- Encodes 3-character alphanumeric node names as base-36 integers
- Uses 64-bit arithmetic throughout for LCM calculation
- Manual hash table implementation with linear probing

**Bash:**
- Extremely slow (230 seconds!) due to:
  - Associative array lookups are slow
  - No native big integer support, shells out to `bc`
  - Each step requires string manipulation
- Uses `bc` for arbitrary-precision GCD/LCM calculation

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 5.9          | 1.9         |
| Zig         | 6.2          | 1.9         |
| C           | 6.6          | 1.8         |
| C++         | 8.3          | 1.9         |
| Rust        | 8.8          | 1.9         |
| Lisp        | 27.4         | 39.9        |
| Python      | 34.5         | 16.0        |
| Perl        | 41.9         | 12.3        |
| Go          | 48.3         | 27.6        |
| Node.js     | 49.2         | 43.9        |
| PHP         | 49.9         | 26.2        |
| Java        | 52.4         | 51.5        |
| Ruby        | 65.0         | 27.8        |
| Clojure     | 420.5        | 135.0       |
| ColdFusion* | ~2,500       | ~1,100      |
| Bash        | 230,070      | 2.1         |

*ColdFusion estimated from similar day patterns.

## Answers

- **Part 1:** 19241
- **Part 2:** 9606140307013
