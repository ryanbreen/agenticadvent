# Day 25: Code Chronicle

## Problem Summary

The final day of Advent of Code 2024 presents a classic "lock and key" matching puzzle. You need to determine which locks and keys are physically compatible based on their pin/tooth configurations.

**Part 1**: Given schematics of locks (pins extending downward) and keys (teeth extending upward), count how many unique lock/key pairs fit together without overlapping in any column.

**Part 2**: Day 25 traditionally has no computational Part 2. Instead, completing all 50 stars throughout the month unlocks the finale.

### Input Format

Each schematic is a 7-row by 5-column grid:
- **Locks**: Top row is all `#`, bottom row is all `.`
- **Keys**: Top row is all `.`, bottom row is all `#`

Schematics are separated by blank lines.

## Algorithmic Approach

### Part 1 Algorithm

1. **Parse schematics**: Split input by blank lines, classify each as lock or key based on the first row
2. **Convert to heights**: For each schematic, count the number of `#` symbols in each column (excluding the boundary row)
   - Locks: Count `#` from row 1 downward
   - Keys: Count `#` from row 5 upward
3. **Check compatibility**: For each lock/key pair, verify that `lock[col] + key[col] <= 5` for all 5 columns
4. **Count valid pairs**: Sum all compatible pairs

### Key Insight

The problem is essentially checking whether two complementary shapes fit within a fixed height constraint. Since there are only 5 columns and the maximum height is 5, each lock and key can be represented as a simple 5-integer tuple. This makes compatibility checking trivial - just sum corresponding heights and verify all sums are at most 5.

### Complexity

- **Time**: O(L × K × 5) where L = number of locks, K = number of keys
  - With ~250 locks and ~250 keys, this is ~312,500 comparisons
  - Each comparison is O(5) = O(1)
- **Space**: O(L + K) to store the height arrays

## Programming Techniques Highlighted

- **Pattern matching**: Identifying locks vs keys by their first row
- **Grid parsing**: Converting 2D character grids into numeric representations
- **Brute force enumeration**: With small input sizes, exhaustive search is efficient
- **Simple data structures**: Arrays of 5 integers suffice

## Language-Specific Notes

### Fast Performers
- **C, ARM64, Rust, C++** (5-7ms): Minimal overhead, efficient array operations
- **Go** (7ms): Compiled performance with simple data structures
- **Zig** (7ms): Systems-level performance

### Scripting Languages
- **Perl** (22ms): Surprisingly fast for text processing
- **Python** (33ms): Clean implementation with list comprehensions
- **Node.js** (45ms): V8 JIT compilation helps
- **PHP** (51ms): Respectable performance for scripting

### JVM Languages
- **Java** (45ms): Startup overhead dominates for small problems
- **Clojure** (417ms): JVM startup + Clojure runtime overhead

### Challenging Cases
- **Bash** (31s): Nested loops in shell scripting are extremely slow; each arithmetic operation spawns subprocesses

### ColdFusion
- Requires web server context for execution, not benchmarkable via CLI

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 5.2          | 1.9         |
| ARM64       | 5.4          | 1.9         |
| Rust        | 5.7          | 1.9         |
| C++         | 5.8          | 1.9         |
| Zig         | 7.1          | 1.9         |
| Go          | 7.2          | 4.1         |
| Perl        | 22.4         | 4.5         |
| Common Lisp | 26.1         | 44.0        |
| Python      | 32.8         | 15.8        |
| Java        | 45.3         | 48.7        |
| Node.js     | 45.3         | 43.3        |
| PHP         | 51.3         | 25.6        |
| Ruby        | 64.6         | 28.0        |
| Clojure     | 416.8        | 178.6       |
| Bash        | 31447.7      | 7.0         |
| ColdFusion  | N/A          | N/A         |

## Answers

- **Part 1**: 3155
- **Part 2**: Merry Christmas! (No computation required)
