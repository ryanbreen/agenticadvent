# Day 5: Supply Stacks

## Problem Summary

A giant cargo crane moves crates between stacks. The input consists of two parts:
1. A visual diagram showing the initial stack configuration
2. A series of move instructions

**Part 1 (CrateMover 9000)**: Move crates one at a time (reverses order when moving multiple).

**Part 2 (CrateMover 9001)**: Move multiple crates at once (preserves order).

Output the letters of the top crate on each stack concatenated together.

## Input Format

Visual diagram with crate letters at positions 1, 5, 9, ... followed by move instructions:
```
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
```

## Algorithmic Approach

### Parsing

1. Split input on blank line to separate diagram from moves
2. Parse diagram bottom-up:
   - Crate letters appear at column positions `1 + 4*i` (for stack index i)
   - Build stacks with index 0 as bottom
3. Parse moves with regex: `move (\d+) from (\d+) to (\d+)`

### Part 1: One at a Time

```
for each move (count, from, to):
    repeat count times:
        pop from source, push to destination
```

This naturally reverses the order of moved crates.

### Part 2: All at Once

```
for each move (count, from, to):
    slice top N crates from source
    append slice to destination
```

This preserves the original order.

### Time Complexity

- O(n * m) where n = number of moves, m = average crates per move
- Part 2 can be O(n) with efficient slice operations

### Space Complexity

- O(s * h) where s = number of stacks, h = max stack height

## Key Insight

The difference between Part 1 and Part 2 is just whether moving multiple crates reverses their order. Part 1 simulates true stack behavior (LIFO), while Part 2 treats the crane as moving a slice.

## Programming Techniques Highlighted

- **Stack operations**: push, pop, slice
- **String parsing**: Fixed-position extraction from visual diagram
- **Regular expressions**: Extracting numbers from move instructions
- **Deep copying**: Running both parts independently

## Language-Specific Notes

### Fast Implementations (< 10ms)
- **Rust**: `drain()` for efficient slice operations
- **C++**: `std::vector` with iterator ranges
- **Zig**: Fixed-size arrays with manual indexing
- **ARM64**: Direct memory manipulation
- **C**: Array-based stacks with index tracking

### Scripting Languages (17-28ms)
- **Perl**: `splice` for efficient multi-element removal
- **Bash**: AWK with string manipulation
- **Python**: List slicing with `[-count:]`
- **Common Lisp**: `subseq` and `nconc` for list operations

### Interpreted/VM Languages (47-74ms)
- **Node.js**: `splice()` for array mutation
- **Ruby**: `pop(count)` returns multiple elements
- **Java**: `subList()` with `addAll()`
- **PHP**: `array_slice()` and `array_merge()`
- **Go**: Slice operations with append

### Heavy Runtimes (400-2600ms)
- **Clojure**: Immutable vectors with `subvec` and `into`
- **ColdFusion**: Array operations with pop/push loops

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Rust        | 5.6          | 1.9         |
| C++         | 6.2          | 1.9         |
| Zig         | 6.4          | 1.9         |
| ARM64 asm   | 6.9          | 1.9         |
| C           | 8.1          | 1.9         |
| Perl        | 16.9         | 5.7         |
| Bash        | 17.9         | 6.7         |
| Python      | 23.8         | 15.2        |
| Common Lisp | 27.4         | 43.5        |
| Node.js     | 47.5         | 39.9        |
| Ruby        | 57.7         | 28.1        |
| Java        | 60.4         | 47.7        |
| PHP         | 62.1         | 26.0        |
| Go          | 73.5         | 27.9        |
| Clojure     | 418.2        | 133.3       |
| ColdFusion  | 2,577.5      | 1,128.1     |

## Answers

- Part 1: GFTNRBZPF
- Part 2: VRQWPDSGP
