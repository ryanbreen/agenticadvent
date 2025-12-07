# Day 7: Laboratories

## Problem Summary

In this puzzle, you find yourself in a North Pole teleporter lab where a broken tachyon manifold needs analysis. A **tachyon beam** enters the manifold from a starting position `S` at the top and travels **downward** through a grid.

### Input Format
- A 2D grid of characters
- `S` marks the starting position (always in the first row)
- `.` represents empty space (beams pass through)
- `^` represents a **splitter** that stops the beam and emits two new beams to the left and right

### What We Compute
- **Part 1**: Count the total number of times any beam hits a splitter
- **Part 2**: Count the total number of distinct timelines created by a single particle taking "both paths" at each splitter (many-worlds interpretation)

---

## Part 1 Analysis

### What Does Part 1 Ask For?
Count the total number of **split events** as one or more tachyon beams travel through the manifold. When multiple beams arrive at the same column position, they merge (since we only track unique positions, not individual beams).

### Algorithm Overview
1. Find the starting column `S` in the first row
2. Track a **set** of active beam positions (columns)
3. Process row by row from top to bottom:
   - For each active beam position, check the cell in the current row
   - If it's a splitter (`^`), increment split count and add left/right positions
   - If it's empty (`.`), keep the beam at the same column
4. Continue until all beams exit the grid or no beams remain

### Key Data Structures
- **Set of integers**: Track unique column positions of active beams
- The set naturally handles beam merging (two beams at the same column become one entry)

---

## Part 2 Analysis

### How Does Part 2 Change the Problem?
Instead of counting split events, we now track **timelines**. Each time a particle hits a splitter, it doesn't just split the beam - it splits **time itself**. The particle exists in both the left and right path simultaneously, creating parallel timelines.

### Key Insight
This is fundamentally a **counting problem**, not a simulation. We don't need to track individual timelines - we need to track how many timelines exist at each grid position.

### Algorithm Modifications
Instead of a **set** (which treats multiple beams at the same position as one), we use a **map/dictionary** from column position to **count of timelines** at that position.

When timelines at position `col` hit a splitter:
- All `count` timelines branch left to `col-1`
- All `count` timelines also branch right to `col+1`
- Both destinations receive the full count (doubling)

When timelines converge at the same column (without splitting), their counts **add together** but they remain **distinct timelines** for future splits.

---

## Algorithmic Approach

### Key Insight
The solution leverages the observation that we only need to track **how many** timelines are at each column, not their individual histories. This reduces exponential complexity to linear per row.

Without this insight, you might try to simulate each timeline individually - which would explode to 2^N timelines for N splitters encountered.

### Data Structures

| Part | Structure | Purpose |
|------|-----------|---------|
| Part 1 | `Set<int>` | Track unique beam positions (merging) |
| Part 2 | `Map<int, bigint>` | Track timeline counts per position |

### Time Complexity
- **O(R * C)** where R = rows, C = columns
- Each row processes at most C positions
- Map/set operations are O(1) amortized

### Space Complexity
- **O(C)** for storing active positions/timelines at any row
- **O(R * C)** for storing the input grid

---

## Programming Techniques Highlighted

### Core Concepts Tested
1. **State Compression**: Tracking counts rather than individual entities
2. **Set vs Map Semantics**: Part 1 needs deduplication; Part 2 needs counting
3. **Big Integer Arithmetic**: Part 2 answer exceeds 64-bit integers in some scenarios
4. **Row-by-row Simulation**: Sweepline-style processing

### Mathematical Properties
- Each split doubles the number of timelines at that position
- Timeline counts grow exponentially with depth (2^splits along a path)
- The problem structure resembles Pascal's triangle in how counts propagate and combine

---

## Language-Specific Implementation Notes

### Languages Naturally Suited

**Python** (`solution.py`)
- Native arbitrary-precision integers handle Part 2 effortlessly
- `defaultdict(int)` provides clean syntax for timeline counting
- Sets and dicts work exactly as the algorithm requires

**JavaScript/Node.js** (`solution.js`)
- `BigInt` provides native big integer support with `1n` syntax
- `Map` and `Set` are first-class data structures
- Clean and readable implementation

### Languages Requiring Workarounds

**PHP** (`solution.php`)
- Must use GMP library (`gmp_init`, `gmp_add`, `gmp_strval`) for Part 2
- Requires explicit extension installation
- Verbose compared to Python/JavaScript

```php
$timelines[$start_col] = gmp_init(1);
$new_timelines[$col - 1] = gmp_add($new_timelines[$col - 1], $count);
```

**Bash** (`solution.sh`)
- Uses `bc` for arbitrary precision arithmetic
- Simulates maps with `"col:count"` space-separated strings
- Pattern matching for set membership: `[[ " $seen " =~ " $col " ]]`
- Extremely verbose but functional

**C** (`solution.c`)
- Uses `unsigned long long` (64-bit) - sufficient for this input
- Manual implementation of map-like behavior with parallel arrays
- Linear search through `TimelineMap` structure

```c
typedef struct {
    int col;
    unsigned long long count;
} Timeline;
```

**Go** (`solution.go`)
- Uses `uint64` which handles this input's answer
- `map[int]uint64` provides natural timeline tracking
- Clean implementation but would overflow on larger inputs

### Notable Differences

| Language | Part 2 Integer Type | Map/Set Available |
|----------|---------------------|-------------------|
| Python | Native bigint | Yes (dict, set) |
| JavaScript | BigInt | Yes (Map, Set) |
| Rust | u128 | Yes (HashMap, HashSet) |
| Go | uint64 | Yes (map) |
| C | unsigned long long | Manual |
| PHP | GMP | Yes (array) |
| Bash | bc | Manual (strings) |

### Performance Characteristics

- **Compiled languages** (C, Rust, Go, Zig): Sub-millisecond execution
- **JVM languages** (Java, Clojure): Fast after JIT warmup
- **Scripting languages** (Python, Ruby, PHP): 10-100ms range
- **Bash**: Significantly slower due to string manipulation and subprocess calls to `bc`

---

## Answers

- **Part 1**: 1615
- **Part 2**: 43560947406326
