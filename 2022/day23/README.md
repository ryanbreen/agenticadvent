# Day 23: Unstable Diffusion

## Problem Summary

Simulate elves spreading out on an infinite grid. Each round, elves propose moves based on neighbor positions, then execute non-conflicting proposals.

**Part 1**: Count empty tiles in bounding rectangle after 10 rounds.

**Part 2**: Find the first round where no elf needs to move.

## Input Format

Grid with `#` for elf positions and `.` for empty:
```
....#..
..###.#
#...#.#
```

## Algorithmic Approach

### Simulation Rules

Each round has two phases:

1. **Proposal Phase**: Each elf with neighbors checks directions in order:
   - North (if N, NE, NW empty) → propose move north
   - South (if S, SE, SW empty) → propose move south
   - West (if W, NW, SW empty) → propose move west
   - East (if E, NE, SE empty) → propose move east

2. **Execution Phase**: Elves move to proposed position only if no other elf proposed the same destination.

After each round, rotate direction priority (N,S,W,E → S,W,E,N → ...).

### Data Structures

- **Elf set**: Hash set of (row, col) positions for O(1) lookup
- **Proposals**: Map from elf → proposed position
- **Proposal counts**: Map from position → count for conflict detection

### Complexity

- **Time**: O(E × R) where E = elves, R = rounds
- **Space**: O(E) for position storage

## Programming Techniques Highlighted

- **Cellular automata**: Grid-based simulation with neighbor rules
- **Set operations**: Efficient position lookup and tracking
- **Direction cycling**: Rotating priority list each round

## Language-Specific Notes

### Performance Characteristics

This problem is simulation-heavy with many hash set operations per round.

**Fast (< 600ms)**:
- **C**: 239.5ms - efficient custom hash table
- **Rust**: 391.8ms - HashSet performance
- **Zig**: 529.3ms - hash map implementation
- **C++**: 573.6ms - unordered_set
- **Go**: 591.9ms - map operations

**Moderate (600ms - 3s)**:
- **Common Lisp**: 905.5ms - hash table performance
- **Java**: 1,704.3ms - HashSet with object overhead
- **PHP**: 2,088.9ms - associative arrays
- **Node.js**: 2,234.3ms - Set/Map overhead
- **Python**: 2,856.7ms - set operations interpreted

**Slow (> 3s)**:
- **Clojure**: 3,081.6ms - persistent data structures
- **Perl**: 4,543.2ms - hash operations
- **Ruby**: 14,295.2ms - set operations slow
- **Bash**: 14,688.8ms - AWK for computation
- **ColdFusion**: 15,920.7ms - struct operations

### Implementation Notes

- Part 2 typically runs ~950 rounds, making efficiency crucial
- Hash set performance dominates overall runtime
- Memory usage correlates with number of intermediate hash entries

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 239.5        | 2.7         |
| Rust        | 391.8        | 2.3         |
| Zig         | 529.3        | 1.9         |
| C++         | 573.6        | 2.1         |
| Go          | 591.9        | 26.8        |
| Common Lisp | 905.5        | 99.4        |
| Java        | 1,704.3      | 1,271.7     |
| PHP         | 2,088.9      | 27.5        |
| Node.js     | 2,234.3      | 90.0        |
| Python      | 2,856.7      | 16.5        |
| Clojure     | 3,081.6      | 1,340.7     |
| Perl        | 4,543.2      | 7.3         |
| Ruby        | 14,295.2     | 31.9        |
| Bash        | 14,688.8     | 6.7         |
| ColdFusion  | 15,920.7     | 1,072.1     |

## Answers

- Part 1: 4075
- Part 2: 950
