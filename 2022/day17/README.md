# Day 17: Pyroclastic Flow

## Problem Summary

Simulate falling rocks (Tetris-like) in a 7-unit wide chamber. Rocks fall from above and are pushed by jets of hot gas while falling. Each rock alternates between being pushed horizontally by a jet and falling one unit down until it comes to rest.

**Part 1**: Calculate the tower height after 2022 rocks have stopped falling.

**Part 2**: Calculate the tower height after 1,000,000,000,000 (one trillion) rocks have stopped.

## Input Format

A single line of `<` and `>` characters representing the jet pattern:
```
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
```

## Rock Shapes

Five rock shapes cycle in order:
1. Horizontal line: `####`
2. Plus: `.#.` / `###` / `.#.`
3. L-shape: `..#` / `..#` / `###`
4. Vertical line: `#` (4 tall)
5. Square: `##` / `##`

## Algorithmic Approach

### Part 1: Direct Simulation

Straightforward simulation:
1. For each rock, start at position (x=2, y=height+3)
2. Alternate: apply jet push (if valid), then fall down (if valid)
3. When rock can't fall, add its positions to the occupied set
4. Track maximum height

### Part 2: Cycle Detection

Simulating 1 trillion rocks directly is infeasible. The key insight is that the system must eventually cycle because the state space is finite:

**State**: (rock_type % 5, jet_index % len(jets), surface_profile)

Where surface_profile captures the relative heights of the top ~30 rows.

**Algorithm**:
1. Simulate rocks while tracking states
2. When a state repeats, we've found a cycle
3. Calculate: `cycle_len = current_rock - cycle_start_rock`
4. Calculate: `cycle_height = current_height - cycle_start_height`
5. Use arithmetic to compute final height:
   - `remaining = 1_000_000_000_000 - current_rock - 1`
   - `full_cycles = remaining // cycle_len`
   - `leftover = remaining % cycle_len`
   - `final_height = height + full_cycles * cycle_height + (heights[cycle_start + leftover] - heights[cycle_start])`

### Complexity

- **Part 1**: O(rocks × avg_fall_distance × rock_size) ≈ O(2022 × 10 × 5) = O(100K)
- **Part 2**: O(until_cycle_found × ...) - typically finds cycle within 10K rocks
- **Space**: O(occupied_cells) + O(states_for_cycle_detection)

## Programming Techniques Highlighted

- **Simulation**: Step-by-step physics simulation
- **Coordinate hashing**: Efficient collision detection using sets
- **Cycle detection**: State-based repetition finding
- **Modular arithmetic**: Computing results for large iterations

## Language-Specific Notes

### Performance Characteristics

Day 17 is much faster than Day 16 since cycle detection allows Part 2 to complete quickly.

**Fast implementations (6-12ms)**:
- **C**: 6.1ms - minimal overhead, efficient set operations
- **ARM64**: 7.3ms - hand-optimized assembly, efficient hash map
- **C++**: 9.6ms - STL unordered_set performs well here
- **Zig**: 10.4ms - good hash map performance
- **Rust**: 12.0ms - HashSet with default hasher

**Moderate (36-85ms)**:
- **Common Lisp**: 36.1ms - hash tables work efficiently
- **Python**: 54.1ms - set operations are optimized
- **Perl**: 63.3ms - hash performance acceptable
- **Java**: 74.2ms - HashMap overhead
- **Node.js**: 79.7ms - V8 Map performance
- **PHP**: 84.3ms

**Slow (200ms+)**:
- **Ruby**: 213ms
- **Clojure**: 497ms

**Very slow**:
- **Go**: 1.4s with 9.5GB memory (implementation issue)
- **Bash**: 49.6s - associative arrays are slow

### Implementation Notes

- The surface profile for cycle detection should capture enough rows (20-30) to ensure accurate state matching
- Some implementations store full grid, others use sparse sets
- Go's massive memory usage suggests a memory leak or inefficient data structure choice

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 6.1          | 1.9         |
| C++         | 9.6          | 2.3         |
| Zig         | 10.4         | 1.9         |
| Rust        | 12.0         | 2.8         |
| Common Lisp | 36.1         | 43.4        |
| Python      | 54.1         | 17.5        |
| Perl        | 63.3         | 6.4         |
| Java        | 74.2         | 59.9        |
| Node.js     | 79.7         | 51.6        |
| PHP         | 84.3         | 26.5        |
| Ruby        | 213.1        | 30.1        |
| Clojure     | 496.9        | 189.5       |
| Go          | 1,440.1      | 9,587.7     |
| Bash        | 49,566.7     | 6.8         |
| ARM64 asm   | 7.3          | 13.3        |
| ColdFusion  | 2,655.0      | 1,059.4     |

## Answers

- Part 1: 3135
- Part 2: 1569054441243
