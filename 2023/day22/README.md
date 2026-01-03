# Day 22: Sand Slabs

## Problem Summary

Bricks of sand are falling and stacking up in a 3D space. Each brick is a straight line of cubes defined by two endpoints. After all bricks settle (fall until they hit the ground at z=1 or another brick), we need to analyze the support structure.

**Part 1**: Count how many bricks can be safely disintegrated without causing any other brick to fall.

**Part 2**: For each brick, calculate how many other bricks would fall if that brick alone were disintegrated. Sum these counts.

### Input Format
Each line contains two 3D coordinates separated by `~`:
```
1,0,1~1,2,1
0,0,2~2,0,2
```
Each coordinate is `x,y,z` representing the two ends of a brick.

## Part 1: Safe Disintegration

A brick can be safely removed if every brick it supports has at least one other supporter. We need to:
1. Settle all bricks by dropping them
2. Build a support graph (who supports whom)
3. Count bricks where all supported bricks have multiple supporters

## Part 2: Chain Reactions

For each brick, simulate removing it and count how many bricks would fall in the resulting chain reaction. A brick falls if ALL of its supporters have fallen.

## Algorithmic Approach

### Settling Bricks
1. Sort bricks by their minimum z-coordinate (process from bottom to top)
2. For each brick, find the maximum z of any occupied cell below its footprint
3. Drop the brick to rest on that surface (or ground at z=1)
4. Track occupied cells with a hash map: `(x,y,z) -> brick_index`

### Building Support Graph
Two data structures:
- `supports[i]` = set of brick indices that brick i supports (bricks directly above)
- `supporters[i]` = set of brick indices that support brick i (bricks directly below)

When settling a brick, check cells at `z-1` beneath its footprint to find supporters.

### Part 1 Logic
```
safe_count = 0
for each brick i:
    can_remove = true
    for each brick j that i supports:
        if j has only one supporter:
            can_remove = false
    if can_remove:
        safe_count++
```

### Part 2 Logic (BFS Chain Reaction)
```
total = 0
for each brick i:
    falling = {i}
    queue = [i]
    while queue not empty:
        brick = queue.pop()
        for each brick j that brick supports:
            if all supporters of j are in falling:
                falling.add(j)
                queue.push(j)
    total += len(falling) - 1  # exclude initial brick
```

### Complexity
- **Time**: O(n * b) where n = number of bricks, b = average number of bricks affected by chain reaction
- **Space**: O(n + c) where c = number of occupied cells

## Key Insights

1. **Sort by Z first**: Processing bricks from bottom to top ensures each brick only needs to consider already-settled bricks.

2. **Height map optimization** (used in Bash): Instead of tracking all 3D cells, maintain max z at each (x,y) position. This trades accuracy for speed when memory or iteration is expensive.

3. **Subset check for Part 2**: A brick falls if `supporters[brick] ⊆ falling`. This is more efficient than counting remaining supporters.

## Programming Techniques Highlighted

- **3D Spatial Indexing**: Hash maps for occupied cell lookup
- **Graph Construction**: Building support relationships from spatial data
- **BFS Simulation**: Propagating chain reactions through support graph
- **Set Operations**: Subset checks for determining if a brick falls

## Language-Specific Notes

- **C (11.3ms)**: Fastest overall. Uses custom hash table for 3D coordinates, bitsets for support relationships.

- **C++ (41.7ms)**: Uses `unordered_map` and `unordered_set`. Higher abstraction overhead than C.

- **Rust (28.0ms)**: Clean implementation with `HashMap` and `HashSet`. Good balance of safety and performance.

- **Zig (33.7ms)**: Uses `AutoHashMapUnmanaged` for memory efficiency. Low-level control like C.

- **Go (38.1ms)**: Straightforward map-based implementation.

- **Common Lisp (85.2ms)**: Surprisingly fast. SBCL optimizes hash table operations well.

- **ARM64 (80.3ms)**: Pure assembly with custom hash table, bitmap for support sets, BFS with array-based queue. Impressively compact for such complex logic.

- **Python (101.3ms)**: Clean reference implementation. `defaultdict` and `set` operations are well-optimized.

- **PHP (111.5ms)**: Good performance with associative arrays.

- **Perl (180.4ms)**: Hash-based implementation works but slower.

- **Ruby (559.9ms)**: Slower due to interpreted nature and Set overhead.

- **Clojure (600.3ms)**: Functional approach with persistent data structures. JVM startup adds overhead.

- **Bash (2.2s)**: Uses height map optimization to reduce 3D lookups. Associative arrays are inherently slow.

- **ColdFusion (3.0s)**: Slowest due to interpreter overhead and memory-heavy data structures.

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 11.3         | 18.6        |
| Rust        | 28.0         | 3.1         |
| Zig         | 33.7         | 2.1         |
| Go          | 38.1         | 9.9         |
| C++         | 41.7         | 2.5         |
| ARM64       | 80.3         | 5.6         |
| Common Lisp | 85.2         | 90.4        |
| Python      | 101.3        | 17.9        |
| Java        | 108.3        | 100.5       |
| PHP         | 111.5        | 28.4        |
| Node.js     | 117.5        | 56.9        |
| Perl        | 180.4        | 8.4         |
| Ruby        | 559.9        | 32.7        |
| Clojure     | 600.3        | 290.1       |
| Bash        | 2168.0       | 8.3         |
| ColdFusion  | 3021.5       | 1104.4      |

## Answers
- Part 1: **482**
- Part 2: **103010**
