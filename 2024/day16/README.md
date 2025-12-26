# Day 16: Reindeer Maze

## Problem Summary

The Reindeer Olympics are underway with the Reindeer Maze challenge, where reindeer compete for the lowest score. You need to find the optimal path through a maze with special scoring rules.

**Part 1**: Calculate the lowest score for a path from Start (S) to End (E). The reindeer starts facing East and can:
- Move forward one tile (cost: 1 point)
- Rotate 90 degrees clockwise or counterclockwise (cost: 1000 points)
- Never move into walls (#)

**Part 2**: Find all tiles that lie on at least one optimal path. Multiple paths may achieve the same lowest score, and we need to count every tile that participates in any of these best paths.

### Input Format

A 2D grid maze with:
- `S` = Start position (reindeer begins here facing East)
- `E` = End position (goal)
- `.` = Open floor tile
- `#` = Wall (impassable)

## Algorithmic Approach

### Part 1: Dijkstra with Directional State

This is a weighted shortest path problem with a twist: the state space includes not just position but also **direction**. Each state is `(x, y, direction)` where direction is one of {East, South, West, North}.

**Key transitions:**
- Move forward: `(x, y, d) → (x+dx, y+dy, d)` with cost +1
- Turn left: `(x, y, d) → (x, y, (d-1) mod 4)` with cost +1000
- Turn right: `(x, y, d) → (x, y, (d+1) mod 4)` with cost +1000

Classic Dijkstra's algorithm finds the minimum cost to reach the End tile in any direction.

### Part 2: Bidirectional Dijkstra for Path Reconstruction

To find all tiles on optimal paths without explicitly enumerating all paths:

1. **Forward Dijkstra**: Run from Start facing East, computing `dist_from_start[(x, y, d)]`
2. **Backward Dijkstra**: Run from End (all directions), computing `dist_to_end[(x, y, d)]`
3. **Optimal Path Detection**: A tile `(x, y)` is on an optimal path if there exists any direction `d` such that:
   ```
   dist_from_start[(x, y, d)] + dist_to_end[(x, y, d)] == best_score
   ```

This elegant approach avoids exponential path enumeration by checking whether each state can be "routed through" on an optimal path.

### Key Insights

**State Space Expansion**: The critical insight is recognizing this as a graph problem where nodes are `(position, direction)` tuples, not just positions. A naive position-only approach fails because turning costs make direction matter.

**Bidirectional Search for Path Recovery**: Part 2's challenge is counting tiles without exploding into exponential path enumeration. Running Dijkstra both forward and backward allows O(1) checking of whether any state lies on an optimal path.

**Turn Cost Asymmetry**: The 1000x cost difference between moving and turning creates interesting maze dynamics. Sometimes taking a longer path with fewer turns is cheaper than a shorter path with many turns.

### Complexity

- **Time**: O((V × 4) log(V × 4)) where V = number of non-wall tiles
  - State space is V × 4 (4 directions per tile)
  - Dijkstra with binary heap: O(E log V)
  - Each state has ~3 edges (forward, turn left, turn right)
- **Space**: O(V × 4) to store distance maps for all directional states
- **Part 2**: Runs Dijkstra twice (forward + backward), still O(V log V)

## Programming Techniques Highlighted

- **Graph algorithms**: Dijkstra's algorithm with custom state representation
- **State space modeling**: Encoding position + direction as composite state
- **Bidirectional search**: Running shortest path from both endpoints
- **Path reconstruction**: Detecting optimal paths via distance sum equality
- **Priority queue**: Heap-based implementation for Dijkstra efficiency

## Language-Specific Notes

### Exceptional Performers
- **Rust** (5.5ms): Efficient heap operations, zero-cost abstractions, optimal memory layout
- **Zig** (5.6ms): Tight control over allocations, fast hash maps
- **C** (11.8ms): Manual memory management, efficient hash tables

### Compiled Languages
- **ARM64 asm** (13ms): Direct hardware-level priority queue and hash table implementation
- **Go** (63.6ms): Built-in heap package works well, GC overhead minimal for this problem
- **C++** (131ms): STL priority queue and map work but have more overhead

### JVM Languages
- **Java** (184ms): HashMap and PriorityQueue are solid, but JVM startup tax
- **Clojure** (1002ms): Persistent data structures add overhead; immutability tax on hot loops

### Scripting Languages
- **Python** (224ms): heapq module is C-implemented and fast; dict lookups efficient
- **PHP** (236ms): SplPriorityQueue and arrays handle the workload reasonably
- **Ruby** (661ms): Heap implementation in pure Ruby is slower
- **Node.js** (178ms): V8's Map and custom heap perform well

### Challenging Cases
- **Perl** (8350ms): No built-in priority queue; manual heap management in Perl is painful
- **ColdFusion** (6314ms): CFML lacks efficient data structures for graph algorithms
- **Bash** (>300s): Priority queue operations in shell scripting are prohibitively slow

### Implementation Tips

**Priority Queue Requirements**: This problem critically depends on efficient heap operations. Languages without built-in heaps (Perl, Bash) struggle enormously.

**Hash Map Performance**: Need fast lookups for `(x, y, direction)` state tuples. Languages with efficient hash tables (Python dict, Rust HashMap) excel.

**Direction Encoding**: Clean integer encoding (0=East, 1=South, 2=West, 3=North) with modular arithmetic for turns simplifies rotation logic across all languages.

**Backward Dijkstra**: Reversing edges requires care. Instead of moving from `(x-dx, y-dy)` toward `(x, y)`, think "if I'm at `(x, y)` facing `d`, I could have come from behind me."

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Rust        | 5.5          | 1.9         |
| Zig         | 5.6          | 1.9         |
| C           | 11.8         | 3.2         |
| ARM64 asm   | 13.0         | 2.3         |
| Go          | 63.6         | 13.2        |
| C++         | 131.0        | 5.9         |
| Common Lisp | 172.8        | 73.0        |
| Node.js     | 178.7        | 77.8        |
| Java        | 184.0        | 99.5        |
| Python      | 224.5        | 30.5        |
| PHP         | 236.5        | 33.2        |
| Ruby        | 661.7        | 41.0        |
| Clojure     | 1,002.2      | 669.3       |
| ColdFusion  | 6,314.4      | 1,065.5     |
| Perl        | 8,350.5      | 14.4        |
| Bash        | >300,000     | TBD         |

## Answers

- **Part 1**: 93436
- **Part 2**: 486
