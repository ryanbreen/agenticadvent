# Day 23: A Long Walk

## Problem Summary

You're hiking on Snow Island and need to find the longest scenic hike through a trail map. The map shows:
- Paths (`.`) - walkable tiles
- Forest (`#`) - impassable
- Slopes (`^`, `v`, `<`, `>`) - steep slopes

**Part 1**: Slopes are icy - when you step on a slope, your next move must be downhill (in the direction the arrow points). Find the longest path from start to end.

**Part 2**: Slopes are now dry and can be traversed in any direction. Find the longest path with this relaxed constraint.

### Input Format
A 2D grid where the start is the single `.` in the top row and the end is the single `.` in the bottom row.

## Algorithmic Approach

This is a **longest path problem**, which is NP-hard in general graphs. The key insight is **graph compression**.

### Graph Compression

The maze has long corridors with no choices - only at junctions (cells with 3+ walkable neighbors) do you make decisions. By compressing the graph:
- Nodes: start, end, and junction cells only (~36 nodes for this input)
- Edges: weighted by the distance along corridors between junctions

This dramatically reduces the search space from ~20,000 cells to ~36 nodes.

### Part 1: Directed Graph (DAG-like)
With slope constraints, the graph becomes mostly directed. The DFS with backtracking explores all paths efficiently since many directions are blocked by slopes.

### Part 2: Undirected Graph (True Longest Path)
Without slope constraints, this becomes a true longest path problem. DFS with backtracking and a visited bitmask explores all possible paths.

### Algorithm Steps
1. Parse grid and identify junction points
2. BFS/DFS from each junction to find adjacent junctions and distances
3. Build adjacency list with edge weights
4. DFS with backtracking to find longest path from start to end

### Complexity
- **Time**: O(V! / (V-L)!) where V = junction count, L = path length (exponential but pruned)
- **Space**: O(V) for visited tracking + O(V^2) for graph storage

## Key Insight

The visited set uses a **bitmask** (since V < 64) for O(1) add/remove operations. This is crucial for the backtracking efficiency.

## Programming Techniques Highlighted

- **Graph compression**: Reducing grid to weighted graph
- **DFS with backtracking**: Exploring all paths
- **Bitmask for visited set**: O(1) operations
- **BFS for edge discovery**: Finding distances between junctions

## Language-Specific Notes

- **C++ (159ms)**: Fastest. Uses `unordered_map` with bitmask visited.
- **Rust (161ms)**: Nearly identical to C++. HashMap with u64 bitmask.
- **Zig (168ms)**: Competitive with C++/Rust.
- **ARM64 (216ms)**: Hand-written assembly with bitmask tracking.
- **C (467ms)**: Slower than expected - adjacency matrix approach has more overhead.
- **Java (1.8s)**: JVM overhead, but respectable for this problem class.
- **Go (3.2s)**: Map-based visited tracking is slower than bitmask.
- **Node.js (3.7s)**: Recursive DFS with Set operations.
- **Common Lisp (4.0s)**: SBCL performs well with hash-table tracking.
- **PHP (4.7s)**: Associative array overhead.
- **Clojure (11.3s)**: Persistent data structures add overhead for backtracking.
- **Python (12.5s)**: Set-based visited with recursive DFS.
- **Perl (15.9s)**: Hash-based tracking.
- **ColdFusion (31.4s)**: JVM + interpreter overhead.
- **Ruby (87.9s)**: Recursive method calls are expensive.
- **Bash (>300s)**: Too slow for benchmark - DFS backtracking in shell is impractical.

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C++         | 159.3        | 1.9         |
| Rust        | 161.4        | 1.9         |
| Zig         | 168.1        | 1.9         |
| ARM64       | 215.9        | 1.4         |
| C           | 467.4        | 1.9         |
| Java        | 1,782.5      | 1,284.1     |
| Go          | 3,219.3      | 6.6         |
| Node.js     | 3,677.8      | 62.7        |
| Common Lisp | 4,001.3      | 53.0        |
| PHP         | 4,721.3      | 25.7        |
| Clojure     | 11,336.2     | 1,323.7     |
| Python      | 12,541.0     | 16.0        |
| Perl        | 15,922.1     | 4.6         |
| ColdFusion  | 31,407.9     | 1,026.5     |
| Ruby        | 87,935.8     | 28.1        |
| Bash        | >300,000     | ~10         |

## Answers
- Part 1: **2310**
- Part 2: **6738**
