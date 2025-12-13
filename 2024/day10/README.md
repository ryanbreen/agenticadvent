# Day 10: Hoof It

## Problem Summary

A reindeer hands you a topographic map of a lava island where heights range from 0 (lowest) to 9 (highest). You need to help identify hiking trails that start at height 0 (trailheads), end at height 9, and increase by exactly 1 at each step. Movement is orthogonal only (up, down, left, right).

### Input Format
A 2D grid of single digits representing terrain heights:
```
89010123
78121874
87430965
...
```

### Part 1: Trailhead Scores
Calculate the **score** of each trailhead (positions with height 0). A trailhead's score is the number of distinct height-9 positions reachable from it via valid hiking trails.

### Part 2: Trailhead Ratings
Calculate the **rating** of each trailhead. A trailhead's rating is the total number of distinct hiking trails that start there and reach any height-9 position.

## Algorithmic Approach

### Part 1: BFS for Reachable Destinations

**Key Insight**: We need to count *unique* 9-height positions reachable from each trailhead, not the number of paths. This is a classic reachability problem.

**Algorithm**:
1. Find all trailheads (cells with value 0)
2. For each trailhead, perform BFS:
   - Track visited cells to avoid revisiting
   - Only traverse to neighbors with height = current_height + 1
   - Collect all reached 9-height positions in a set
3. Sum the sizes of all 9-position sets

**Why BFS?** BFS naturally explores all reachable positions level-by-level (here, height-by-height), and using a visited set prevents counting the same destination twice even if multiple paths lead there.

### Part 2: DFS Path Counting

**Key Insight**: Now we care about *distinct paths*, not just destinations. The same 9 can be reached via different routes, and each counts.

**Algorithm**:
1. For each trailhead, perform DFS without tracking visited cells
2. At each position:
   - If height is 9, return 1 (found a valid trail)
   - Otherwise, recursively count paths through all valid neighbors
   - Sum all neighbor contributions
3. Sum ratings across all trailheads

**Why DFS without visited?** Since we're counting paths, not destinations, we *want* to traverse the same cell multiple times if reached via different routes. The DAG structure (strictly increasing heights) guarantees no cycles, so recursion naturally terminates.

### Complexity Analysis

- **Grid size**: N = rows × cols
- **Part 1 (BFS)**: O(N × N) worst case - each trailhead might visit all cells
- **Part 2 (DFS)**: O(N × paths) where paths can be exponential in path length, but bounded by the grid structure

In practice, the strictly-increasing height constraint heavily prunes the search space. Memoization could optimize Part 2 but isn't necessary for the input size.

## Data Structures Used

- **2D Array/Grid**: Store the terrain heights
- **Queue (BFS)**: Process cells level-by-level for Part 1
- **Set/HashSet**: Track visited cells (Part 1) and unique 9s reached
- **Recursion Stack (DFS)**: Implicit for Part 2 path counting

## Programming Techniques Highlighted

1. **Graph Traversal**: Both BFS and DFS on an implicit grid graph
2. **Reachability vs Path Counting**: Understanding when visited tracking helps vs hurts
3. **Grid Navigation**: Standard 4-directional movement pattern
4. **DAG Properties**: Exploiting the cycle-free nature of strictly increasing paths

## Language-Specific Notes

### Performance Leaders
- **C/ARM64/C++/Rust**: Fastest at 7-8ms with minimal memory (~2MB). Low-level languages excel at simple grid operations with tight loops.
- **Go**: Nearly as fast at 8.5ms, with efficient built-in data structures.

### Mid-Tier Performance
- **Zig/Perl/Common Lisp**: 12-31ms range. Zig slightly slower due to safety checks; Perl's regex engine overhead not relevant here.
- **Python/PHP/Node.js/Java**: 35-60ms. Interpreted/JIT overhead, but problem is I/O-bound enough that algorithm efficiency dominates.

### Slower Implementations
- **Ruby**: 75ms - Higher overhead for method calls in tight loops.
- **Clojure**: 569ms - JVM startup + functional overhead, but immutable data structures ensure correctness.
- **Bash**: 101 seconds - Shell scripting with arrays and string manipulation is extremely slow for grid traversal. Uses temp files for efficiency but still impractical for production use.

### Notable Implementation Differences

**Nested Functions for DFS**:
- Python, JavaScript, Ruby support nested functions elegantly
- PHP had to move DFS to a top-level function (no nested function support)
- Lisp uses labels for local recursive functions

**Coordinate Hashing**:
- Low-level languages (C, Rust, Zig): Integer-based visited array or single integer key
- High-level languages: String-based keys ("row_col") or tuple hashing

**ARM64 Assembly**:
- Uses flat arrays with computed indices
- Direction vectors as aligned 16-byte pairs
- Stack-based queue for BFS

## Answers

- **Part 1**: 517
- **Part 2**: 1116
