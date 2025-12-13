# Day 10: Hoof It - Zig Solution

## Compilation

```bash
# Simple compilation
zig build-exe solution.zig -O ReleaseFast

# Or using build.zig
zig build
```

## Running

```bash
./solution
```

## Implementation Notes

This Zig solution implements both parts of Day 10:

- **Part 1**: Uses BFS (Breadth-First Search) to find all unique 9-height positions reachable from each trailhead (height 0). Uses `AutoHashMap` to track visited positions and reachable nines.

- **Part 2**: Uses DFS (Depth-First Search) with recursion to count all distinct hiking trails from each trailhead to any 9-height position.

### Key Data Structures

- `Grid`: Stores the topographic map as a 2D array of heights
- `Position`: A simple struct representing row/column coordinates
- `AutoHashMap`: Used for tracking visited positions and unique destinations
- `ArrayList`: Used for the BFS queue and collecting trailheads

### Algorithm Complexity

- **Part 1 (BFS)**: O(N * M) per trailhead where N×M is grid size
- **Part 2 (DFS)**: O(4^H) worst case per trailhead where H is max height (9), but typically much better due to grid constraints

The solution leverages Zig's allocator pattern for memory management and uses Zig 0.15's updated ArrayList API.
