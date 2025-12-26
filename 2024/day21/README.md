# Day 21: Keypad Conundrum

## Problem Summary

This puzzle presents a delightfully recursive challenge involving a chain of robots controlling robots to type door codes. You need to type on a directional keypad, which controls a robot typing on another directional keypad, which controls yet another robot... ultimately controlling a robot that types on a numeric keypad to enter door codes.

**Part 1**: Calculate the complexity sum for five door codes, where each robot chain has **2 intermediate directional keypads** (you → robot 1 → robot 2 → robot 3 on numeric pad). Complexity is defined as the shortest sequence length multiplied by the numeric portion of the code.

**Part 2**: The same calculation, but with **25 intermediate directional keypads** in the robot chain. This exponentially increases the sequence lengths required.

### Input Format

Five 4-character door codes, each ending with 'A' (e.g., `029A`, `980A`).

### Keypad Layouts

**Numeric keypad** (on the door):
```
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+
```

**Directional keypad** (all robots and you):
```
    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+
```

**Critical constraint**: The robot arm must never point at a gap (empty space) even momentarily, or it panics. All robots start at the 'A' button.

## Algorithmic Approach

### Part 1: Two-Level Robot Chain

The core insight is that this is a **recursive shortest path problem** with memoization. Each button press at one level expands into a sequence of button presses at the level above.

**Key algorithm components:**

1. **Shortest path generation**: For any move from button X to button Y on either keypad, generate all shortest paths that avoid the gap
   - Use directional characters (^, v, <, >) to represent moves
   - Multiple shortest paths may exist (e.g., going right then up vs. up then right)

2. **Recursive depth-first search with memoization**:
   - At depth 0 (human level): sequence length is just the number of moves + 1 for pressing 'A'
   - At depth N: for each possible shortest path, recursively compute the cost of typing that path on the directional keypad above
   - Cache results by (from_char, to_char, depth, keypad_type)

3. **Greedy path selection**: Among all shortest paths for a move, choose the one that minimizes the recursive cost

4. **Complexity calculation**: For each code:
   - Compute minimum presses needed to type it
   - Multiply by the numeric part (e.g., `029A` → 29)
   - Sum all complexities

### Part 2: Twenty-Five-Level Robot Chain

The exact same algorithm, just with depth = 25 instead of depth = 2. The memoization becomes absolutely critical here - without caching, the exponential branching would be computationally infeasible.

### Key Insights

1. **Recursive expansion**: Each button press at level N expands into ~1-10 button presses at level N-1, creating exponential growth
2. **Memoization is essential**: The same (from, to, depth) transitions occur repeatedly across different codes
3. **Path ordering matters**: Among equally-short paths, some lead to shorter expansions at higher levels. The algorithm must explore all possibilities.
4. **Gap avoidance**: The gap constraint eliminates some otherwise-shortest paths, forcing detours
5. **Directional sequences propagate**: Moving left (<) is particularly expensive because it requires complex sequences on the controlling keypad

### Complexity

- **Time**: O(C × L × P × D) where:
  - C = number of codes (5)
  - L = code length (4)
  - P = average number of shortest paths per move (~2-4)
  - D = depth (2 for Part 1, 25 for Part 2)
  - With memoization, each unique (from, to, depth, keypad) pair is computed once
  - Total unique states: ~100 (buttons) × D (depth) × 2 (keypads) ≈ 400-5000 states

- **Space**: O(states) for memoization cache
  - Part 1: ~400 cached entries
  - Part 2: ~5000 cached entries

## Programming Techniques Highlighted

- **Dynamic programming**: Memoization with `@lru_cache` to avoid recomputing subproblems
- **Recursive problem decomposition**: Breaking down the multi-level robot chain into recursive calls
- **Graph algorithms**: Finding all shortest paths while avoiding obstacles (the gap)
- **Depth-first search**: Exploring all possible shortest paths to find the optimal one
- **Combinatorial optimization**: Choosing among multiple equally-valid paths based on downstream cost

## Language-Specific Notes

### Fast Performers

- **ARM64 Assembly** (4.8ms): Hand-optimized memoization with custom hash table, minimal overhead
- **Zig** (5.6ms): Efficient HashMap and manual memory management
- **C** (6.0ms): Simple hash table implementation, careful memory layout
- **C++** (6.7ms): std::unordered_map provides good cache performance
- **Rust** (6.8ms): HashMap with excellent memory safety guarantees

These languages excel because the algorithm is cache-bound. Fast hash lookups and minimal allocation overhead are critical.

### Mid-Tier Performers

- **Go** (8.0ms): Built-in map is efficient, garbage collection overhead is minimal
- **Perl** (23.0ms): Surprisingly fast for a scripting language; native hash tables shine
- **Python** (26.4ms): `@lru_cache` is highly optimized C code, dict lookups are fast
- **Common Lisp** (31.4ms): Good hash table performance, but SBCL startup overhead
- **Bash** (33.3ms): Shockingly fast for Bash! Associative arrays + careful algorithm design

### Slower Performers

- **Node.js** (44.3ms): V8 JIT is good, but object creation overhead adds up
- **Ruby** (62.5ms): Hash table overhead and interpreter slowness
- **Java** (66.9ms): JVM startup dominates runtime; HashMap itself is fast
- **PHP** (69.6ms): Array operations aren't as optimized as other languages
- **Clojure** (418.9ms): JVM startup + persistent data structure overhead
- **ColdFusion** (2,601.0ms): Web server overhead + CFML interpreter inefficiency

### Implementation Challenges

- **Memoization**: Languages without built-in memoization (C, C++, ARM64) require custom hash table implementations
- **Multiple return values**: Returning all shortest paths requires either dynamic arrays or linked structures
- **Recursion depth**: 25 levels of recursion requires careful stack management in some languages
- **Large numbers**: Part 2 results exceed 200 trillion, requiring 64-bit integers (or BigInteger in Java)

### Special Cases

- **Bash**: Uses associative arrays for memoization; surprisingly effective despite shell overhead
- **ARM64**: Custom hash function and collision resolution, but linear probing works well with small state space
- **ColdFusion**: Requires web server context; significant initialization overhead but functional

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 4.8          | 1.9         |
| Zig         | 5.6          | 1.9         |
| C           | 6.0          | 4.4         |
| C++         | 6.7          | 1.9         |
| Rust        | 6.8          | 1.9         |
| Go          | 8.0          | 4.2         |
| Perl        | 23.0         | 8.1         |
| Python      | 26.4         | 15.9        |
| Common Lisp | 31.4         | 41.9        |
| Bash        | 33.3         | 15.9        |
| Node.js     | 44.3         | 38.9        |
| Ruby        | 62.5         | 27.8        |
| Java        | 66.9         | 50.3        |
| PHP         | 69.6         | 24.9        |
| Clojure     | 418.9        | 133.6       |
| ColdFusion  | 2,601.0      | 1,119.8     |

## Answers

- **Part 1**: 203814
- **Part 2**: 248566068436630
