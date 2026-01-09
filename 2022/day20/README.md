# Day 20: Grove Positioning System

## Problem Summary

The Elves' encrypted grove coordinates file needs decryption through a process called **mixing**. Each number in the circular list moves forward (positive) or backward (negative) by its value, wrapping around the ends.

**Part 1**: Mix the list once and find the grove coordinates (sum of 1000th, 2000th, 3000th values after 0).

**Part 2**: Multiply all numbers by the decryption key (811589153), mix 10 times, then find grove coordinates.

## Input Format

Simple list of signed integers, one per line:
```
1
2
-3
3
-2
0
4
```

## Algorithmic Approach

### Key Insight

The main challenge is handling **duplicate values** and **maintaining original order**. Since numbers can repeat, we can't identify elements by value alone. The solution tracks each element as an `(original_index, value)` pair.

### Data Structure

Use a list/vector of (original_index, value) pairs. This allows:
- Finding elements by their original position in O(n)
- Removing and inserting elements in O(n)
- Processing elements in original order by iterating orig_idx from 0 to n-1

### Mix Algorithm

For each element in original order:
1. Find its current position in the mixed list
2. Remove it (shifts remaining elements)
3. Calculate new position: `(current_pos + value) % (n - 1)`
   - Note: modulo (n-1) because after removal, list has n-1 elements
4. Insert at new position (shifts elements to make room)

### Handling Negative Modulo

Most languages handle negative modulo differently. For proper circular behavior:
```
new_pos = ((current_pos + value) % (n-1) + (n-1)) % (n-1)
```

### Complexity

- **Time**: O(n² × t) where n = list size, t = number of mix rounds
  - Part 1: O(n²) with t=1
  - Part 2: O(n²) with t=10
- **Space**: O(n) for the indexed list

## Programming Techniques Highlighted

- **Circular list manipulation**: Modular arithmetic for wraparound
- **Index tracking**: Maintaining identity through movement
- **Array shifting**: Efficient remove/insert operations
- **Big integer handling**: Part 2 produces values ~10^15 requiring 64-bit integers

## Language-Specific Notes

### Performance Characteristics

This problem has O(n²) complexity per mix round, making array operations critical.

**Fast (< 500ms)**:
- **Zig**: 102.5ms - extremely efficient array operations
- **ARM64 asm**: 148.0ms - hand-optimized memory access
- **C**: 205.4ms - efficient pointer arithmetic
- **Go**: 369.4ms - good slice operations
- **Rust**: 373.1ms - vec operations competitive

**Moderate (500ms - 3s)**:
- **Java**: 532.8ms - ArrayList overhead
- **C++**: 541.8ms - vector operations
- **Node.js**: 1,148.0ms - V8 array performance
- **Common Lisp**: 1,381.0ms - adjustable arrays

**Slow (3s+)**:
- **PHP**: 2,875.5ms - array_splice overhead
- **Python**: 3,807.7ms - list operations interpreted
- **Ruby**: 3,879.6ms - similar to Python
- **Perl**: 4,247.3ms - splice operations
- **Clojure**: 14,729.6ms - immutable data structures
- **ColdFusion**: 17,848.5ms - JVM overhead
- **Bash**: 59,131.9ms - shell array limitations

### Implementation Notes

- Languages with efficient slice/splice operations excel here
- Immutable data structures (Clojure) suffer due to O(n) copy per operation
- Bash requires AWK for reasonable performance
- All implementations need 64-bit integers for Part 2

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 102.5        | 1.7         |
| ARM64 asm   | 148.0        | 35.7        |
| C           | 205.4        | 47.3        |
| Go          | 369.4        | 27.2        |
| Rust        | 373.1        | 102.9       |
| Java        | 532.8        | 118.4       |
| C++         | 541.8        | 118.7       |
| Node.js     | 1,148.0      | 53.2        |
| Common Lisp | 1,381.0      | 44.1        |
| PHP         | 2,875.5      | 26.9        |
| Python      | 3,807.7      | 16.1        |
| Ruby        | 3,879.6      | 28.2        |
| Perl        | 4,247.3      | 6.7         |
| Clojure     | 14,729.6     | 2,225.1     |
| ColdFusion  | 17,848.5     | 1,036.7     |
| Bash        | 59,131.9     | 6.7         |

## Answers

- Part 1: 3466
- Part 2: 9995532008348
