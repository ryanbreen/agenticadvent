# Day 11: Plutonian Pebbles

## Problem Summary

You encounter physics-defying stones on Pluto that transform every time you blink. Each stone has a number engraved on it, and they follow three transformation rules applied in order:

1. **Rule 1**: If the stone shows `0`, it becomes `1`
2. **Rule 2**: If the stone has an **even number of digits**, it splits into two stones (left half and right half of the digits)
3. **Rule 3**: Otherwise, multiply the number by `2024`

**Part 1**: Count how many stones you have after 25 blinks.
**Part 2**: Count how many stones you have after 75 blinks.

### Input Format
A single line of space-separated integers representing the initial stone values.

Example: `125 17`

## Algorithmic Approach

### The Naive Approach (Why It Fails)

The obvious approach is to simulate the stones directly - maintain a list and apply transformations each blink. This works for Part 1 (25 blinks) but fails catastrophically for Part 2 (75 blinks).

Why? The number of stones grows **exponentially**. Starting with 8 stones:
- After 25 blinks: ~211,000 stones (manageable)
- After 75 blinks: ~250 trillion stones (impossible to store)

Even if each stone only took 1 byte, you'd need 250 TB of memory!

### Key Insight: Order Doesn't Matter

The critical realization is that **the order of stones is irrelevant** for counting. We only care about the total count, not which stone is where.

More importantly: **identical stones evolve identically**. If you have 1000 stones showing `42`, they will all transform the same way. You don't need to track 1000 separate evolutions - just track one and multiply by 1000.

### The Memoization Solution

Instead of tracking individual stones, we track the **count of stones resulting from a single value after N blinks**.

```
count_stones(value, blinks) -> number of stones
```

This function is pure and deterministic - the same inputs always produce the same output. This makes it perfect for **memoization** (caching results).

The recurrence relation:
- Base case: `count_stones(v, 0) = 1` (no blinks = one stone)
- If `v == 0`: `count_stones(0, n) = count_stones(1, n-1)`
- If `v` has even digits: `count_stones(v, n) = count_stones(left, n-1) + count_stones(right, n-1)`
- Otherwise: `count_stones(v, n) = count_stones(v * 2024, n-1)`

### Why Memoization Works

While there are 250 trillion *stones*, there are far fewer unique `(value, blinks)` pairs:
- Values are bounded by the transformation rules
- Blinks range from 0-75
- Many paths through the recursion tree hit the same states

In practice, the cache stays small (tens of thousands of entries), making the solution run in milliseconds.

### Complexity Analysis

- **Time**: O(U * B) where U = unique values encountered, B = number of blinks
- **Space**: O(U * B) for the memoization cache
- In practice, U is surprisingly small due to the recursive structure

## Programming Techniques Highlighted

### Core Concepts
- **Memoization / Dynamic Programming**: The entire solution hinges on caching subproblem results
- **Recursion with caching**: Natural way to express the stone evolution rules
- **Big Integer Handling**: Part 2 result (250+ trillion) exceeds 32-bit integers

### Data Structures
- **Hash Map**: For the memoization cache (key: value+blinks, value: count)
- **String manipulation**: For digit splitting (convert to string, slice, convert back)

### Mathematical Properties
- **Exponential growth bounded by caching**: What would be O(2^75) becomes polynomial
- **Digit counting**: `len(str(n))` or `floor(log10(n)) + 1`

## Language-Specific Notes

### Fast Performers
- **Zig (17.6ms)**: Excellent hash map performance, zero-overhead abstractions
- **Rust (26.6ms)**: Efficient HashMap with good default hasher
- **C (28.4ms)**: Custom hash table implementation, direct memory control

### Notable Implementations

**Python** - Clean and idiomatic:
```python
@lru_cache(maxsize=None)
def count_stones(value: int, blinks: int) -> int:
    if blinks == 0:
        return 1
    if value == 0:
        return count_stones(1, blinks - 1)
    s = str(value)
    if len(s) % 2 == 0:
        mid = len(s) // 2
        return count_stones(int(s[:mid]), blinks - 1) + count_stones(int(s[mid:]), blinks - 1)
    return count_stones(value * 2024, blinks - 1)
```

**Node.js** - Uses BigInt for safety:
```javascript
const cache = new Map();
function countStones(value, blinks) {
    if (blinks === 0) return 1n;
    const key = `${value},${blinks}`;
    if (cache.has(key)) return cache.get(key);
    // ... transformation logic
    cache.set(key, result);
    return result;
}
```

### Big Integer Considerations
- **Python**: Native arbitrary precision (transparent)
- **Node.js**: BigInt required for Part 2 result
- **Java**: Can use `long` for intermediate values, `BigInteger` for final sum
- **C/C++/Rust/Zig**: `uint64_t`/`u64` sufficient for this problem
- **Bash**: Uses `bc` for arbitrary precision arithmetic

### Performance Anomalies
- **ARM64 Assembly (135.8ms)**: Surprisingly slow due to manual hash table implementation overhead
- **Go (170.8ms)**: Map operations have more overhead than expected
- **Bash (12,062ms)**: Shell overhead dominates; function calls are expensive


## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 17.6         | 10.8        |
| Rust        | 26.6         | 14.2        |
| C           | 28.4         | 12.9        |
| C++         | 69.3         | 9.3         |
| Common Lisp | 78.1         | 69.4        |
| Python      | 128.0        | 38.8        |
| ARM64 asm   | 135.8        | 2.4         |
| PHP         | 138.9        | 33.7        |
| Java        | 147.6        | 91.5        |
| Node.js     | 157.0        | 62.9        |
| Perl        | 159.5        | 21.9        |
| Go          | 170.8        | 28.0        |
| Ruby        | 432.2        | 42.7        |
| Clojure     | 768.5        | 208.4       |
| ColdFusion  | 4,830.4      | 991.4       |
| Bash        | 12,062.0     | 14.1        |

## Answers

- **Part 1**: 211306
- **Part 2**: 250783680217283
