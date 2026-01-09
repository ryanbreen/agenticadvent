# Day 10: Cathode-Ray Tube

## Problem Summary

Simulate a simple CPU that drives a CRT display. The CPU has one register `X` (starting at 1) and two instructions:
- `noop` - takes 1 cycle, does nothing
- `addx V` - takes 2 cycles, then adds V to X

**Part 1**: Calculate signal strength (cycle × X) at cycles 20, 60, 100, 140, 180, 220 and sum them.

**Part 2**: Render a 40×6 CRT display where each pixel lights up (`#`) if the sprite (3 pixels wide, centered at X) overlaps the current drawing position.

## Input Format

Instructions, one per line:
```
noop
addx 3
addx -5
noop
```

## Algorithmic Approach

### Key Insight

The critical insight is understanding **when** register updates happen:
- For `addx V`, the value of X changes **after** the 2nd cycle completes
- During both cycles of an addx, X retains its **previous** value
- Signal strength and pixel drawing use X's value **during** the cycle

### Part 1 Algorithm

Use a generator/iterator pattern to yield `(cycle, x)` pairs:

```
x = 1, cycle = 0
for each instruction:
    if noop:
        cycle++
        yield (cycle, x)
    else addx V:
        cycle++
        yield (cycle, x)  # First cycle, X unchanged
        cycle++
        yield (cycle, x)  # Second cycle, X still unchanged
        x += V            # Update happens AFTER second cycle
```

Sum `cycle * x` for target cycles {20, 60, 100, 140, 180, 220}.

### Part 2 Algorithm

Same generator, but track CRT position:
- CRT position = `(cycle - 1) % 40` (0-indexed column in current row)
- Sprite covers positions `X-1`, `X`, `X+1`
- If `|pos - X| <= 1`, draw `#`, otherwise `.`
- After every 40 cycles, start a new row

### Data Structures

- **Generator/Iterator**: Yields cycle states without storing all in memory
- **Screen buffer**: Array of strings (one per row) or 2D character array

### Complexity

- Time: O(n) where n = number of instructions
- Space: O(1) for Part 1, O(240) = O(1) for Part 2 (fixed 40×6 screen)

## Programming Techniques Highlighted

- **Generator Pattern**: Elegant way to model CPU cycles
- **Simulation**: Step-by-step state tracking
- **Modular Arithmetic**: `% 40` for row wrapping
- **String Building**: Constructing visual output character by character

## Language-Specific Notes

### Fast Performers (5-7ms)
- **ARM64, C, Zig, C++, Rust**: Minimal overhead, simple string operations
- Go also fast at 6.5ms

### Shell Scripts Excel Here (14ms)
- **Bash, Perl**: Both fast because the problem is pure string processing with limited iterations
- Unlike Days 8-9 which had O(n²) or hash table operations, Day 10 is linear

### Mid-Tier (22-54ms)
- **Python, Common Lisp**: Generator pattern natural and efficient
- **Node.js, Java, PHP, Ruby**: Standard performance

### Slow (384-2418ms)
- **Clojure**: JVM startup + functional overhead
- **ColdFusion**: JVM-based CFML runtime

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 4.9          | 1.9         |
| Zig         | 5.5          | 1.9         |
| C           | 5.5          | 1.9         |
| C++         | 5.6          | 1.9         |
| Rust        | 6.4          | 1.9         |
| Go          | 6.5          | 4.1         |
| Perl        | 14.0         | 5.9         |
| Bash        | 14.4         | 6.8         |
| Python      | 22.0         | 14.7        |
| Common Lisp | 26.8         | 39.9        |
| Node.js     | 41.2         | 37.6        |
| Java        | 44.1         | 45.8        |
| PHP         | 50.5         | 25.5        |
| Ruby        | 54.2         | 27.9        |
| Clojure     | 384.0        | 128.8       |
| ColdFusion  | 2,418.0      | 1,151.6     |

## Answers

- Part 1: 12560
- Part 2: PLPAFBCL
