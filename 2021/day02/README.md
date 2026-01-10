# Day 2: Dive!

## Problem Summary

You're piloting a submarine and need to follow a series of navigation commands to determine your final position.

**Part 1**: Simple navigation where:
- `forward X` increases horizontal position by X
- `down X` increases depth by X
- `up X` decreases depth by X

Calculate `horizontal * depth` after following all commands.

**Part 2**: Introduces an "aim" variable that changes command semantics:
- `down X` increases aim by X
- `up X` decreases aim by X
- `forward X` increases horizontal by X AND increases depth by `aim * X`

Calculate `horizontal * depth` with this new interpretation.

## Algorithmic Approach

### Part 1 Algorithm

Simple state machine with two variables:
- Track `horizontal` and `depth`, both starting at 0
- Process each command, updating the appropriate variable
- Return `horizontal * depth`

**Time Complexity**: O(n) where n is the number of commands
**Space Complexity**: O(1) - just tracking two integers

### Part 2 Algorithm

Extended state machine with three variables:
- Track `horizontal`, `depth`, and `aim`, all starting at 0
- `down`/`up` modify aim instead of depth
- `forward` modifies both horizontal AND depth (based on aim)
- Return `horizontal * depth`

**Time Complexity**: O(n)
**Space Complexity**: O(1)

### Key Insight

Part 2 introduces a coupling between commands - the effect of `forward` depends on the accumulated value of previous `down`/`up` commands. This makes the order of commands significant in a different way than Part 1.

The "aim" acts as a slope/gradient - when you move forward, you also descend (or ascend) based on your current aim angle.

## Programming Techniques Highlighted

- **State Machine**: Processing commands that modify internal state
- **Command Pattern**: Parsing and dispatching based on command type
- **String Parsing**: Splitting input lines into command and value

## Data Structures Used

- Array/list of command tuples (command string, integer value)
- Integer accumulators for position tracking

## Language-Specific Notes

- **Compiled languages (C++, Rust, ARM64, Zig, Go, C)**: All complete in ~5-7ms, very similar performance for this simple problem
- **Perl**: Fast at 8.4ms - efficient string processing
- **Common Lisp**: Good 22ms using simple loops
- **Bash**: Efficient 25ms using case statements and arithmetic
- **Python**: 26ms with clean tuple unpacking
- **Java/PHP/Node.js/Ruby**: 43-60ms range, dominated by runtime overhead
- **Clojure**: 407ms with JVM startup, uses `reduce` idiomatically
- **ColdFusion**: 2.6s with CommandBox overhead

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C++         | 5.3          | 1.9         |
| Rust        | 5.5          | 1.9         |
| ARM64 asm   | 5.6          | 1.9         |
| Zig         | 5.7          | 1.9         |
| Go          | 5.9          | 4.0         |
| C           | 6.5          | 1.9         |
| Perl        | 8.4          | 4.6         |
| Common Lisp | 22.2         | 38.4        |
| Bash        | 24.6         | 6.7         |
| Python      | 26.3         | 14.8        |
| Java        | 43.3         | 48.1        |
| PHP         | 49.3         | 25.8        |
| Node.js     | 51.2         | 40.2        |
| Ruby        | 60.2         | 28.1        |
| Clojure     | 406.7        | 131.2       |
| ColdFusion  | 2,647.0      | 1,157.7     |

## Answers

- **Part 1**: 1524750
- **Part 2**: 1592426537
