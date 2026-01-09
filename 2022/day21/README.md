# Day 21: Monkey Math

## Problem Summary

Monkeys are playing a number-yelling game. Each monkey either yells a specific number or waits for two other monkeys, then performs a math operation on their values.

**Part 1**: Evaluate the expression tree starting from 'root' to find what value it yells.

**Part 2**: The 'humn' monkey (you!) needs to yell a specific value to make root's two children equal. Find that value.

## Input Format

Each line defines a monkey's job:
```
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
```

- Number monkeys: `name: 5`
- Operation monkeys: `name: left + right`

Operations: `+`, `-`, `*`, `/`

## Algorithmic Approach

### Part 1: Expression Tree Evaluation

Simple recursive evaluation with memoization:
1. If the monkey has a number, return it
2. Otherwise, recursively evaluate both operands and apply the operator

### Part 2: Symbolic Equation Solving

Work backwards from the root to find what `humn` must be:
1. Determine which child of `root` contains `humn` in its subtree
2. Evaluate the other child to get the target value
3. Recursively solve: if this node should equal `target`, what must the child containing `humn` equal?

**Inverse operations:**
- If `left + right = target` and we need left: `left = target - right`
- If `left - right = target` and we need right: `right = left - target`
- If `left * right = target` and we need left: `left = target / right`
- If `left / right = target` and we need right: `right = left / target`

### Complexity

- **Time**: O(n) where n = number of monkeys (each visited once with memoization)
- **Space**: O(n) for memoization and recursion stack

## Programming Techniques Highlighted

- **Expression tree evaluation**: Recursive traversal with lazy evaluation
- **Symbolic algebra**: Inverting operations to solve for unknowns
- **Memoization**: Caching results to avoid recomputation
- **Hash tables**: Storing monkey definitions for O(1) lookup

## Language-Specific Notes

### Performance Characteristics

This problem is I/O and hash-table bound, making scripting languages surprisingly competitive:

**Very fast (< 30ms)**:
- **Zig**: 5.6ms - extremely efficient hash map
- **Perl**: 18.6ms - excellent hash table performance
- **Common Lisp**: 28.7ms - optimized hash tables
- **Python**: 29.0ms - dict operations are fast

**Fast (30-100ms)**:
- **Node.js**: 50.6ms - V8's Map optimization
- **ARM64 asm**: 61.1ms - hand-coded hash implementation
- **Ruby**: 66.6ms - good hash performance
- **PHP**: 73.4ms - associative arrays
- **C**: 85.4ms - custom hash table overhead

**Moderate (100-500ms)**:
- **Go**: 139.3ms - map operations
- **Rust**: 313.8ms - HashMap overhead
- **Java**: 368.3ms - HashMap wrapper objects
- **Clojure**: 417.9ms - persistent maps
- **C++**: 512.5ms - unordered_map overhead

**Slow (> 1s)**:
- **ColdFusion**: 2,409.1ms - struct operations
- **Bash**: 13,984.1ms - associative array limitations

### Notable Observations

- Scripting languages excel because the problem is algorithmically simple
- Zig's hash map implementation is exceptionally fast
- C and C++ are slower than expected due to hash table implementation overhead
- Compiled languages don't have a significant advantage for tree traversal

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 5.6          | 1.7         |
| Perl        | 18.6         | 5.8         |
| Common Lisp | 28.7         | 42.8        |
| Python      | 29.0         | 15.4        |
| Node.js     | 50.6         | 44.6        |
| ARM64 asm   | 61.1         | 35.1        |
| Ruby        | 66.6         | 28.6        |
| PHP         | 73.4         | 26.7        |
| C           | 85.4         | 47.5        |
| Go          | 139.3        | 67.3        |
| Rust        | 313.8        | 119.9       |
| Java        | 368.3        | 115.8       |
| Clojure     | 417.9        | 136.3       |
| C++         | 512.5        | 129.9       |
| ColdFusion  | 2,409.1      | 1,123.2     |
| Bash        | 13,984.1     | 7.3         |

## Answers

- Part 1: 282285213953670
- Part 2: 3699945358564
