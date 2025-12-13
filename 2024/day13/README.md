# Day 13: Claw Contraption

*"Long story short, I survived"* - and so did these claw machines.

## A Note on Lucky Number 13

Today is **December 13th** - Taylor Swift's lucky number and her birthday. In her honor, we celebrate Day 13 of Advent of Code with references to the artist who taught us that sometimes the best solutions are the ones that don't play by the conventional rules.

*"It's me, hi, I'm the problem solver, it's me"*

## Problem Summary

*"We are never ever ever... going to brute force this"*

You find yourself in an arcade full of claw machines - but these aren't your typical carnival games. Each machine has two buttons:
- **Button A**: Costs 3 tokens, moves the claw by (ax, ay)
- **Button B**: Costs 1 token, moves the claw by (bx, by)

The prize sits at coordinates (px, py). You need to press some combination of buttons to land the claw **exactly** on the prize.

### Part 1: Fearless (100 Button Press Limit)

*"I don't know how it gets better than this"*

Find the minimum tokens needed to win all winnable prizes, where each button can be pressed **at most 100 times**.

### Part 2: The Bigger Picture

*"Nothing's gonna change, nothing's gonna change unless you're ready"*

The prizes have moved! Add **10,000,000,000,000** (10^13 - naturally) to both X and Y coordinates of every prize. The 100-press limit is gone.

### Input Format
```
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176
```

## Algorithmic Approach

### The Anti-Hero Solution: Why Brute Force Fails

*"I wake up screaming from dreaming / One day I'll watch as you're leaving"* - your program, if you try brute force.

A naive approach would try all combinations of button presses. For Part 1, that's checking up to 100×100 = 10,000 combinations per machine - slow but survivable.

For Part 2? You'd need to check up to 10^13 × 10^13 = 10^26 combinations. Even at a billion checks per second, that would take longer than the age of the universe.

*"Band-aids don't fix bullet holes"* - and nested loops don't fix exponential complexity.

### The Key Insight: It's a Love Story (of Linear Algebra)

*"Romeo, save me"* - Linear algebra to the rescue!

This is a system of two linear equations with two unknowns:
```
a × ax + b × bx = px    (X-axis equation)
a × ay + b × by = py    (Y-axis equation)
```

Where:
- `a` = number of Button A presses
- `b` = number of Button B presses

### Cramer's Rule: The Algorithm

*"The Algorithm"* - what Swifties call the mysterious force that brings them together at concerts. Our algorithm is more deterministic:

```
det = ax × by - ay × bx

a = (px × by - py × bx) / det
b = (ax × py - ay × px) / det
```

This gives us the **exact** solution in O(1) time per machine!

### Validation Checks

*"Are we out of the woods yet?"* - Not until we verify:

1. **Determinant ≠ 0**: If det = 0, the lines are parallel (no unique solution)
2. **Integer solutions**: Both `a` and `b` must divide evenly (no fractional button presses!)
3. **Non-negative**: Can't press a button negative times
4. **Within bounds** (Part 1 only): Neither `a` nor `b` exceeds 100

### Complexity

*"It's a new soundtrack, I could dance to this beat forevermore"*

- **Time**: O(n) where n = number of machines
- **Space**: O(1) - just arithmetic operations
- **Big Integer Requirement**: Part 2 needs 64-bit integers (or arbitrary precision)

## Programming Techniques Highlighted

### Core Concepts

*"We're happy, free, confused, and lonely at the same time"* - but our code shouldn't be.

- **Linear Algebra**: Solving 2×2 systems with Cramer's rule
- **Integer Arithmetic**: No floating point! Check divisibility before dividing
- **Big Integer Handling**: Part 2's coordinates exceed 32-bit limits

### Mathematical Properties

*"Magic, madness, heaven, sin"* - or just determinants:

- The determinant `ax × by - ay × bx` tells us if a unique solution exists
- When det = 0, the button movements are parallel (linearly dependent)
- Integer division requires checking `numerator % denominator == 0`

### Data Parsing

*"All you had to do was stay"* - at consistent regex patterns:

```regex
Button A: X\+(\d+), Y\+(\d+)
Button B: X\+(\d+), Y\+(\d+)
Prize: X=(\d+), Y=(\d+)
```

## Language-Specific Notes

### The Eras Tour of Implementations

#### Systems Languages Era (Fearless Performance)

*"I knew you were trouble when you walked in"* - said no one about these speeds.

- **C (6.0ms)**: Direct, no nonsense, like "Mean"
- **ARM64 Assembly (6.5ms)**: The most stripped-down version, like an acoustic set
- **Zig (6.6ms)**: Modern and safe, like the re-recordings
- **Rust (7.2ms)**: Memory safe banger, "Safe & Sound" personified
- **C++ (8.9ms)**: Classic with modern touches, "Love Story (Taylor's Version)"

#### Compiled Languages Era (Speak Now)

- **Go (8.6ms)**: Simple and effective, speaks for itself
- **Perl (10.7ms)**: *"I knew you were trouble"* - but it's actually pretty fast!

#### Interpreted Era (Red)

- **Python (32.1ms)**: Clean and readable, "All Too Well (10 Minute Version)" of code
- **Node.js (46.5ms)**: BigInt required for Part 2, handling the "22" trillion offset
- **PHP (62.2ms)**: Gets the job done, no drama
- **Ruby (67.1ms)**: Elegant syntax, *"enchanted to meet you"*
- **Java (62.9ms)**: Enterprise-ready, like stadium tour production

#### JVM Functional Era (folklore)

- **Common Lisp (27.4ms)**: *"this is me trying"* - with parentheses
- **Clojure (505.8ms)**: JVM startup time is the real *"exile"*

#### Vintage Era (evermore)

- **Bash (15,336ms)**: *"no body, no crime"* - just bc for big integers
- **ColdFusion (2,818ms)**: The deep cut, *"right where you left me"* in 2005

### Big Integer Notes

*"We were both young when I first saw you"* - and 32-bit integers when computers first saw Part 2's numbers.

| Language | Big Integer Approach |
|----------|---------------------|
| Python | Native (transparent) |
| Node.js | `BigInt` type |
| Java | `BigInteger` class |
| Ruby | Native (automatic) |
| PHP | `gmp` functions |
| Perl | Native (automatic) |
| Bash | `bc` calculator |
| Common Lisp | Native |
| C/Rust/Zig | `int64_t`/`i64` sufficient |

## Benchmarks

*"Look what you made me do"* - benchmark everything!

| Language | Runtime (ms) | Memory (MB) |
|----------|--------------|-------------|
| C | 6.0 | 1.9 |
| ARM64 asm | 6.5 | 1.9 |
| Zig | 6.6 | 1.9 |
| Rust | 7.2 | 1.9 |
| Go | 8.6 | 4.7 |
| C++ | 8.9 | 1.9 |
| Perl | 10.7 | 4.6 |
| Common Lisp | 27.4 | 41.2 |
| Python | 32.1 | 15.8 |
| Node.js | 46.5 | 40.9 |
| PHP | 62.2 | 24.8 |
| Java | 62.9 | 49.6 |
| Ruby | 67.1 | 28.4 |
| Clojure | 505.8 | 133.8 |
| ColdFusion | 2,818.2 | 1126.3 |
| Bash | 15,335.6 | 6.6 |

## The Tortured Poet's Solution

```python
def solve_machine(ax, ay, bx, by, px, py):
    """
    I've been the archer, I've been the prey
    Who could ever leave me, darling?
    But who could stay? (The determinant knows)
    """
    det = ax * by - ay * bx

    if det == 0:
        return None  # 'tis the damn season for no solution

    a = (px * by - py * bx) // det
    b = (ax * py - ay * px) // det

    # Check if we're Out Of The Woods
    if (px * by - py * bx) % det != 0:
        return None
    if (ax * py - ay * px) % det != 0:
        return None
    if a < 0 or b < 0:
        return None

    return 3 * a + b  # The cost of Fearless pursuit
```

## Answers

*"And in the end, in Wonderland, we both went mad"* - but we got the right answers:

- **Part 1**: 35997
- **Part 2**: 82510994362072

## Dedication

*"And I'm just like, damn, it's 13:00"*

Happy Birthday, Taylor Swift! On this December 13th, your lucky number guided us through claw machines with mathematical elegance.

Like your re-recordings taught us: sometimes the best solution isn't brute force - it's doing it smarter, with precision and style.

*"I had the time of my life fighting dragons with you"* - and solving linear equations.

---

*"This is me trying"* - to make Advent of Code fun since Day 1.

🎂 **Happy 35th Birthday, Taylor!** 🎂

*Written with love on December 13, 2024*
