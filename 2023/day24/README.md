# Day 24: Never Tell Me The Odds

## Problem Summary

Hailstones are flying through 3D space with constant linear velocities. Each hailstone has a position `(px, py, pz)` and velocity `(vx, vy, vz)`. At time `t`, a hailstone is at position `(px + t*vx, py + t*vy, pz + t*vz)`.

**Input format**: Lines of `px, py, pz @ vx, vy, vz`

## Part 1: 2D Path Intersections

Ignoring the Z axis, count how many pairs of hailstones have paths that intersect within a test area (X and Y both between 200,000,000,000,000 and 400,000,000,000,000), considering only **future** intersections (positive time for both hailstones).

### Algorithm

For each pair of hailstones, solve the 2D line intersection:
- Line 1: `P1 + t1 * V1`
- Line 2: `P2 + t2 * V2`

Using Cramer's rule on the system:
```
vx1*t1 - vx2*t2 = px2 - px1
vy1*t1 - vy2*t2 = py2 - py1
```

Check: `det = vx1*(-vy2) - (-vx2)*vy1 != 0` (not parallel), `t1 >= 0`, `t2 >= 0`, and intersection point within bounds.

### Complexity
- Time: O(n²) for n hailstones (~300 hailstones = ~45,000 pairs)
- Space: O(n)

## Part 2: The Perfect Throw

Find a rock starting position `(rx, ry, rz)` and velocity `(rvx, rvy, rvz)` such that the rock will hit **every** hailstone at some positive time. Return `rx + ry + rz`.

### Key Insight

For each hailstone `i`, at collision time `ti`:
```
rx + ti*rvx = pxi + ti*vxi
ry + ti*rvy = pyi + ti*vyi
rz + ti*rvz = pzi + ti*vzi
```

This gives 9 unknowns (rx, ry, rz, rvx, rvy, rvz, t0, t1, t2) with 9 equations from 3 hailstones. However, these equations are **nonlinear** in the unknowns (products like `ti*rvx`).

### The Linearization Trick

Rearranging the X-Y equations:
```
(rx - pxi) * (vyi - rvy) = (ry - pyi) * (vxi - rvx)
```

Expanding and subtracting equations for two different hailstones **cancels the nonlinear terms** (products of unknowns), yielding:
```
(vy1 - vy2)*rx + (vx2 - vx1)*ry + (py2 - py1)*rvx + (px1 - px2)*rvy =
    px1*vy1 - py1*vx1 - (px2*vy2 - py2*vx2)
```

This is **linear** in `rx, ry, rvx, rvy`. Four such equations (from 5 hailstones) give a 4x4 linear system solvable by Gaussian elimination. Repeat for the X-Z plane to get `rz, rvz`.

### Numerical Considerations

Positions are ~10^14, requiring careful handling:
- **Python**: `fractions.Fraction` for exact rational arithmetic
- **C/C++/Rust/Zig**: Double-precision floating-point (15-16 significant digits)
- **ARM64 Assembly**: Double-precision FP registers (`d0-d16`)
- **Java**: `BigInteger` for exact integer arithmetic
- **ColdFusion**: Java BigInteger via CFML's Java interop (with care to avoid precision loss in string conversions)

### Complexity
- Time: O(1) - only uses first 5 hailstones, O(n) Gaussian elimination
- Space: O(1) - constant-size matrices

## Programming Techniques Highlighted

1. **Linear Algebra**: Gaussian elimination with partial pivoting
2. **Parametric Line Intersection**: Solving for intersection time
3. **Equation Manipulation**: Linearizing nonlinear systems by clever subtraction
4. **Arbitrary Precision Arithmetic**: Required for exact solutions with large coordinates

## Language-Specific Notes

- **Fast performers (C, C++, Rust, Zig, ARM64)**: ~7-9ms using floating-point arithmetic
- **Node.js**: Surprisingly fast at 56ms, benefits from V8 JIT
- **Python**: 63ms, `fractions.Fraction` provides exact arithmetic
- **Java**: 68ms including JVM startup, uses `BigInteger`
- **Bash**: 22 seconds - uses `bc` for arbitrary precision, shell overhead significant
- **ColdFusion**: 7.5 seconds - requires careful handling of Java BigInteger to avoid Lucee's precision-losing string conversions

## Benchmarks

| Language | Runtime (ms) | Memory (MB) |
|----------|--------------|-------------|
| C | 7.1 | 1.9 |
| C++ | 7.6 | 1.9 |
| ARM64 | 7.8 | 1.9 |
| Rust | 8.5 | 1.9 |
| Zig | 8.8 | 1.9 |
| Node.js | 56.1 | 45.8 |
| Python | 62.7 | 15.7 |
| Java | 68.5 | 49.9 |
| PHP | 78.6 | 26.4 |
| Common Lisp | 87.6 | 71.5 |
| Ruby | 92.3 | 28.2 |
| Go | 99.7 | 26.8 |
| Perl | 110.3 | 17.6 |
| Clojure | 604.3 | 294.8 |
| ColdFusion | 7550.1 | 1129.6 |
| Bash | 22092.1 | 268.5 |

## Answers

- Part 1: **27732**
- Part 2: **641619849766168**
