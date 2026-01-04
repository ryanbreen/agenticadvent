#!/usr/bin/env python3
import sys
from itertools import combinations

def parse_input(filename):
    """Parse hailstone positions and velocities."""
    hailstones = []
    with open(filename) as f:
        for line in f:
            pos, vel = line.strip().split('@')
            px, py, pz = map(int, pos.split(','))
            vx, vy, vz = map(int, vel.split(','))
            hailstones.append(((px, py, pz), (vx, vy, vz)))
    return hailstones

def find_intersection_2d(h1, h2):
    """
    Find intersection of two hailstone paths in 2D (XY plane).
    Returns (x, y, t1, t2) where t1 and t2 are times for each hailstone.
    Returns None if paths are parallel or don't intersect.
    """
    (px1, py1, _), (vx1, vy1, _) = h1
    (px2, py2, _), (vx2, vy2, _) = h2

    # Line 1: position at time t1 is (px1 + vx1*t1, py1 + vy1*t1)
    # Line 2: position at time t2 is (px2 + vx2*t2, py2 + vy2*t2)
    # At intersection:
    # px1 + vx1*t1 = px2 + vx2*t2
    # py1 + vy1*t1 = py2 + vy2*t2

    # Solve for t1 and t2:
    # vx1*t1 - vx2*t2 = px2 - px1
    # vy1*t1 - vy2*t2 = py2 - py1

    # Using Cramer's rule:
    det = vx1 * (-vy2) - (-vx2) * vy1
    if det == 0:
        return None  # Parallel lines

    dx = px2 - px1
    dy = py2 - py1

    t1 = (dx * (-vy2) - (-vx2) * dy) / det
    t2 = (vx1 * dy - dx * vy1) / det

    # Calculate intersection point
    x = px1 + vx1 * t1
    y = py1 + vy1 * t1

    return (x, y, t1, t2)

def part1(hailstones, min_coord=200000000000000, max_coord=400000000000000):
    """Count intersections within test area, in the future for both hailstones."""
    count = 0

    for h1, h2 in combinations(hailstones, 2):
        result = find_intersection_2d(h1, h2)
        if result is None:
            continue

        x, y, t1, t2 = result

        # Check if intersection is in the future for both hailstones
        if t1 < 0 or t2 < 0:
            continue

        # Check if intersection is within test area
        if min_coord <= x <= max_coord and min_coord <= y <= max_coord:
            count += 1

    return count

def part2(hailstones):
    """
    Find rock position and velocity that hits all hailstones.

    Let rock position be (rx, ry, rz) and velocity be (rvx, rvy, rvz).
    For each hailstone i, there exists time ti >= 0 such that:
    rx + rvx*ti = pxi + vxi*ti
    ry + rvy*ti = pyi + vyi*ti
    rz + rvz*ti = pzi + vzi*ti

    This gives us:
    rx - pxi = ti * (vxi - rvx)
    ry - pyi = ti * (vyi - rvy)
    rz - pzi = ti * (vzi - rvz)

    If we eliminate ti:
    (rx - pxi) * (vyi - rvy) = (ry - pyi) * (vxi - rvx)
    (rx - pxi) * (vzi - rvz) = (rz - pzi) * (vxi - rvx)

    Expand the first equation:
    rx*vyi - rx*rvy - pxi*vyi + pxi*rvy = ry*vxi - ry*rvx - pyi*vxi + pyi*rvx
    rx*vyi - rx*rvy - pxi*vyi + pxi*rvy - ry*vxi + ry*rvx + pyi*vxi - pyi*rvx = 0

    Rearranging to get linear terms in rx, ry, rvx, rvy:
    vyi*rx - vxi*ry - rvy*rx + rvx*ry = pxi*vyi - pyi*vxi - pxi*rvy + pyi*rvx

    The terms -rvy*rx + rvx*ry are nonlinear in our unknowns.
    But if we take TWO hailstones and subtract their equations, these nonlinear terms cancel!

    For hailstones i and j:
    (vyi - vyj)*rx + (-vxi + vxj)*ry + (-pyi + pyj)*rvx + (pxi - pxj)*rvy = pxi*vyi - pyi*vxi - (pxj*vyj - pyj*vxj)

    This is a linear equation in rx, ry, rvx, rvy.
    We need 4 equations (4 pairs of hailstones) to solve for these 4 unknowns.
    Then we can use similar approach for XZ plane to get rz, rvz.
    """
    from fractions import Fraction

    def solve_system(matrix, rhs):
        """Solve system of linear equations using Gaussian elimination with fractions."""
        n = len(matrix)
        # Augment matrix with rhs
        aug = [[Fraction(x) for x in row] + [Fraction(rhs[i])] for i, row in enumerate(matrix)]

        # Forward elimination
        for col in range(n):
            # Find pivot
            max_row = col
            for row in range(col + 1, n):
                if abs(aug[row][col]) > abs(aug[max_row][col]):
                    max_row = row
            aug[col], aug[max_row] = aug[max_row], aug[col]

            if aug[col][col] == 0:
                continue

            # Eliminate column
            for row in range(col + 1, n):
                if aug[row][col] != 0:
                    factor = aug[row][col] / aug[col][col]
                    for j in range(col, n + 1):
                        aug[row][j] -= factor * aug[col][j]

        # Back substitution
        solution = [Fraction(0)] * n
        for i in range(n - 1, -1, -1):
            solution[i] = aug[i][n]
            for j in range(i + 1, n):
                solution[i] -= aug[i][j] * solution[j]
            solution[i] /= aug[i][i]

        return solution

    # Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
    # Take first 5 hailstones to get 4 pairs
    h = hailstones[:5]

    matrix_xy = []
    rhs_xy = []

    for i in range(4):
        (px1, py1, _), (vx1, vy1, _) = h[i]
        (px2, py2, _), (vx2, vy2, _) = h[i+1]

        # Coefficients for rx, ry, rvx, rvy
        a = vy1 - vy2
        b = vx2 - vx1
        c = py2 - py1
        d = px1 - px2
        e = px1*vy1 - py1*vx1 - (px2*vy2 - py2*vx2)

        matrix_xy.append([a, b, c, d])
        rhs_xy.append(e)

    rx, ry, rvx, rvy = solve_system(matrix_xy, rhs_xy)

    # Build system for XZ plane (4 equations, 4 unknowns: rx, rz, rvx, rvz)
    # We already know rx and rvx, so we use them to solve for rz and rvz
    matrix_xz = []
    rhs_xz = []

    for i in range(4):
        (px1, _, pz1), (vx1, _, vz1) = h[i]
        (px2, _, pz2), (vx2, _, vz2) = h[i+1]

        # Same structure as XY but with Z instead of Y
        a = vz1 - vz2
        b = vx2 - vx1
        c = pz2 - pz1
        d = px1 - px2
        e = px1*vz1 - pz1*vx1 - (px2*vz2 - pz2*vx2)

        matrix_xz.append([a, b, c, d])
        rhs_xz.append(e)

    rx2, rz, rvx2, rvz = solve_system(matrix_xz, rhs_xz)

    return int(rx + ry + rz)

def main():
    input_file = sys.argv[1] if len(sys.argv) > 1 else '../input.txt'
    hailstones = parse_input(input_file)

    print(f"Part 1: {part1(hailstones)}")
    print(f"Part 2: {part2(hailstones)}")

if __name__ == '__main__':
    main()
