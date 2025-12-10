#!/usr/bin/env python3
"""Day 10: Factory - Linear algebra over GF(2) to minimize button presses."""

import re
from pathlib import Path


def parse_line(line):
    """Parse a machine line into target state and button masks."""
    # Extract indicator pattern [.##.]
    indicator_match = re.search(r'\[([.#]+)\]', line)
    indicator = indicator_match.group(1)
    n_lights = len(indicator)

    # Target state: 1 where # appears
    target = 0
    for i, c in enumerate(indicator):
        if c == '#':
            target |= (1 << i)

    # Extract button schematics (0,1,2) etc.
    buttons = []
    for match in re.finditer(r'\(([0-9,]+)\)', line):
        indices = [int(x) for x in match.group(1).split(',')]
        mask = 0
        for idx in indices:
            mask |= (1 << idx)
        buttons.append((mask, indices))

    return n_lights, target, buttons


def parse_line_part2(line):
    """Parse a machine line for Part 2 - extract joltage requirements."""
    # Extract joltage requirements {3,5,4,7}
    joltage_match = re.search(r'\{([0-9,]+)\}', line)
    joltage = [int(x) for x in joltage_match.group(1).split(',')]
    n_counters = len(joltage)

    # Extract button schematics (0,1,2) etc.
    buttons = []
    for match in re.finditer(r'\(([0-9,]+)\)', line):
        indices = [int(x) for x in match.group(1).split(',')]
        buttons.append(indices)

    return n_counters, joltage, buttons


def count_bits(n):
    """Count number of 1 bits in n."""
    count = 0
    while n:
        count += n & 1
        n >>= 1
    return count


def solve_machine_brute(n_lights, target, buttons):
    """Brute force: try all combinations of button presses (0 or 1 each)."""
    n_buttons = len(buttons)
    min_presses = float('inf')

    # Extract just the masks for Part 1
    button_masks = [b[0] for b in buttons]

    # Try all 2^n_buttons combinations
    for mask in range(1 << n_buttons):
        state = 0
        presses = 0
        for i in range(n_buttons):
            if mask & (1 << i):
                state ^= button_masks[i]
                presses += 1

        if state == target:
            min_presses = min(min_presses, presses)

    return min_presses if min_presses != float('inf') else 0


def gaussian_elimination_gf2(matrix, target, n_cols):
    """
    Gaussian elimination over GF(2).
    matrix: list of (row_bits, button_index) tuples
    Returns the reduced matrix and pivot information.
    """
    rows = list(matrix)  # Copy
    n_rows = len(rows)

    pivot_row = 0
    pivots = []  # (column, row_index) for each pivot

    for col in range(n_cols):
        # Find a row with 1 in this column
        found = -1
        for r in range(pivot_row, n_rows):
            if rows[r][0] & (1 << col):
                found = r
                break

        if found == -1:
            continue  # No pivot in this column

        # Swap to pivot position
        rows[pivot_row], rows[found] = rows[found], rows[pivot_row]
        pivots.append((col, pivot_row))

        # Eliminate this column from all other rows
        for r in range(n_rows):
            if r != pivot_row and rows[r][0] & (1 << col):
                rows[r] = (rows[r][0] ^ rows[pivot_row][0], rows[r][1] ^ rows[pivot_row][1])

        pivot_row += 1

    return rows, pivots


def solve_machine_gauss(n_lights, target, buttons):
    """
    Solve using Gaussian elimination over GF(2).
    Find minimum weight solution.
    """
    n_buttons = len(buttons)

    if n_buttons == 0:
        return 0 if target == 0 else float('inf')

    # Build augmented matrix: each row is (button_mask, button_index_as_bitmask)
    # button_index_as_bitmask tracks which original buttons are combined
    matrix = [(buttons[i], 1 << i) for i in range(n_buttons)]

    # Apply Gaussian elimination
    rows, pivots = gaussian_elimination_gf2(matrix, target, n_lights)

    # Check if solution exists
    # The free variables are columns not in pivots
    pivot_cols = set(col for col, _ in pivots)
    free_cols = [i for i in range(n_buttons) if i not in pivot_cols]

    # Build the solution by back-substitution
    # We need to find which buttons to press to get target
    # Start from target, then for each pivot column, determine if that button is needed

    min_presses = float('inf')

    # Try all combinations of free variables
    n_free = len(free_cols)

    # For efficiency, limit brute force on free variables
    if n_free > 20:
        # Fallback to simpler brute force if too many free vars
        return solve_machine_brute(n_lights, target, buttons)

    for free_mask in range(1 << n_free):
        # Determine the solution with these free variable choices
        solution = 0

        # Set free variables according to free_mask
        for i, col in enumerate(free_cols):
            if free_mask & (1 << i):
                solution |= (1 << col)

        # Compute what state we get from just free variables
        state = 0
        for i in range(n_buttons):
            if solution & (1 << i):
                state ^= buttons[i]

        # Now we need to use pivot buttons to fix remaining bits
        remaining = target ^ state

        # For each pivot, check if we need that button
        valid = True
        for col, row_idx in reversed(pivots):
            row_mask, button_combo = rows[row_idx]

            # Check if this row affects 'remaining'
            if remaining & (1 << col):
                # We need to "press" this row's button combination
                # This means XORing the button_combo into our solution
                solution ^= button_combo
                remaining ^= row_mask

        if remaining == 0:
            presses = count_bits(solution)
            min_presses = min(min_presses, presses)

    return min_presses if min_presses != float('inf') else 0


def solve_machine(n_lights, target, buttons):
    """Solve a single machine - use appropriate method based on size."""
    n_buttons = len(buttons)

    if n_buttons <= 20:
        # Small enough for brute force
        return solve_machine_brute(n_lights, target, buttons)
    else:
        # Use Gaussian elimination for larger problems
        return solve_machine_gauss(n_lights, target, buttons)


def part1(lines):
    """Find minimum total button presses for all machines."""
    total = 0
    for line in lines:
        line = line.strip()
        if not line:
            continue
        n_lights, target, buttons = parse_line(line)
        min_presses = solve_machine(n_lights, target, buttons)
        total += min_presses
    return total


def solve_machine_part2(n_counters, joltage, buttons):
    """
    Solve Part 2: minimize button presses to reach target joltage levels.
    This is an integer linear programming problem: min sum(x) s.t. Ax = b, x >= 0.

    Key insight: We can solve this column by column using Gaussian elimination
    and then find optimal non-negative integer solution.
    """
    from fractions import Fraction

    n_buttons = len(buttons)

    if n_buttons == 0:
        return 0 if all(j == 0 for j in joltage) else float('inf')

    # Build matrix A (n_counters x n_buttons)
    A = [[Fraction(0)] * n_buttons for _ in range(n_counters)]
    for j, indices in enumerate(buttons):
        for idx in indices:
            if idx < n_counters:
                A[idx][j] = Fraction(1)

    b = [Fraction(j) for j in joltage]

    # Augmented matrix [A | b]
    aug = [row[:] + [b[i]] for i, row in enumerate(A)]
    n_rows = n_counters
    n_cols = n_buttons

    # Gaussian elimination with partial pivoting
    pivot_cols = []
    pivot_row = 0

    for col in range(n_cols):
        # Find non-zero entry in this column
        found = -1
        for row in range(pivot_row, n_rows):
            if aug[row][col] != 0:
                found = row
                break

        if found == -1:
            continue

        # Swap rows
        aug[pivot_row], aug[found] = aug[found], aug[pivot_row]
        pivot_cols.append((col, pivot_row))

        # Scale pivot row
        scale = aug[pivot_row][col]
        for c in range(n_cols + 1):
            aug[pivot_row][c] /= scale

        # Eliminate column in other rows
        for row in range(n_rows):
            if row != pivot_row and aug[row][col] != 0:
                factor = aug[row][col]
                for c in range(n_cols + 1):
                    aug[row][c] -= factor * aug[pivot_row][c]

        pivot_row += 1

    # Check for inconsistency (rows with all zeros but non-zero RHS)
    for row in range(pivot_row, n_rows):
        if aug[row][n_cols] != 0:
            return float('inf')  # No solution

    # Now we have RREF form
    # Identify free variables (columns not in pivot_cols)
    pivot_col_set = set(col for col, _ in pivot_cols)
    free_vars = [c for c in range(n_cols) if c not in pivot_col_set]

    # If no free variables, we have a unique solution
    if not free_vars:
        # Extract solution
        solution = [Fraction(0)] * n_buttons
        for col, row in pivot_cols:
            solution[col] = aug[row][n_cols]

        # Check non-negative integers
        total = 0
        for val in solution:
            if val < 0 or val.denominator != 1:
                return float('inf')
            total += int(val)
        return total

    # With free variables, we need to search for optimal solution
    # The general solution is: x = x_particular + sum(t_i * null_vector_i)
    # where t_i are the free variable values

    # For each free variable, extract the null space vector
    null_vectors = []
    for fv in free_vars:
        vec = [Fraction(0)] * n_buttons
        vec[fv] = Fraction(1)
        for col, row in pivot_cols:
            # The coefficient of free var 'fv' in the equation for pivot var 'col'
            vec[col] = -aug[row][fv]
        null_vectors.append(vec)

    # Extract particular solution (with free vars = 0)
    particular = [Fraction(0)] * n_buttons
    for col, row in pivot_cols:
        particular[col] = aug[row][n_cols]

    # Now search for optimal non-negative integer solution
    # x = particular + sum(t_i * null_vectors[i])
    # We want all x[j] >= 0 and integer, minimize sum(x)

    # Bound the search: each t_i should be bounded
    # For each component j: particular[j] + sum(t_i * null_vectors[i][j]) >= 0 and <= max_needed

    # Find bounds for each t_i
    # This is still potentially expensive, but usually the null space is small

    n_free = len(free_vars)

    # For each button j, we have: x_j = particular[j] + sum(t_i * null_vectors[i][j])
    # Constraint: x_j >= 0 for all j
    # This gives: sum(t_i * null_vectors[i][j]) >= -particular[j]

    # Compute tighter bounds using linear constraints
    # For one free variable, we can solve directly
    if n_free == 1:
        # x_j = particular[j] + t * null_vectors[0][j] >= 0
        # If null_vectors[0][j] > 0: t >= -particular[j] / null_vectors[0][j]
        # If null_vectors[0][j] < 0: t <= -particular[j] / null_vectors[0][j]
        # If null_vectors[0][j] == 0: need particular[j] >= 0

        t_low = float('-inf')
        t_high = float('inf')

        for j in range(n_buttons):
            p = particular[j]
            nv = null_vectors[0][j]

            if nv == 0:
                if p < 0:
                    return float('inf')  # No valid solution
            elif nv > 0:
                bound = -p / nv
                t_low = max(t_low, bound)
            else:  # nv < 0
                bound = -p / nv
                t_high = min(t_high, bound)

        if t_low > t_high:
            return float('inf')

        # t must be integer
        t_low_int = int(t_low) if t_low == int(t_low) else int(t_low) + 1 if t_low > 0 else int(t_low)
        if t_low > 0 and t_low != int(t_low):
            t_low_int = int(t_low) + 1
        else:
            import math
            t_low_int = math.ceil(float(t_low))

        t_high_int = int(t_high) if t_high == int(t_high) else int(t_high) if t_high > 0 else int(t_high) - 1
        import math
        t_high_int = math.floor(float(t_high))

        min_total = float('inf')
        for t in range(t_low_int, t_high_int + 1):
            t_frac = Fraction(t)
            total = 0
            valid = True
            for j in range(n_buttons):
                val = particular[j] + t_frac * null_vectors[0][j]
                if val < 0 or val.denominator != 1:
                    valid = False
                    break
                total += int(val)
            if valid:
                min_total = min(min_total, total)

        return min_total if min_total != float('inf') else 0

    # For multiple free variables, use brute force search with smart bounds
    # The constraints couple all variables together, so we must search jointly
    import math

    max_j = max(joltage) if joltage else 100

    min_total = float('inf')

    # Compute bounds for each free variable independently first
    # For each free var i, considering only null_vectors[i]:
    # x_j = particular[j] + t_i * null_vectors[i][j] >= 0
    bounds = []
    for i in range(n_free):
        t_low = float('-inf')
        t_high = float('inf')
        for j in range(n_buttons):
            p = float(particular[j])
            nv = float(null_vectors[i][j])
            if nv > 0:
                # t_i >= -p / nv
                t_low = max(t_low, -p / nv)
            elif nv < 0:
                # t_i <= -p / nv
                t_high = min(t_high, -p / nv)
        # Expand bounds a bit since other free vars affect the constraints
        t_low = max(-max_j * 2, math.floor(t_low - max_j))
        t_high = min(max_j * 2, math.ceil(t_high + max_j))
        bounds.append((t_low, t_high))

    if n_free == 2:
        # 2D search with smart inner loop
        t0_low, t0_high = bounds[0]
        for t0 in range(t0_low, t0_high + 1):
            t0_frac = Fraction(t0)
            # Compute intermediate values
            intermediate = [particular[j] + t0_frac * null_vectors[0][j] for j in range(n_buttons)]

            # Compute bounds for t1 given t0
            t1_low_cur = float('-inf')
            t1_high_cur = float('inf')
            for j in range(n_buttons):
                p = float(intermediate[j])
                nv = float(null_vectors[1][j])
                if nv > 0:
                    t1_low_cur = max(t1_low_cur, -p / nv)
                elif nv < 0:
                    t1_high_cur = min(t1_high_cur, -p / nv)

            t1_low_int = math.ceil(t1_low_cur)
            t1_high_int = math.floor(t1_high_cur)

            for t1 in range(t1_low_int, t1_high_int + 1):
                t1_frac = Fraction(t1)
                valid = True
                total = 0
                for j in range(n_buttons):
                    val = intermediate[j] + t1_frac * null_vectors[1][j]
                    if val < 0 or val.denominator != 1:
                        valid = False
                        break
                    total += int(val)
                if valid and total < min_total:
                    min_total = total

        return min_total if min_total != float('inf') else 0

    elif n_free == 3:
        # Specialized 3D search with fixed wide bounds
        # Use widened bounds to account for fractional null vectors
        bound = max_j

        for t0 in range(-bound, bound + 1):
            t0_frac = Fraction(t0)
            # Compute intermediate after t0
            inter0 = [particular[j] + t0_frac * null_vectors[0][j] for j in range(n_buttons)]

            # Compute bounds for t1 given t0
            t1_low = float('-inf')
            t1_high = float('inf')
            for j in range(n_buttons):
                p = float(inter0[j])
                nv = float(null_vectors[1][j])
                if nv > 0:
                    t1_low = max(t1_low, -p / nv - bound)
                elif nv < 0:
                    t1_high = min(t1_high, -p / nv + bound)

            t1_low_int = max(math.ceil(t1_low), -bound)
            t1_high_int = min(math.floor(t1_high), bound)

            for t1 in range(t1_low_int, t1_high_int + 1):
                t1_frac = Fraction(t1)
                inter1 = [inter0[j] + t1_frac * null_vectors[1][j] for j in range(n_buttons)]

                # Compute bounds for t2 given t0, t1
                t2_low = float('-inf')
                t2_high = float('inf')
                for j in range(n_buttons):
                    p = float(inter1[j])
                    nv = float(null_vectors[2][j])
                    if nv > 0:
                        t2_low = max(t2_low, -p / nv)
                    elif nv < 0:
                        t2_high = min(t2_high, -p / nv)

                t2_low_int = math.ceil(t2_low)
                t2_high_int = math.floor(t2_high)

                for t2 in range(t2_low_int, t2_high_int + 1):
                    t2_frac = Fraction(t2)
                    valid = True
                    total = 0
                    for j in range(n_buttons):
                        val = inter1[j] + t2_frac * null_vectors[2][j]
                        if val < 0 or val.denominator != 1:
                            valid = False
                            break
                        total += int(val)
                    if valid and total < min_total:
                        min_total = total

        return min_total if min_total != float('inf') else 0

    elif n_free <= 6:
        # General recursive search with dynamic bounds
        def search_recursive(idx, partial):
            nonlocal min_total

            if idx == n_free:
                # Evaluate solution
                valid = True
                total = 0
                for val in partial:
                    if val < 0 or val.denominator != 1:
                        valid = False
                        break
                    total += int(val)
                if valid:
                    min_total = min(min_total, total)
                return

            # Compute bounds for current free var given partial state
            t_low = float('-inf')
            t_high = float('inf')
            for j in range(n_buttons):
                p = float(partial[j])
                nv = float(null_vectors[idx][j])
                if nv > 0:
                    t_low = max(t_low, -p / nv)
                elif nv < 0:
                    t_high = min(t_high, -p / nv)

            if t_low > t_high or t_low == float('inf') or t_high == float('-inf'):
                return

            # Widen bounds a bit to account for integrality constraints
            t_low_int = max(math.ceil(t_low) - max_j, -max_j * 2)
            t_high_int = min(math.floor(t_high) + max_j, max_j * 2)

            for t in range(t_low_int, t_high_int + 1):
                t_frac = Fraction(t)
                new_partial = [partial[j] + t_frac * null_vectors[idx][j] for j in range(n_buttons)]
                search_recursive(idx + 1, new_partial)

        search_recursive(0, particular[:])
        return min_total if min_total != float('inf') else 0

    return 0  # Fallback for large null spaces


def part2(lines):
    """Find minimum total button presses for joltage configuration."""
    total = 0
    for line in lines:
        line = line.strip()
        if not line:
            continue
        n_counters, joltage, buttons = parse_line_part2(line)
        min_presses = solve_machine_part2(n_counters, joltage, buttons)
        total += min_presses
    return total


def main():
    input_file = Path(__file__).parent.parent / 'input.txt'
    lines = input_file.read_text().strip().split('\n')

    print(f"Part 1: {part1(lines)}")
    print(f"Part 2: {part2(lines)}")


if __name__ == '__main__':
    main()
