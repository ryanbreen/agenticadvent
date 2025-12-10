#!/usr/bin/env bash
set -euo pipefail

# Day 10: Factory - Button press minimization
# Part 1: XOR toggle problem (brute force over GF(2))
# Part 2: Integer linear programming via Gaussian elimination with Python helper

INPUT_FILE="../input.txt"

# Count number of set bits in a number
count_bits() {
    local n=$1
    local count=0
    while ((n > 0)); do
        ((count += n & 1))
        ((n >>= 1))
    done
    echo "$count"
}

# Solve Part 1 for a single machine using brute force
solve_machine_part1() {
    local n_lights=$1
    local target=$2
    shift 2
    local buttons=("$@")

    local n_buttons=${#buttons[@]}
    local min_presses=999999

    # Try all 2^n_buttons combinations
    local max_mask=$((1 << n_buttons))
    for ((mask=0; mask<max_mask; mask++)); do
        local state=0
        local presses=0

        for ((i=0; i<n_buttons; i++)); do
            if ((mask & (1 << i))); then
                state=$((state ^ buttons[i]))
                ((presses++))
            fi
        done

        if ((state == target)); then
            if ((presses < min_presses)); then
                min_presses=$presses
            fi
        fi
    done

    echo "$min_presses"
}

# Part 1: Solve all machines
part1() {
    local total=0

    while IFS= read -r line; do
        [[ -z "$line" ]] && continue

        # Extract indicator pattern [.##.] etc
        local indicator=$(echo "$line" | sed -n 's/.*\[\([.#]*\)\].*/\1/p')
        local n_lights=${#indicator}

        # Build target state (bit positions where # appears)
        local target=0
        for ((i=0; i<n_lights; i++)); do
            if [[ "${indicator:$i:1}" == "#" ]]; then
                target=$((target | (1 << i)))
            fi
        done

        # Extract button patterns (x,y,z) and convert to bitmasks
        local buttons=()
        # Use sed to extract all parenthesized patterns
        local patterns=$(echo "$line" | sed 's/[^(]*(\([0-9,]*\))/\1\n/g' | grep '^[0-9]')

        while IFS= read -r pattern; do
            [[ -z "$pattern" ]] && continue
            # Convert (1,2,3) to bitmask
            local mask=0
            IFS=',' read -ra indices <<< "$pattern"
            for idx in "${indices[@]}"; do
                mask=$((mask | (1 << idx)))
            done
            buttons+=("$mask")
        done <<< "$patterns"

        if ((${#buttons[@]} > 0)); then
            local min_presses
            min_presses=$(solve_machine_part1 "$n_lights" "$target" "${buttons[@]}")
            total=$((total + min_presses))
        fi
    done < "$INPUT_FILE"

    echo "$total"
}

# Part 2: Use Python for complex matrix operations
part2() {
    python3 - <<'PYEOF'
import sys
import re
from fractions import Fraction
import math

input_file = "../input.txt"

def solve_machine_part2(line):
    """Solve Part 2 for a single machine using Gaussian elimination."""
    # Extract joltage requirements
    joltage_match = re.search(r'\{([0-9,]+)\}', line)
    joltage = [int(x) for x in joltage_match.group(1).split(',')]
    n_counters = len(joltage)

    # Extract button indices
    buttons = []
    for match in re.finditer(r'\(([0-9,]+)\)', line):
        indices = [int(x) for x in match.group(1).split(',')]
        buttons.append(indices)

    n_buttons = len(buttons)

    if n_buttons == 0:
        return 0 if all(j == 0 for j in joltage) else 999999

    # Build matrix A (n_counters x n_buttons)
    A = [[Fraction(0)] * n_buttons for _ in range(n_counters)]
    for j, indices in enumerate(buttons):
        for idx in indices:
            if idx < n_counters:
                A[idx][j] = Fraction(1)

    b = [Fraction(j) for j in joltage]

    # Augmented matrix [A | b]
    aug = [A[i] + [b[i]] for i in range(n_counters)]

    # Gaussian elimination with partial pivoting
    pivot_cols = []
    pivot_row = 0

    for col in range(n_buttons):
        # Find non-zero entry in this column
        found = -1
        for row in range(pivot_row, n_counters):
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
        for c in range(n_buttons + 1):
            aug[pivot_row][c] /= scale

        # Eliminate column in other rows
        for row in range(n_counters):
            if row != pivot_row and aug[row][col] != 0:
                factor = aug[row][col]
                for c in range(n_buttons + 1):
                    aug[row][c] -= factor * aug[pivot_row][c]

        pivot_row += 1

    # Check for inconsistency
    for row in range(pivot_row, n_counters):
        if aug[row][n_buttons] != 0:
            return 999999

    # Identify free variables
    pivot_col_set = set(col for col, _ in pivot_cols)
    free_vars = [c for c in range(n_buttons) if c not in pivot_col_set]

    # Extract particular solution
    particular = [Fraction(0)] * n_buttons
    for col, row in pivot_cols:
        particular[col] = aug[row][n_buttons]

    # Extract null space vectors
    null_vectors = []
    for fv in free_vars:
        vec = [Fraction(0)] * n_buttons
        vec[fv] = Fraction(1)
        for col, row in pivot_cols:
            vec[col] = -aug[row][fv]
        null_vectors.append(vec)

    # Search for optimal solution
    max_j = max(joltage) if joltage else 100
    min_total = 999999

    if not free_vars:
        # Unique solution
        total = 0
        for val in particular:
            if val < 0 or val.denominator != 1:
                return 999999
            total += int(val)
        return total

    # With free variables, search
    if len(free_vars) == 1:
        # 1D search
        t_low = float('-inf')
        t_high = float('inf')

        for j in range(n_buttons):
            p = particular[j]
            nv = null_vectors[0][j]

            if nv == 0:
                if p < 0:
                    return 999999
            elif nv > 0:
                t_low = max(t_low, -p / nv)
            else:
                t_high = min(t_high, -p / nv)

        if t_low > t_high:
            return 999999

        t_low_int = math.ceil(float(t_low))
        t_high_int = math.floor(float(t_high))

        for t in range(t_low_int, min(t_high_int + 1, t_low_int + 10000)):
            t_frac = Fraction(t)
            valid = True
            total = 0
            for j in range(n_buttons):
                val = particular[j] + t_frac * null_vectors[0][j]
                if val < 0 or val.denominator != 1:
                    valid = False
                    break
                total += int(val)
            if valid:
                min_total = min(min_total, total)

    elif len(free_vars) == 2:
        # 2D search
        bound = max_j * 2
        for t0 in range(-bound, bound + 1):
            intermediate = [particular[j] + t0 * null_vectors[0][j] for j in range(n_buttons)]

            t1_low = float('-inf')
            t1_high = float('inf')
            for j in range(n_buttons):
                p = float(intermediate[j])
                nv = float(null_vectors[1][j])
                if nv > 0:
                    t1_low = max(t1_low, -p / nv)
                elif nv < 0:
                    t1_high = min(t1_high, -p / nv)

            t1_low_int = math.ceil(t1_low)
            t1_high_int = math.floor(t1_high)

            for t1 in range(t1_low_int, t1_high_int + 1):
                valid = True
                total = 0
                for j in range(n_buttons):
                    val = intermediate[j] + t1 * null_vectors[1][j]
                    if val < 0 or val.denominator != 1:
                        valid = False
                        break
                    total += int(val)
                if valid:
                    min_total = min(min_total, total)

    elif len(free_vars) == 3:
        # 3D search
        bound = max_j
        for t0 in range(-bound, bound + 1):
            inter0 = [particular[j] + t0 * null_vectors[0][j] for j in range(n_buttons)]

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
                inter1 = [inter0[j] + t1 * null_vectors[1][j] for j in range(n_buttons)]

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
                    valid = True
                    total = 0
                    for j in range(n_buttons):
                        val = inter1[j] + t2 * null_vectors[2][j]
                        if val < 0 or val.denominator != 1:
                            valid = False
                            break
                        total += int(val)
                    if valid:
                        min_total = min(min_total, total)

    else:
        # Higher dimensional - use bounded recursive search
        def search(idx, partial):
            nonlocal min_total
            if idx == len(free_vars):
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

            t_low = float('-inf')
            t_high = float('inf')
            for j in range(n_buttons):
                p = float(partial[j])
                nv = float(null_vectors[idx][j])
                if nv > 0:
                    t_low = max(t_low, -p / nv)
                elif nv < 0:
                    t_high = min(t_high, -p / nv)

            if t_low > t_high:
                return

            t_low_int = max(math.ceil(t_low) - max_j, -max_j * 2)
            t_high_int = min(math.floor(t_high) + max_j, max_j * 2)

            for t in range(t_low_int, t_high_int + 1):
                new_partial = [partial[j] + t * null_vectors[idx][j] for j in range(n_buttons)]
                search(idx + 1, new_partial)

        search(0, particular[:])

    return min_total if min_total < 999999 else 0

# Main Part 2 solver
total = 0
with open(input_file, 'r') as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        min_presses = solve_machine_part2(line)
        total += min_presses

print(total)
PYEOF
}

# Main execution
main() {
    echo "Part 1: $(part1)"
    echo "Part 2: $(part2)"
}

main
