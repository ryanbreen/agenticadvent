#!/usr/bin/env bash

# Day 20: Grove Positioning System
# Circular list mixing algorithm - awk-based for performance

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="${INPUT_FILE:-$SCRIPT_DIR/../input.txt}"

# Use awk for the heavy lifting
solve() {
    local multiplier=$1
    local rounds=$2

    awk -v mult="$multiplier" -v rounds="$rounds" '
    BEGIN {
        n = 0
    }

    # Read all values
    {
        val[n] = $1 * mult
        order[n] = n
        n++
    }

    END {
        mod = n - 1

        # Build position lookup
        for (i = 0; i < n; i++) {
            pos[i] = i
        }

        # Mix rounds times
        for (r = 0; r < rounds; r++) {
            for (orig = 0; orig < n; orig++) {
                curr = pos[orig]
                v = val[orig]

                # Compute move amount (v mod (n-1)), handling negatives
                move = v % mod
                if (move < 0) move += mod

                new_pos = (curr + move) % mod

                if (new_pos == curr) continue

                # Shift elements
                if (new_pos > curr) {
                    for (i = curr; i < new_pos; i++) {
                        next_orig = order[i + 1]
                        order[i] = next_orig
                        pos[next_orig] = i
                    }
                } else {
                    for (i = curr; i > new_pos; i--) {
                        prev_orig = order[i - 1]
                        order[i] = prev_orig
                        pos[prev_orig] = i
                    }
                }

                order[new_pos] = orig
                pos[orig] = new_pos
            }
        }

        # Find zero position
        for (i = 0; i < n; i++) {
            if (val[i] == 0) {
                zero_pos = pos[i]
                break
            }
        }

        # Get grove coordinates
        p1 = (zero_pos + 1000) % n
        p2 = (zero_pos + 2000) % n
        p3 = (zero_pos + 3000) % n

        sum = val[order[p1]] + val[order[p2]] + val[order[p3]]
        printf "%.0f\n", sum
    }
    ' "$INPUT_FILE"
}

echo "Part 1: $(solve 1 1)"
echo "Part 2: $(solve 811589153 10)"
