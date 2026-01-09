#!/bin/bash
# Day 5: Supply Stacks

INPUT_FILE="${1:-../input.txt}"

# Use awk for efficiency - single pass for each part
solve() {
    local part=$1
    awk -v part="$part" '
    BEGIN {
        in_stacks = 1
        num_stacks = 0
    }

    in_stacks && /^ 1/ {
        # Stack number line - determine number of stacks
        num_stacks = NF

        # Reverse the stacks (they were built top-down, we need bottom-up)
        for (s = 1; s <= num_stacks; s++) {
            n = stack_len[s]
            for (i = 1; i <= int(n/2); i++) {
                tmp = stacks[s, i]
                stacks[s, i] = stacks[s, n - i + 1]
                stacks[s, n - i + 1] = tmp
            }
        }
        next
    }

    in_stacks && /\[/ {
        # Stack content line - crates at positions 2, 6, 10, ... (1-indexed in awk)
        for (s = 1; s <= 9; s++) {
            pos = 2 + (s - 1) * 4
            if (pos <= length($0)) {
                c = substr($0, pos, 1)
                if (c != " " && c ~ /[A-Z]/) {
                    stack_len[s]++
                    stacks[s, stack_len[s]] = c
                }
            }
        }
        next
    }

    /^$/ {
        in_stacks = 0
        next
    }

    /^move/ {
        # Parse: move COUNT from FROM to TO
        # $2 = count, $4 = from, $6 = to
        count = int($2)
        from = int($4)
        to = int($6)

        if (part == 1) {
            # Part 1: Move one at a time (reverses order)
            for (i = 1; i <= count; i++) {
                # Pop from source
                crate = stacks[from, stack_len[from]]
                stack_len[from]--
                # Push to destination
                stack_len[to]++
                stacks[to, stack_len[to]] = crate
            }
        } else {
            # Part 2: Move multiple at once (preserves order)
            # Get starting position in source
            start_pos = stack_len[from] - count + 1

            # Copy crates to destination
            for (i = 0; i < count; i++) {
                stack_len[to]++
                stacks[to, stack_len[to]] = stacks[from, start_pos + i]
            }

            # Remove from source
            stack_len[from] -= count
        }
    }

    END {
        result = ""
        for (s = 1; s <= num_stacks; s++) {
            if (stack_len[s] > 0) {
                result = result stacks[s, stack_len[s]]
            }
        }
        print result
    }
    ' "$INPUT_FILE"
}

echo "Part 1: $(solve 1)"
echo "Part 2: $(solve 2)"
