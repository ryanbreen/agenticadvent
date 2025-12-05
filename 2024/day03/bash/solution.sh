#!/usr/bin/env bash

set -euo pipefail

INPUT_FILE="../input.txt"

part1() {
    local data
    data=$(cat "$INPUT_FILE")

    # Use grep with extended regex to find mul(X,Y) patterns
    # Since macOS grep doesn't support -P, we'll use -E and process with awk
    local sum=0

    # Extract all mul(X,Y) patterns and calculate sum
    echo "$data" | grep -oE 'mul\([0-9]{1,3},[0-9]{1,3}\)' | while IFS= read -r match; do
        # Extract numbers using parameter expansion
        local nums="${match#mul(}"  # Remove 'mul(' prefix
        nums="${nums%)}"            # Remove ')' suffix
        local x="${nums%,*}"        # Get first number
        local y="${nums#*,}"        # Get second number
        sum=$((sum + x * y))
        echo "$sum" > /tmp/part1_sum.txt
    done

    if [[ -f /tmp/part1_sum.txt ]]; then
        cat /tmp/part1_sum.txt
        rm /tmp/part1_sum.txt
    else
        echo "0"
    fi
}

part2() {
    local data
    data=$(cat "$INPUT_FILE")

    # Strategy: Process the input character by character (or use awk)
    # Track position and enabled state

    # Use awk for more sophisticated parsing
    awk '
    BEGIN {
        sum = 0
        enabled = 1
    }
    {
        text = $0
        pos = 1
        len = length(text)

        while (pos <= len) {
            # Try to match do()
            if (substr(text, pos, 4) == "do()") {
                enabled = 1
                pos += 4
                continue
            }

            # Try to match don'\''t()
            if (substr(text, pos, 7) == "don'\''t()") {
                enabled = 0
                pos += 7
                continue
            }

            # Try to match mul(X,Y)
            if (substr(text, pos, 4) == "mul(") {
                start = pos + 4
                # Find the closing paren
                end = start
                found = 0
                while (end <= len && end < start + 10) {
                    if (substr(text, end, 1) == ")") {
                        found = 1
                        break
                    }
                    end++
                }

                if (found) {
                    args = substr(text, start, end - start)
                    # Check if args match X,Y pattern with 1-3 digits each
                    if (match(args, /^([0-9]{1,3}),([0-9]{1,3})$/)) {
                        split(args, parts, ",")
                        x = parts[1]
                        y = parts[2]
                        if (enabled) {
                            sum += x * y
                        }
                        pos = end + 1
                        continue
                    }
                }
            }

            pos++
        }
    }
    END {
        print sum
    }
    ' "$INPUT_FILE"
}

main() {
    echo "Part 1: $(part1)"
    echo "Part 2: $(part2)"
}

main
