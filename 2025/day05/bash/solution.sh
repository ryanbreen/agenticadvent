#!/usr/bin/env bash

set -euo pipefail

# Read input file
INPUT_FILE="../input.txt"

# Parse input into two sections
LINES=()
while IFS= read -r line; do
    LINES+=("$line")
done < "$INPUT_FILE"

# Find the blank line separator
BLANK_IDX=-1
for i in "${!LINES[@]}"; do
    if [[ -z "${LINES[$i]}" ]]; then
        BLANK_IDX=$i
        break
    fi
done

if [[ $BLANK_IDX -eq -1 ]]; then
    echo "Error: No blank line found in input" >&2
    exit 1
fi

# Part 1: Count how many available IDs fall within ANY range
part1() {
    local fresh_count=0

    # Iterate over ingredient IDs (second section)
    for ((i = BLANK_IDX + 1; i < ${#LINES[@]}; i++)); do
        local ingredient_id="${LINES[$i]}"
        [[ -z "$ingredient_id" ]] && continue

        # Check if this ID falls within any range
        local found=0
        for ((j = 0; j < BLANK_IDX; j++)); do
            local range="${LINES[$j]}"
            IFS='-' read -r start end <<< "$range"

            # Check if ingredient_id is in range [start, end]
            if [[ "$ingredient_id" -ge "$start" && "$ingredient_id" -le "$end" ]]; then
                found=1
                break
            fi
        done

        if [[ $found -eq 1 ]]; then
            ((fresh_count++))
        fi
    done

    echo "$fresh_count"
}

# Part 2: Count total unique IDs covered by ALL ranges (merge overlapping)
part2() {
    # Parse ranges into arrays
    local -a starts=()
    local -a ends=()

    for ((i = 0; i < BLANK_IDX; i++)); do
        local range="${LINES[$i]}"
        IFS='-' read -r start end <<< "$range"
        starts+=("$start")
        ends+=("$end")
    done

    # Sort ranges by start position (bubble sort for simplicity)
    local n=${#starts[@]}
    for ((i = 0; i < n; i++)); do
        for ((j = 0; j < n - i - 1; j++)); do
            if [[ "${starts[$j]}" -gt "${starts[$((j + 1))]}" ]]; then
                # Swap starts
                local tmp="${starts[$j]}"
                starts[$j]="${starts[$((j + 1))]}"
                starts[$((j + 1))]="$tmp"

                # Swap ends
                tmp="${ends[$j]}"
                ends[$j]="${ends[$((j + 1))]}"
                ends[$((j + 1))]="$tmp"
            fi
        done
    done

    # Merge overlapping ranges
    local -a merged_starts=()
    local -a merged_ends=()

    merged_starts+=("${starts[0]}")
    merged_ends+=("${ends[0]}")

    for ((i = 1; i < n; i++)); do
        local start="${starts[$i]}"
        local end="${ends[$i]}"
        local last_idx=$((${#merged_starts[@]} - 1))
        local last_end="${merged_ends[$last_idx]}"

        # Check if ranges overlap or are adjacent (use bc for large number comparison)
        # start <= last_end + 1 means overlap or adjacent
        local overlaps=$(echo "$start <= $last_end + 1" | bc)

        if [[ "$overlaps" -eq 1 ]]; then
            # Merge: update the last end to max(last_end, end)
            local new_end=$(echo "if ($last_end > $end) $last_end else $end" | bc)
            merged_ends[$last_idx]="$new_end"
        else
            # No overlap - add as new range
            merged_starts+=("$start")
            merged_ends+=("$end")
        fi
    done

    # Count total unique IDs covered by merged ranges
    # Use bc for large number arithmetic
    local total_count=0
    for ((i = 0; i < ${#merged_starts[@]}; i++)); do
        local start="${merged_starts[$i]}"
        local end="${merged_ends[$i]}"
        # Count = end - start + 1
        local count=$(echo "$end - $start + 1" | bc)
        total_count=$(echo "$total_count + $count" | bc)
    done

    echo "$total_count"
}

# Run both parts
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
