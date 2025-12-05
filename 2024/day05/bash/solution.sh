#!/usr/bin/env bash

set -euo pipefail

# Read the input file
input_file="$(dirname "$0")/../input.txt"

# Read all rules (lines containing |)
rules=$(grep '|' "$input_file")

# Read all updates (lines containing ,)
updates=$(grep ',' "$input_file")

# Function to get pages that must come after a given page
get_after_pages() {
    local page=$1
    echo "$rules" | grep "^${page}|" | cut -d'|' -f2 | tr '\n' ' '
}

# Function to check if an update is in valid order
is_valid_order() {
    local update="$1"
    local -a pages
    IFS=',' read -ra pages <<< "$update"

    # Check each page
    for i in "${!pages[@]}"; do
        local page=${pages[$i]}

        # Get pages that must come after this page
        local after_pages=$(get_after_pages "$page")

        if [[ -n "$after_pages" ]]; then
            for after_page in $after_pages; do
                # Check if after_page is in this update
                for j in "${!pages[@]}"; do
                    if [[ ${pages[$j]} -eq $after_page ]]; then
                        # Found it - check if it comes after current position
                        if (( j < i )); then
                            return 1  # Invalid order
                        fi
                    fi
                done
            done
        fi
    done

    return 0  # Valid order
}

# Function to check if page A must come before page B
must_come_before() {
    local a=$1
    local b=$2

    # Check if there's a rule a|b
    echo "$rules" | grep -q "^${a}|${b}$" && return 0
    return 1
}

# Compare two pages: returns 0 if should swap, 1 if no swap
should_swap() {
    local a=$1
    local b=$2

    # If a|b exists (a must come before b), don't swap
    if grep -q "^${a}|${b}$" <<< "$rules"; then
        return 1  # Don't swap
    fi

    # If b|a exists (b must come before a), swap
    if grep -q "^${b}|${a}$" <<< "$rules"; then
        return 0  # Swap
    fi

    return 1  # No rule, don't swap
}

# Fix order using bubble sort with multiple passes until stable
fix_order() {
    local update="$1"
    local -a pages
    IFS=',' read -ra pages <<< "$update"
    local n=${#pages[@]}

    # Bubble sort with multiple passes until no swaps occur
    local swapped=1
    while (( swapped == 1 )); do
        swapped=0
        for (( i=0; i<n-1; i++ )); do
            local a=${pages[$i]}
            local b=${pages[$i+1]}

            # Check if we should swap
            if should_swap "$a" "$b"; then
                pages[$i]=$b
                pages[$((i+1))]=$a
                swapped=1
            fi
        done
    done

    # Convert to comma-separated
    local output=""
    for page in "${pages[@]}"; do
        if [[ -z "$output" ]]; then
            output="$page"
        else
            output="$output,$page"
        fi
    done
    echo "$output"
}

# Part 1: Sum middle page numbers of valid updates
part1_total=0

while IFS= read -r update_line; do
    if [[ -z "$update_line" ]]; then
        continue
    fi

    if is_valid_order "$update_line"; then
        # Get middle page
        IFS=',' read -ra pages <<< "$update_line"
        middle_idx=$((${#pages[@]} / 2))
        part1_total=$((part1_total + pages[middle_idx]))
    fi
done <<< "$updates"

# Part 2: Fix invalid updates and sum their middle page numbers
part2_total=0

while IFS= read -r update_line; do
    if [[ -z "$update_line" ]]; then
        continue
    fi

    if ! is_valid_order "$update_line"; then
        # Fix the order
        fixed=$(fix_order "$update_line")

        # Get middle page
        IFS=',' read -ra pages <<< "$fixed"
        middle_idx=$((${#pages[@]} / 2))
        part2_total=$((part2_total + pages[middle_idx]))
    fi
done <<< "$updates"

echo "Part 1: $part1_total"
echo "Part 2: $part2_total"
