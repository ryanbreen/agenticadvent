#!/usr/bin/env bash

# Function to check if a number is invalid (repeated pattern)
is_invalid() {
    local num="$1"
    local len=${#num}

    # Only even-length numbers can be repeated patterns
    if (( len % 2 != 0 )); then
        return 1
    fi

    local half=$((len / 2))
    local first_half="${num:0:$half}"
    local second_half="${num:$half:$half}"

    # Check if the two halves are identical
    if [[ "$first_half" == "$second_half" ]]; then
        return 0
    fi

    return 1
}

# Read the input file
input=$(cat /Users/wrb/fun/code/advent2025/day02/input.txt)

# Remove newlines and extra spaces
input=$(echo "$input" | tr -d '\n')

# Split by commas
IFS=',' read -ra ranges <<< "$input"

total=0

# Process each range
for range in "${ranges[@]}"; do
    # Skip empty ranges
    [[ -z "$range" ]] && continue

    # Parse the range (format: start-end)
    IFS='-' read -ra parts <<< "$range"
    start="${parts[0]}"
    end="${parts[1]}"

    # Iterate through all numbers in the range
    for ((num=start; num<=end; num++)); do
        if is_invalid "$num"; then
            total=$((total + num))
        fi
    done
done

echo "$total"
