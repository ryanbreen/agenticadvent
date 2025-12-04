#!/bin/bash

# Read the input file into an array
input_file="../input.txt"

# Read file line by line into array (compatible with older bash)
grid=()
while IFS= read -r line; do
    grid+=("$line")
done < "$input_file"

rows=${#grid[@]}
cols=${#grid[0]}

# 8 directions: N, NE, E, SE, S, SW, W, NW
directions=("-1 -1" "-1 0" "-1 1" "0 -1" "0 1" "1 -1" "1 0" "1 1")

# Function to count adjacent rolls at a given position
count_adjacent() {
    local r=$1
    local c=$2
    local count=0

    for dir in "${directions[@]}"; do
        local dr dc
        read -r dr dc <<< "$dir"
        local nr=$((r + dr))
        local nc=$((c + dc))

        # Check bounds
        if [[ $nr -ge 0 && $nr -lt $rows && $nc -ge 0 && $nc -lt $cols ]]; then
            # Check if it's a roll
            local char="${grid[$nr]:$nc:1}"
            if [[ "$char" == "@" ]]; then
                ((count++))
            fi
        fi
    done

    echo "$count"
}

# Part 1: Count accessible rolls (fewer than 4 adjacent rolls)
part1() {
    local accessible=0

    for ((r=0; r<rows; r++)); do
        for ((c=0; c<cols; c++)); do
            local char="${grid[$r]:$c:1}"
            if [[ "$char" == "@" ]]; then
                local adj_count=$(count_adjacent "$r" "$c")
                if [[ $adj_count -lt 4 ]]; then
                    ((accessible++))
                fi
            fi
        done
    done

    echo "$accessible"
}

# Part 2: Iteratively remove accessible rolls
part2() {
    # Create a mutable copy of the grid
    local -a working_grid=()
    for line in "${grid[@]}"; do
        working_grid+=("$line")
    done

    local total_removed=0

    while true; do
        local -a removable=()

        # Find all removable rolls in this iteration
        for ((r=0; r<rows; r++)); do
            for ((c=0; c<cols; c++)); do
                local char="${working_grid[$r]:$c:1}"
                if [[ "$char" == "@" ]]; then
                    # Use count_adjacent helper, but need to pass working_grid
                    # Since count_adjacent uses global grid array, temporarily swap it
                    local -a original_grid=("${grid[@]}")
                    grid=("${working_grid[@]}")
                    local adj_count=$(count_adjacent "$r" "$c")
                    grid=("${original_grid[@]}")

                    # If fewer than 4 adjacent rolls, mark for removal
                    if [[ $adj_count -lt 4 ]]; then
                        removable+=("$r $c")
                    fi
                fi
            done
        done

        # If no rolls can be removed, we're done
        if [[ ${#removable[@]} -eq 0 ]]; then
            break
        fi

        # Remove all accessible rolls
        for pos in "${removable[@]}"; do
            local r c
            read -r r c <<< "$pos"
            # Replace character at position with '.'
            working_grid[$r]="${working_grid[$r]:0:$c}.${working_grid[$r]:$((c+1))}"
        done

        total_removed=$((total_removed + ${#removable[@]}))
    done

    echo "$total_removed"
}

# Run both parts
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
