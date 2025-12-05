#!/usr/bin/env bash

# Read input file
input_path="$(dirname "$0")/../input.txt"

# Parse input into two arrays
declare -a left_list
declare -a right_list

while read -r left right; do
    left_list+=("$left")
    right_list+=("$right")
done < "$input_path"

part1() {
    # Sort both lists
    IFS=$'\n' sorted_left=($(sort -n <<< "${left_list[*]}"))
    IFS=$'\n' sorted_right=($(sort -n <<< "${right_list[*]}"))
    unset IFS

    # Calculate total distance
    local total_distance=0
    local count=${#sorted_left[@]}

    for ((i=0; i<count; i++)); do
        local diff=$((sorted_left[i] - sorted_right[i]))
        # Absolute value
        if ((diff < 0)); then
            diff=$((-diff))
        fi
        total_distance=$((total_distance + diff))
    done

    echo "$total_distance"
}

part2() {
    local similarity_score=0

    for num in "${left_list[@]}"; do
        # Count occurrences of num in right_list using grep
        local count=$(printf '%s\n' "${right_list[@]}" | grep -c "^${num}$")
        similarity_score=$((similarity_score + num * count))
    done

    echo "$similarity_score"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
