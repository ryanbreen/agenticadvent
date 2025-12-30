#!/usr/bin/env bash
#
# Advent of Code 2023 - Day 13: Point of Incidence
# Find lines of reflection in patterns of ash and rocks
#
set -euo pipefail

declare -a patterns

# Read all patterns from input, separated by blank lines
read_patterns() {
    local input_file="$1"
    local -n patterns_ref="$2"
    local -a current_pattern=()
    local line

    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ -z "$line" ]]; then
            if (( ${#current_pattern[@]} > 0 )); then
                patterns_ref+=("$(printf '%s\n' "${current_pattern[@]}")")
                current_pattern=()
            fi
        else
            current_pattern+=("$line")
        fi
    done < "$input_file"

    if (( ${#current_pattern[@]} > 0 )); then
        patterns_ref+=("$(printf '%s\n' "${current_pattern[@]}")")
    fi
}

# Count character differences between two strings
count_differences() {
    local s1="$1" s2="$2"
    local -n result_ref="$3"
    local i len1=${#s1} len2=${#s2} min_len

    min_len=$((len1 < len2 ? len1 : len2))
    result_ref=0

    for (( i = 0; i < min_len; i++ )); do
        if [[ "${s1:i:1}" != "${s2:i:1}" ]]; then
            result_ref=$((result_ref + 1))
        fi
    done
}

# Reverse a string in-place via nameref
reverse_string() {
    local str="$1"
    local -n rev_ref="$2"
    local i

    rev_ref=""
    for (( i = ${#str} - 1; i >= 0; i-- )); do
        rev_ref+="${str:i:1}"
    done
}

# Find reflection line in pattern
# smudge_mode: 0 = perfect reflection, 1 = exactly one smudge required
# Returns: columns left of vertical line, or 100 * rows above horizontal line
find_reflection() {
    local pattern="$1" smudge_mode="$2"
    local -n reflection_result="$3"
    local -a rows
    local width height col i
    local total_diff diff left right left_reversed
    local left_len right_len min_len top_idx bottom_idx

    readarray -t rows <<< "$pattern"
    height=${#rows[@]}
    width=${#rows[0]}

    # Try vertical reflection at each column
    for (( col = 1; col < width; col++ )); do
        total_diff=0

        for line in "${rows[@]}"; do
            left="${line:0:col}"
            right="${line:col}"

            reverse_string "$left" left_reversed

            left_len=${#left_reversed}
            right_len=${#right}
            min_len=$((left_len < right_len ? left_len : right_len))

            count_differences "${left_reversed:0:min_len}" "${right:0:min_len}" diff
            total_diff=$((total_diff + diff))

            if [[ $total_diff -gt $smudge_mode ]]; then break; fi
        done

        if [[ $total_diff -eq $smudge_mode ]]; then
            reflection_result=$col
            return
        fi
    done

    # Try horizontal reflection at each row
    for (( row_idx = 1; row_idx < height; row_idx++ )); do
        total_diff=0
        min_len=$(( (height - row_idx) < row_idx ? (height - row_idx) : row_idx ))

        for (( i = 0; i < min_len; i++ )); do
            top_idx=$((row_idx - 1 - i))
            bottom_idx=$((row_idx + i))

            count_differences "${rows[top_idx]}" "${rows[bottom_idx]}" diff
            total_diff=$((total_diff + diff))

            if [[ $total_diff -gt $smudge_mode ]]; then break; fi
        done

        if [[ $total_diff -eq $smudge_mode ]]; then
            reflection_result=$((row_idx * 100))
            return
        fi
    done

    reflection_result=0
}

# Sum reflection values for all patterns
calculate_total() {
    local smudge_mode="$1"
    local -n total_ref="$2"
    local pattern value

    total_ref=0
    for pattern in "${patterns[@]}"; do
        find_reflection "$pattern" "$smudge_mode" value
        total_ref=$((total_ref + value))
    done
}

main() {
    local script_dir input_file part1 part2

    script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    input_file="$script_dir/../input.txt"

    read_patterns "$input_file" patterns

    calculate_total 0 part1
    calculate_total 1 part2

    echo "Part 1: $part1"
    echo "Part 2: $part2"
}

main
