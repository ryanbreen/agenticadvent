#!/usr/bin/env bash

# Read input
input_file="../input.txt"
read -r -a stones < "$input_file"

# Associative array for memoization: key is "value,blinks"
declare -A memo

# Count how many stones result from a single stone after N blinks
# Returns result via global variable to avoid subshell overhead
count_stones() {
    local value=$1
    local blinks=$2
    local key="${value},${blinks}"

    # Check memo
    if [[ -n "${memo[$key]}" ]]; then
        result="${memo[$key]}"
        return
    fi

    # Base case: 0 blinks left
    if (( blinks == 0 )); then
        memo[$key]=1
        result=1
        return
    fi

    # Rule 1: 0 becomes 1
    if [[ "$value" == "0" ]]; then
        count_stones 1 $((blinks - 1))
        memo[$key]=$result
        return
    fi

    # Rule 2: Even number of digits -> split
    local len=${#value}

    if (( len % 2 == 0 )); then
        local mid=$((len / 2))
        local left="${value:0:$mid}"
        local right="${value:$mid}"

        # Remove leading zeros using arithmetic evaluation with 10# base
        left=$((10#$left))
        right=$((10#$right))

        count_stones "$left" $((blinks - 1))
        local left_count=$result

        count_stones "$right" $((blinks - 1))
        local right_count=$result

        # Use bc only when numbers might overflow bash arithmetic
        if (( ${#left_count} > 15 || ${#right_count} > 15 )); then
            result=$(bc <<< "$left_count + $right_count")
        else
            result=$((left_count + right_count))
        fi
        memo[$key]=$result
        return
    fi

    # Rule 3: Multiply by 2024
    # Use bc for multiplication to handle large numbers
    local new_value
    if (( ${#value} > 15 )); then
        new_value=$(bc <<< "$value * 2024")
    else
        # Use 10# prefix to force base-10 interpretation
        new_value=$((10#$value * 2024))
    fi
    count_stones "$new_value" $((blinks - 1))
    memo[$key]=$result
}

# Part 1: 25 blinks
part1() {
    local total=0
    for stone in "${stones[@]}"; do
        count_stones "$stone" 25
        if (( ${#total} > 15 || ${#result} > 15 )); then
            total=$(bc <<< "$total + $result")
        else
            total=$((total + result))
        fi
    done
    echo "$total"
}

# Part 2: 75 blinks
part2() {
    local total=0
    for stone in "${stones[@]}"; do
        count_stones "$stone" 75
        total=$(bc <<< "$total + $result")
    done
    echo "$total"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
