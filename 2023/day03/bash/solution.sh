#!/usr/bin/env bash

# Advent of Code 2023 - Day 3: Gear Ratios

# Read input into an array (Bash 3.2 compatible)
grid=()
while IFS= read -r line || [ -n "$line" ]; do
    grid+=("$line")
done < ../input.txt

rows=${#grid[@]}
cols=${#grid[0]}

# Part 1: Find all numbers adjacent to symbols
part1_sum=0

# Part 2: Track gears using a temp file (no associative arrays in Bash 3.2)
gear_file=$(mktemp)
trap "rm -f $gear_file" EXIT

# Function to check if a character is a symbol (not digit and not period)
is_symbol() {
    local char="$1"
    [[ "$char" != "." && ! "$char" =~ [0-9] ]]
}

# Function to check if a number at given position is adjacent to a symbol
check_adjacent_symbol() {
    local row=$1
    local col_start=$2
    local col_end=$3

    # Check all 8 directions for each digit in the number
    for ((col=col_start; col<=col_end; col++)); do
        for ((dr=-1; dr<=1; dr++)); do
            for ((dc=-1; dc<=1; dc++)); do
                # Skip the cell itself
                if [ $dr -eq 0 ] && [ $dc -eq 0 ]; then
                    continue
                fi

                local nr=$((row + dr))
                local nc=$((col + dc))

                # Check bounds
                if [ $nr -ge 0 ] && [ $nr -lt $rows ] && [ $nc -ge 0 ] && [ $nc -lt $cols ]; then
                    local char="${grid[$nr]:$nc:1}"
                    if is_symbol "$char"; then
                        return 0  # Found a symbol
                    fi
                fi
            done
        done
    done

    return 1  # No symbol found
}

# Function to record numbers adjacent to gears for Part 2
record_gear_adjacency() {
    local row=$1
    local col_start=$2
    local col_end=$3
    local number=$4

    # Track which gears this number is adjacent to (to avoid duplicates)
    local seen_gears=""

    # Check all 8 directions for each digit in the number
    for ((col=col_start; col<=col_end; col++)); do
        for ((dr=-1; dr<=1; dr++)); do
            for ((dc=-1; dc<=1; dc++)); do
                # Skip the cell itself
                if [ $dr -eq 0 ] && [ $dc -eq 0 ]; then
                    continue
                fi

                local nr=$((row + dr))
                local nc=$((col + dc))

                # Check bounds
                if [ $nr -ge 0 ] && [ $nr -lt $rows ] && [ $nc -ge 0 ] && [ $nc -lt $cols ]; then
                    local char="${grid[$nr]:$nc:1}"
                    if [ "$char" = "*" ]; then
                        local gear_key="${nr}_${nc}"
                        # Only record once per gear (avoid duplicates from same number)
                        if [[ ! "$seen_gears" =~ $gear_key ]]; then
                            seen_gears="$seen_gears $gear_key"
                            # Append to gear file: gear_key number
                            echo "$gear_key $number" >> "$gear_file"
                        fi
                    fi
                fi
            done
        done
    done
}

# Process each row to find numbers
for ((row=0; row<rows; row++)); do
    line="${grid[$row]}"
    col=0

    while [ $col -lt $cols ]; do
        char="${line:$col:1}"

        # Check if we found the start of a number
        if [[ "$char" =~ [0-9] ]]; then
            # Extract the full number
            number=""
            col_start=$col

            while [ $col -lt $cols ] && [[ "${line:$col:1}" =~ [0-9] ]]; do
                number="${number}${line:$col:1}"
                col=$((col + 1))
            done

            col_end=$((col - 1))

            # Part 1: Check if adjacent to a symbol
            if check_adjacent_symbol $row $col_start $col_end; then
                part1_sum=$((part1_sum + number))
            fi

            # Part 2: Record gear adjacency
            record_gear_adjacency $row $col_start $col_end $number
        else
            col=$((col + 1))
        fi
    done
done

# Part 2: Calculate sum of gear ratios
part2_sum=0

# Group numbers by gear and find gears with exactly 2 adjacent numbers
if [ -s "$gear_file" ]; then
    # Use awk to group and calculate gear ratios
    part2_sum=$(awk '
    {
        gear = $1
        number = $2
        if (!(gear in count)) {
            count[gear] = 0
            prod[gear] = 1
        }
        count[gear]++
        prod[gear] *= number
    }
    END {
        sum = 0
        for (gear in count) {
            if (count[gear] == 2) {
                sum += prod[gear]
            }
        }
        print sum
    }
    ' "$gear_file")
fi

echo "Part 1: $part1_sum"
echo "Part 2: $part2_sum"
