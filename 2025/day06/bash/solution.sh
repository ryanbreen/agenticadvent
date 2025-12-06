#!/usr/bin/env bash

# Read input file
input_file="../input.txt"

# Read all lines into an array (compatible way)
lines=()
while IFS= read -r line; do
    lines+=("$line")
done < "$input_file"

# Find the operator row (last row with only +, *, and spaces)
op_row_idx=-1
for ((i=${#lines[@]}-1; i>=0; i--)); do
    line="${lines[$i]}"
    # Skip empty lines
    if [[ -z "${line// /}" ]]; then
        continue
    fi
    # Check if line contains only +, *, and spaces
    if [[ "$line" =~ ^[+*[:space:]]+$ ]]; then
        op_row_idx=$i
        break
    fi
done

if [[ $op_row_idx -lt 0 ]]; then
    echo "Error: No operator row found"
    exit 1
fi

# Separate number rows and operator row
number_rows=("${lines[@]:0:$op_row_idx}")
op_row="${lines[$op_row_idx]}"

# Find max width
max_width=0
for line in "${lines[@]}"; do
    len=${#line}
    if [[ $len -gt $max_width ]]; then
        max_width=$len
    fi
done

# Pad all rows to the same width
padded_number_rows=()
for line in "${number_rows[@]}"; do
    padded_number_rows+=("$(printf "%-${max_width}s" "$line")")
done
padded_op_row="$(printf "%-${max_width}s" "$op_row")"

# Function to check if a column is all spaces (separator)
is_separator_column() {
    local col=$1
    # Check operator row
    local char="${padded_op_row:$col:1}"
    if [[ "$char" != " " ]]; then
        return 1
    fi
    # Check all number rows
    for row in "${padded_number_rows[@]}"; do
        char="${row:$col:1}"
        if [[ "$char" != " " ]]; then
            return 1
        fi
    done
    return 0
}

# Part 1: Parse problems left-to-right, numbers vertically
part1() {
    local total=0
    local col=0

    while [[ $col -lt $max_width ]]; do
        # Skip separator columns
        while [[ $col -lt $max_width ]] && is_separator_column $col; do
            ((col++))
        done

        if [[ $col -ge $max_width ]]; then
            break
        fi

        # Find the end of this problem
        local start_col=$col
        while [[ $col -lt $max_width ]]; do
            if is_separator_column $col; then
                break
            fi
            ((col++))
        done
        local end_col=$col

        # Extract numbers and operator for this problem
        local numbers=()
        for row in "${padded_number_rows[@]}"; do
            local num_str="${row:$start_col:$((end_col-start_col))}"
            num_str="${num_str// /}"  # Remove spaces
            if [[ -n "$num_str" ]]; then
                numbers+=("$num_str")
            fi
        done

        local op_str="${padded_op_row:$start_col:$((end_col-start_col))}"
        op_str="${op_str// /}"  # Remove spaces

        if [[ -n "$op_str" && ${#numbers[@]} -gt 0 ]]; then
            # Solve the problem
            local result="${numbers[0]}"
            for ((i=1; i<${#numbers[@]}; i++)); do
                if [[ "$op_str" == "+" ]]; then
                    result=$(echo "$result + ${numbers[$i]}" | bc)
                elif [[ "$op_str" == "*" ]]; then
                    result=$(echo "$result * ${numbers[$i]}" | bc)
                fi
            done
            total=$(echo "$total + $result" | bc)
        fi
    done

    echo "$total"
}

# Part 2: Parse problems, reading columns right-to-left
part2() {
    local total=0
    local col=0

    while [[ $col -lt $max_width ]]; do
        # Skip separator columns
        while [[ $col -lt $max_width ]] && is_separator_column $col; do
            ((col++))
        done

        if [[ $col -ge $max_width ]]; then
            break
        fi

        # Find the end of this problem
        local start_col=$col
        while [[ $col -lt $max_width ]]; do
            if is_separator_column $col; then
                break
            fi
            ((col++))
        done
        local end_col=$col

        # For Part 2: Read columns right-to-left, each column forms a number
        local numbers=()
        for ((c=end_col-1; c>=start_col; c--)); do
            local digits=""
            for row in "${padded_number_rows[@]}"; do
                local ch="${row:$c:1}"
                if [[ "$ch" =~ [0-9] ]]; then
                    digits="${digits}${ch}"
                fi
            done
            if [[ -n "$digits" ]]; then
                numbers+=("$digits")
            fi
        done

        local op_str="${padded_op_row:$start_col:$((end_col-start_col))}"
        op_str="${op_str// /}"  # Remove spaces

        if [[ -n "$op_str" && ${#numbers[@]} -gt 0 ]]; then
            # Solve the problem
            local result="${numbers[0]}"
            for ((i=1; i<${#numbers[@]}; i++)); do
                if [[ "$op_str" == "+" ]]; then
                    result=$(echo "$result + ${numbers[$i]}" | bc)
                elif [[ "$op_str" == "*" ]]; then
                    result=$(echo "$result * ${numbers[$i]}" | bc)
                fi
            done
            total=$(echo "$total + $result" | bc)
        fi
    done

    echo "$total"
}

# Run both parts
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
