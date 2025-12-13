#!/bin/bash

# Read input file
INPUT_FILE="../input.txt"

# Parse machines from input
parse_machines() {
    local ax ay bx by px py
    local line_count=0

    while IFS= read -r line; do
        if [[ -z "$line" ]]; then
            # Empty line - output machine if we have data
            if [ $line_count -eq 3 ]; then
                echo "$ax $ay $bx $by $px $py"
            fi
            line_count=0
            continue
        fi

        if [[ "$line" =~ Button\ A:\ X\+([0-9]+),\ Y\+([0-9]+) ]]; then
            ax="${BASH_REMATCH[1]}"
            ay="${BASH_REMATCH[2]}"
            ((line_count++))
        elif [[ "$line" =~ Button\ B:\ X\+([0-9]+),\ Y\+([0-9]+) ]]; then
            bx="${BASH_REMATCH[1]}"
            by="${BASH_REMATCH[2]}"
            ((line_count++))
        elif [[ "$line" =~ Prize:\ X=([0-9]+),\ Y=([0-9]+) ]]; then
            px="${BASH_REMATCH[1]}"
            py="${BASH_REMATCH[2]}"
            ((line_count++))
        fi
    done < "$INPUT_FILE"

    # Output last machine if file doesn't end with empty line
    if [ $line_count -eq 3 ]; then
        echo "$ax $ay $bx $by $px $py"
    fi
}

# Solve using Cramer's rule
# Args: ax ay bx by px py max_presses
solve_machine() {
    local ax=$1 ay=$2 bx=$3 by=$4 px=$5 py=$6 max_presses=$7

    # Calculate determinant: det = ax*by - ay*bx
    local det=$(bc <<< "$ax * $by - $ay * $bx")

    # No unique solution if det = 0
    if [ "$det" = "0" ]; then
        echo "0"
        return
    fi

    # Calculate numerators using Cramer's rule
    # a = (px*by - py*bx) / det
    # b = (ax*py - ay*px) / det
    local a_num=$(bc <<< "$px * $by - $py * $bx")
    local b_num=$(bc <<< "$ax * $py - $ay * $px")

    # Check if solutions are integers (remainder must be 0)
    local a_rem=$(bc <<< "$a_num % $det")
    local b_rem=$(bc <<< "$b_num % $det")

    if [ "$a_rem" != "0" ] || [ "$b_rem" != "0" ]; then
        echo "0"
        return
    fi

    # Calculate integer solutions
    local a=$(bc <<< "$a_num / $det")
    local b=$(bc <<< "$b_num / $det")

    # Check non-negative (bc returns negative numbers with leading -)
    if [[ "$a" == -* ]] || [[ "$b" == -* ]]; then
        echo "0"
        return
    fi

    # Check max presses constraint (Part 1 only)
    if [ -n "$max_presses" ]; then
        if [ $(bc <<< "$a > $max_presses") -eq 1 ] || [ $(bc <<< "$b > $max_presses") -eq 1 ]; then
            echo "0"
            return
        fi
    fi

    # Calculate cost: 3*a + b
    local cost=$(bc <<< "3 * $a + $b")
    echo "$cost"
}

# Part 1: Max 100 presses per button
part1() {
    local total=0

    while read -r ax ay bx by px py; do
        local cost=$(solve_machine "$ax" "$ay" "$bx" "$by" "$px" "$py" 100)
        total=$(bc <<< "$total + $cost")
    done < <(parse_machines)

    echo "$total"
}

# Part 2: Prize coordinates shifted by 10^13, no press limit
part2() {
    local total=0
    local offset=10000000000000

    while read -r ax ay bx by px py; do
        # Shift prize coordinates
        local px_shifted=$(bc <<< "$px + $offset")
        local py_shifted=$(bc <<< "$py + $offset")

        local cost=$(solve_machine "$ax" "$ay" "$bx" "$by" "$px_shifted" "$py_shifted" "")
        total=$(bc <<< "$total + $cost")
    done < <(parse_machines)

    echo "$total"
}

# Main execution
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
