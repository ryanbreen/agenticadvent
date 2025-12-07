#!/bin/bash

# Read input file
input_file="../input.txt"

# Read all lines into an array (portable way)
lines=()
while IFS= read -r line; do
    lines+=("$line")
done < "$input_file"

rows=${#lines[@]}
cols=${#lines[0]}

# Part 1: Count how many times the beam is split
part1() {
    # Find starting position S in first row
    local start_col=-1
    for ((col=0; col<cols; col++)); do
        if [[ "${lines[0]:col:1}" == "S" ]]; then
            start_col=$col
            break
        fi
    done

    if [[ $start_col -eq -1 ]]; then
        echo 0
        return
    fi

    # Track active beam columns using space-separated string
    local active_beams="$start_col"
    local split_count=0

    # Process row by row starting from row 1
    for ((row=1; row<rows; row++)); do
        local new_beams=""
        local seen=""

        for col in $active_beams; do
            if [[ $col -ge 0 && $col -lt $cols ]]; then
                local cell="${lines[row]:col:1}"

                if [[ "$cell" == "^" ]]; then
                    # Beam hits splitter - count it and emit left/right
                    ((split_count++))

                    # Left beam goes to col-1
                    local left_col=$((col-1))
                    if [[ $left_col -ge 0 ]]; then
                        # Add to new_beams if not already seen
                        if [[ ! " $seen " =~ " $left_col " ]]; then
                            new_beams="$new_beams $left_col"
                            seen="$seen $left_col"
                        fi
                    fi

                    # Right beam goes to col+1
                    local right_col=$((col+1))
                    if [[ $right_col -lt $cols ]]; then
                        # Add to new_beams if not already seen
                        if [[ ! " $seen " =~ " $right_col " ]]; then
                            new_beams="$new_beams $right_col"
                            seen="$seen $right_col"
                        fi
                    fi
                else
                    # Beam continues straight down (whether '.' or other)
                    if [[ ! " $seen " =~ " $col " ]]; then
                        new_beams="$new_beams $col"
                        seen="$seen $col"
                    fi
                fi
            fi
        done

        active_beams="$new_beams"

        # If no more beams, stop
        if [[ -z "$active_beams" ]]; then
            break
        fi
    done

    echo "$split_count"
}

# Part 2: Count total timelines using bc for arbitrary precision
part2() {
    # Find starting position S in first row
    local start_col=-1
    for ((col=0; col<cols; col++)); do
        if [[ "${lines[0]:col:1}" == "S" ]]; then
            start_col=$col
            break
        fi
    done

    if [[ $start_col -eq -1 ]]; then
        echo 0
        return
    fi

    # Track timelines using parallel arrays: cols and counts
    # Format: "col1:count1 col2:count2 ..."
    local timelines="$start_col:1"

    # Process row by row starting from row 1
    for ((row=1; row<rows; row++)); do
        local new_timelines=""

        # Process each timeline
        for entry in $timelines; do
            local col="${entry%:*}"
            local count="${entry#*:}"

            if [[ $col -ge 0 && $col -lt $cols ]]; then
                local cell="${lines[row]:col:1}"

                if [[ "$cell" == "^" ]]; then
                    # Each timeline splits into 2 (left and right)

                    # Left beam goes to col-1
                    local left_col=$((col-1))
                    if [[ $left_col -ge 0 ]]; then
                        # Find if this column already exists in new_timelines
                        local found=0
                        local updated_timelines=""
                        for new_entry in $new_timelines; do
                            local nc="${new_entry%:*}"
                            local ncount="${new_entry#*:}"
                            if [[ $nc -eq $left_col ]]; then
                                # Add to existing count
                                local new_count=$(echo "$ncount + $count" | bc)
                                updated_timelines="$updated_timelines $nc:$new_count"
                                found=1
                            else
                                updated_timelines="$updated_timelines $new_entry"
                            fi
                        done
                        if [[ $found -eq 0 ]]; then
                            # Add new entry
                            updated_timelines="$updated_timelines $left_col:$count"
                        fi
                        new_timelines="$updated_timelines"
                    fi

                    # Right beam goes to col+1
                    local right_col=$((col+1))
                    if [[ $right_col -lt $cols ]]; then
                        # Find if this column already exists in new_timelines
                        local found=0
                        local updated_timelines=""
                        for new_entry in $new_timelines; do
                            local nc="${new_entry%:*}"
                            local ncount="${new_entry#*:}"
                            if [[ $nc -eq $right_col ]]; then
                                # Add to existing count
                                local new_count=$(echo "$ncount + $count" | bc)
                                updated_timelines="$updated_timelines $nc:$new_count"
                                found=1
                            else
                                updated_timelines="$updated_timelines $new_entry"
                            fi
                        done
                        if [[ $found -eq 0 ]]; then
                            # Add new entry
                            updated_timelines="$updated_timelines $right_col:$count"
                        fi
                        new_timelines="$updated_timelines"
                    fi
                else
                    # Timelines continue straight down
                    # Find if this column already exists in new_timelines
                    local found=0
                    local updated_timelines=""
                    for new_entry in $new_timelines; do
                        local nc="${new_entry%:*}"
                        local ncount="${new_entry#*:}"
                        if [[ $nc -eq $col ]]; then
                            # Add to existing count
                            local new_count=$(echo "$ncount + $count" | bc)
                            updated_timelines="$updated_timelines $nc:$new_count"
                            found=1
                        else
                            updated_timelines="$updated_timelines $new_entry"
                        fi
                    done
                    if [[ $found -eq 0 ]]; then
                        # Add new entry
                        updated_timelines="$updated_timelines $col:$count"
                    fi
                    new_timelines="$updated_timelines"
                fi
            fi
        done

        timelines="$new_timelines"

        # If no more timelines, stop
        if [[ -z "$timelines" ]]; then
            break
        fi
    done

    # Sum all timeline counts using bc
    local total=0
    for entry in $timelines; do
        local count="${entry#*:}"
        total=$(echo "$total + $count" | bc)
    done

    echo "$total"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
