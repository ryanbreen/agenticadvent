#!/usr/bin/env bash

# Day 15: Warehouse Woes

# Read input file
input_file="${1:-../input.txt}"
input_text=$(cat "$input_file")

# Parse input - split on blank line
grid_text=$(echo "$input_text" | awk 'BEGIN{RS=""} NR==1')
moves_text=$(echo "$input_text" | awk 'BEGIN{RS=""} NR==2' | tr -d '\n')

# Global variables for grid dimensions
ROWS=0
COLS=0

# Global grid for Part 2 (to avoid nameref issues)
declare -A GLOBAL_GRID

# Load grid into array
load_grid() {
    local grid_text=$1
    local -n grid_ref=$2

    ROWS=0
    COLS=0

    while IFS= read -r line; do
        if [[ -z "$line" ]]; then
            continue
        fi

        if [[ $COLS -eq 0 ]]; then
            COLS=${#line}
        fi

        for ((i=0; i<${#line}; i++)); do
            grid_ref[$ROWS,$i]="${line:$i:1}"
        done

        ((ROWS++))
    done <<< "$grid_text"
}

# Find robot position
find_robot() {
    local -n grid_ref=$1
    local -n pos_ref=$2

    for ((r=0; r<ROWS; r++)); do
        for ((c=0; c<COLS; c++)); do
            if [[ "${grid_ref[$r,$c]}" == "@" ]]; then
                pos_ref=("$r" "$c")
                return
            fi
        done
    done
}

# Get direction deltas
get_delta() {
    local dir=$1
    local -n dr_ref=$2
    local -n dc_ref=$3

    case $dir in
        '<') dr_ref=0; dc_ref=-1 ;;
        '>') dr_ref=0; dc_ref=1 ;;
        '^') dr_ref=-1; dc_ref=0 ;;
        'v') dr_ref=1; dc_ref=0 ;;
    esac
}

# Move robot (Part 1)
move_robot() {
    local -n grid_ref=$1
    local -n pos_ref=$2
    local direction=$3

    local r=${pos_ref[0]}
    local c=${pos_ref[1]}
    local dr dc
    get_delta "$direction" dr dc

    local nr=$((r + dr))
    local nc=$((c + dc))

    local target="${grid_ref[$nr,$nc]}"

    # Wall - don't move
    if [[ "$target" == "#" ]]; then
        return
    fi

    # Empty space - just move
    if [[ "$target" == "." ]]; then
        grid_ref[$r,$c]="."
        grid_ref[$nr,$nc]="@"
        pos_ref=("$nr" "$nc")
        return
    fi

    # Box - try to push
    if [[ "$target" == "O" ]]; then
        local check_r=$nr
        local check_c=$nc

        # Find end of box chain
        while [[ "${grid_ref[$check_r,$check_c]}" == "O" ]]; do
            check_r=$((check_r + dr))
            check_c=$((check_c + dc))
        done

        # If wall at end, can't push
        if [[ "${grid_ref[$check_r,$check_c]}" == "#" ]]; then
            return
        fi

        # Move boxes
        grid_ref[$check_r,$check_c]="O"
        grid_ref[$r,$c]="."
        grid_ref[$nr,$nc]="@"
        pos_ref=("$nr" "$nc")
    fi
}

# Calculate GPS sum
calculate_gps() {
    local -n grid_ref=$1
    local box_char=$2
    local total=0

    for ((r=0; r<ROWS; r++)); do
        for ((c=0; c<COLS; c++)); do
            if [[ "${grid_ref[$r,$c]}" == "$box_char" ]]; then
                total=$((total + 100 * r + c))
            fi
        done
    done

    echo $total
}

# Part 1
part1() {
    declare -A grid
    load_grid "$grid_text" grid

    local robot_pos
    find_robot grid robot_pos

    # Process each move
    for ((i=0; i<${#moves_text}; i++)); do
        local move="${moves_text:$i:1}"
        move_robot grid robot_pos "$move"
    done

    calculate_gps grid "O"
}

# Scale grid for Part 2
scale_grid() {
    local -n old_grid=$1
    local -n new_grid=$2
    local old_rows=$ROWS
    local old_cols=$COLS

    COLS=$((old_cols * 2))

    for ((r=0; r<old_rows; r++)); do
        local nc=0
        for ((c=0; c<old_cols; c++)); do
            local cell="${old_grid[$r,$c]}"
            case $cell in
                '#')
                    new_grid[$r,$nc]="#"
                    new_grid[$r,$((nc+1))]="#"
                    ;;
                'O')
                    new_grid[$r,$nc]="["
                    new_grid[$r,$((nc+1))]="]"
                    ;;
                '.')
                    new_grid[$r,$nc]="."
                    new_grid[$r,$((nc+1))]="."
                    ;;
                '@')
                    new_grid[$r,$nc]="@"
                    new_grid[$r,$((nc+1))]="."
                    ;;
            esac
            nc=$((nc + 2))
        done
    done
}

# Check if a wide box can move vertically (recursive) - uses GLOBAL_GRID
can_move_box_vertical() {
    local box_left_c=$1
    local r=$2
    local dr=$3

    local nr=$((r + dr))
    local left_c=$box_left_c
    local right_c=$((box_left_c + 1))

    local left_target="${GLOBAL_GRID[$nr,$left_c]}"
    local right_target="${GLOBAL_GRID[$nr,$right_c]}"

    # Wall blocks movement
    if [[ "$left_target" == "#" || "$right_target" == "#" ]]; then
        return 1
    fi

    # Check for boxes that would be pushed
    local boxes_to_check=()

    if [[ "$left_target" == "[" ]]; then
        boxes_to_check+=("$nr,$left_c")
    elif [[ "$left_target" == "]" ]]; then
        boxes_to_check+=("$nr,$((left_c-1))")
    fi

    if [[ "$right_target" == "[" ]]; then
        # Avoid duplicates
        local found=0
        for box in "${boxes_to_check[@]}"; do
            if [[ "$box" == "$nr,$right_c" ]]; then
                found=1
                break
            fi
        done
        if [[ $found -eq 0 ]]; then
            boxes_to_check+=("$nr,$right_c")
        fi
    elif [[ "$right_target" == "]" ]]; then
        local check_c=$((right_c - 1))
        local found=0
        for box in "${boxes_to_check[@]}"; do
            if [[ "$box" == "$nr,$check_c" ]]; then
                found=1
                break
            fi
        done
        if [[ $found -eq 0 ]]; then
            boxes_to_check+=("$nr,$check_c")
        fi
    fi

    # Recursively check each box
    for box in "${boxes_to_check[@]}"; do
        IFS=',' read -r box_r box_c <<< "$box"
        if ! can_move_box_vertical "$box_c" "$box_r" "$dr"; then
            return 1
        fi
    done

    return 0
}

# Collect all boxes that need to move vertically (recursive) - uses GLOBAL_GRID
declare -A collected_boxes
collect_boxes_vertical() {
    local box_left_c=$1
    local r=$2
    local dr=$3

    local key="$r,$box_left_c"
    collected_boxes[$key]=1

    local nr=$((r + dr))
    local left_c=$box_left_c
    local right_c=$((box_left_c + 1))

    local left_target="${GLOBAL_GRID[$nr,$left_c]}"
    local right_target="${GLOBAL_GRID[$nr,$right_c]}"

    local boxes_to_check=()

    if [[ "$left_target" == "[" ]]; then
        boxes_to_check+=("$nr,$left_c")
    elif [[ "$left_target" == "]" ]]; then
        boxes_to_check+=("$nr,$((left_c-1))")
    fi

    if [[ "$right_target" == "[" ]]; then
        local found=0
        for box in "${boxes_to_check[@]}"; do
            if [[ "$box" == "$nr,$right_c" ]]; then
                found=1
                break
            fi
        done
        if [[ $found -eq 0 ]]; then
            boxes_to_check+=("$nr,$right_c")
        fi
    elif [[ "$right_target" == "]" ]]; then
        local check_c=$((right_c - 1))
        local found=0
        for box in "${boxes_to_check[@]}"; do
            if [[ "$box" == "$nr,$check_c" ]]; then
                found=1
                break
            fi
        done
        if [[ $found -eq 0 ]]; then
            boxes_to_check+=("$nr,$check_c")
        fi
    fi

    for box in "${boxes_to_check[@]}"; do
        if [[ -z "${collected_boxes[$box]}" ]]; then
            IFS=',' read -r box_r box_c <<< "$box"
            collect_boxes_vertical "$box_c" "$box_r" "$dr"
        fi
    done
}

# Move robot with wide boxes (Part 2)
move_robot_wide() {
    local -n pos_ref=$1
    local direction=$2

    local r=${pos_ref[0]}
    local c=${pos_ref[1]}
    local dr dc
    get_delta "$direction" dr dc

    local nr=$((r + dr))
    local nc=$((c + dc))

    local target="${GLOBAL_GRID[$nr,$nc]}"

    # Wall - don't move
    if [[ "$target" == "#" ]]; then
        return
    fi

    # Empty space - just move
    if [[ "$target" == "." ]]; then
        GLOBAL_GRID[$r,$c]="."
        GLOBAL_GRID[$nr,$nc]="@"
        pos_ref=("$nr" "$nc")
        return
    fi

    # Box - try to push
    if [[ "$target" == "[" || "$target" == "]" ]]; then
        if [[ $dc -ne 0 ]]; then
            # Horizontal movement
            local check_c=$nc

            # Find end of box chain
            while [[ "${GLOBAL_GRID[$r,$check_c]}" == "[" || "${GLOBAL_GRID[$r,$check_c]}" == "]" ]]; do
                check_c=$((check_c + dc))
            done

            # If wall at end, can't push
            if [[ "${GLOBAL_GRID[$r,$check_c]}" == "#" ]]; then
                return
            fi

            # Shift all boxes
            if [[ $dc -gt 0 ]]; then
                # Moving right
                for ((col=check_c; col>nc; col--)); do
                    GLOBAL_GRID[$r,$col]="${GLOBAL_GRID[$r,$((col-1))]}"
                done
            else
                # Moving left
                for ((col=check_c; col<nc; col++)); do
                    GLOBAL_GRID[$r,$col]="${GLOBAL_GRID[$r,$((col+1))]}"
                done
            fi

            GLOBAL_GRID[$r,$c]="."
            GLOBAL_GRID[$nr,$nc]="@"
            pos_ref=("$nr" "$nc")
        else
            # Vertical movement
            local box_left_c
            if [[ "$target" == "[" ]]; then
                box_left_c=$nc
            else
                box_left_c=$((nc - 1))
            fi

            # Check if we can move
            if ! can_move_box_vertical "$box_left_c" "$nr" "$dr"; then
                return
            fi

            # Collect all boxes to move
            collected_boxes=()
            collect_boxes_vertical "$box_left_c" "$nr" "$dr"

            # Convert associative array to sorted list
            local -a boxes_list
            for key in "${!collected_boxes[@]}"; do
                boxes_list+=("$key")
            done

            # Sort boxes by row (reverse if moving down)
            if [[ $dr -gt 0 ]]; then
                # Moving down - sort descending
                IFS=$'\n' boxes_list=($(printf "%s\n" "${boxes_list[@]}" | sort -t',' -k1 -rn))
            else
                # Moving up - sort ascending
                IFS=$'\n' boxes_list=($(printf "%s\n" "${boxes_list[@]}" | sort -t',' -k1 -n))
            fi

            # Move all boxes
            for box in "${boxes_list[@]}"; do
                IFS=',' read -r box_r box_c <<< "$box"
                GLOBAL_GRID[$box_r,$box_c]="."
                GLOBAL_GRID[$box_r,$((box_c+1))]="."
                GLOBAL_GRID[$((box_r+dr)),$box_c]="["
                GLOBAL_GRID[$((box_r+dr)),$((box_c+1))]="]"
            done

            # Move robot
            GLOBAL_GRID[$r,$c]="."
            GLOBAL_GRID[$nr,$nc]="@"
            pos_ref=("$nr" "$nc")
        fi
    fi
}

# Part 2
part2() {
    declare -A grid
    load_grid "$grid_text" grid

    # Scale grid and copy to global
    GLOBAL_GRID=()
    scale_grid grid GLOBAL_GRID

    local robot_pos
    find_robot GLOBAL_GRID robot_pos

    # Process each move
    for ((i=0; i<${#moves_text}; i++)); do
        local move="${moves_text:$i:1}"
        move_robot_wide robot_pos "$move"
    done

    calculate_gps GLOBAL_GRID "["
}

# Run both parts
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
