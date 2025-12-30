#!/opt/homebrew/bin/bash

# Advent of Code 2023 Day 16: The Floor Will Be Lava
# Light beam bouncing through a grid with mirrors and splitters

INPUT_FILE="${1:-../input.txt}"

# Read grid into array
mapfile -t GRID < "$INPUT_FILE"
ROWS=${#GRID[@]}
COLS=${#GRID[0]}

# Direction encoding: 0=right, 1=down, 2=left, 3=up
# DR and DC give row/col deltas for each direction
DR=(0 1 0 -1)
DC=(1 0 -1 0)

# Count energized tiles starting from given position and direction
count_energized() {
    local start_row=$1
    local start_col=$2
    local start_dir=$3

    # Use associative arrays for visited states and energized cells
    declare -A visited
    declare -A energized

    # BFS queue as array of "row,col,dir" strings
    local queue=("$start_row,$start_col,$start_dir")
    local head=0

    while (( head < ${#queue[@]} )); do
        local state="${queue[$head]}"
        ((head++))

        # Parse state
        IFS=',' read -r row col dir <<< "$state"

        # Check bounds
        if (( row < 0 || row >= ROWS || col < 0 || col >= COLS )); then
            continue
        fi

        # Check if already visited this state
        local key="$row,$col,$dir"
        if [[ -n "${visited[$key]}" ]]; then
            continue
        fi
        visited[$key]=1
        energized["$row,$col"]=1

        # Get cell character
        local cell="${GRID[$row]:$col:1}"

        # Determine next directions based on cell and current direction
        local -a next_dirs=()

        case "$cell" in
            '.')
                next_dirs=("$dir")
                ;;
            '/')
                # right(0)->up(3), down(1)->left(2), left(2)->down(1), up(3)->right(0)
                case "$dir" in
                    0) next_dirs=(3) ;;
                    1) next_dirs=(2) ;;
                    2) next_dirs=(1) ;;
                    3) next_dirs=(0) ;;
                esac
                ;;
            '\')
                # right(0)->down(1), down(1)->right(0), left(2)->up(3), up(3)->left(2)
                case "$dir" in
                    0) next_dirs=(1) ;;
                    1) next_dirs=(0) ;;
                    2) next_dirs=(3) ;;
                    3) next_dirs=(2) ;;
                esac
                ;;
            '|')
                # Vertical splitter: if horizontal (right/left), split to up+down
                if (( dir == 0 || dir == 2 )); then
                    next_dirs=(1 3)  # down and up
                else
                    next_dirs=("$dir")  # pass through
                fi
                ;;
            '-')
                # Horizontal splitter: if vertical (up/down), split to left+right
                if (( dir == 1 || dir == 3 )); then
                    next_dirs=(0 2)  # right and left
                else
                    next_dirs=("$dir")  # pass through
                fi
                ;;
        esac

        # Add next states to queue
        for ndir in "${next_dirs[@]}"; do
            local nrow=$((row + DR[ndir]))
            local ncol=$((col + DC[ndir]))
            queue+=("$nrow,$ncol,$ndir")
        done
    done

    echo "${#energized[@]}"
}

# Part 1: Start at (0,0) heading right
part1=$(count_energized 0 0 0)
echo "Part 1: $part1"

# Part 2: Try all edge starting positions
max_energized=0

# Top edge, heading down
for ((c=0; c<COLS; c++)); do
    result=$(count_energized 0 "$c" 1)
    if (( result > max_energized )); then
        max_energized=$result
    fi
done

# Bottom edge, heading up
for ((c=0; c<COLS; c++)); do
    result=$(count_energized $((ROWS-1)) "$c" 3)
    if (( result > max_energized )); then
        max_energized=$result
    fi
done

# Left edge, heading right
for ((r=0; r<ROWS; r++)); do
    result=$(count_energized "$r" 0 0)
    if (( result > max_energized )); then
        max_energized=$result
    fi
done

# Right edge, heading left
for ((r=0; r<ROWS; r++)); do
    result=$(count_energized "$r" $((COLS-1)) 2)
    if (( result > max_energized )); then
        max_energized=$result
    fi
done

echo "Part 2: $max_energized"
