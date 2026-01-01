#!/usr/bin/env bash
# Day 18: Lavaduct Lagoon - Polygon area with Shoelace formula and Pick's theorem

set -euo pipefail

INPUT_FILE="${1:-../input.txt}"

# Read all instructions into arrays
declare -a DIRECTIONS DISTANCES COLORS
idx=0
while read -r dir dist color; do
    DIRECTIONS[idx]="$dir"
    DISTANCES[idx]="$dist"
    # Extract hex code - remove (#...) wrapper
    COLORS[idx]="${color:2:6}"
    ((idx++)) || true
done < "$INPUT_FILE"

NUM_LINES=$idx

# Part 1: Use R/D/L/U directions with integer distances
part1() {
    local r=0 c=0 perimeter=0
    local -a vertices_r vertices_c
    vertices_r[0]=0
    vertices_c[0]=0
    local vidx=1

    for ((i=0; i<NUM_LINES; i++)); do
        local dir="${DIRECTIONS[i]}"
        local dist="${DISTANCES[i]}"

        case "$dir" in
            R) ((c += dist)) ;;
            L) ((c -= dist)) ;;
            D) ((r += dist)) ;;
            U) ((r -= dist)) ;;
        esac

        vertices_r[vidx]=$r
        vertices_c[vidx]=$c
        ((vidx++))
        ((perimeter += dist))
    done

    # Shoelace formula
    local area=0
    local n=$vidx
    for ((i=0; i<n; i++)); do
        local j=$(( (i + 1) % n ))
        ((area += vertices_r[i] * vertices_c[j])) || true
        ((area -= vertices_r[j] * vertices_c[i])) || true
    done

    # Take absolute value
    if ((area < 0)); then
        ((area = -area))
    fi
    ((area /= 2))

    # Total = area + perimeter/2 + 1
    echo $((area + perimeter / 2 + 1))
}

# Part 2: Decode from hex - first 5 hex digits = distance, last digit = direction
part2() {
    # For part 2, coordinates can be very large, so use bc
    local r=0 c=0 perimeter=0
    local -a vertices_r vertices_c
    vertices_r[0]=0
    vertices_c[0]=0
    local vidx=1

    for ((i=0; i<NUM_LINES; i++)); do
        local color="${COLORS[i]}"
        # First 5 hex digits = distance
        local hex_dist="${color:0:5}"
        local dist=$((16#$hex_dist))
        # Last digit = direction (0=R, 1=D, 2=L, 3=U)
        local dir_code="${color:5:1}"

        case "$dir_code" in
            0) ((c += dist)) ;;  # R
            1) ((r += dist)) ;;  # D
            2) ((c -= dist)) ;;  # L
            3) ((r -= dist)) ;;  # U
        esac

        vertices_r[vidx]=$r
        vertices_c[vidx]=$c
        ((vidx++))
        ((perimeter += dist))
    done

    # Shoelace formula using bc for arbitrary precision
    local n=$vidx

    # Build bc expression for area calculation
    local bc_expr="scale=0; area=0; "
    for ((i=0; i<n; i++)); do
        local j=$(( (i + 1) % n ))
        bc_expr+="area = area + (${vertices_r[i]}) * (${vertices_c[j]}); "
        bc_expr+="area = area - (${vertices_r[j]}) * (${vertices_c[i]}); "
    done
    bc_expr+="if (area < 0) area = -area; area / 2 + $perimeter / 2 + 1"

    echo "$bc_expr" | bc
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
