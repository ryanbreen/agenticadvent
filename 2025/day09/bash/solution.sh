#!/bin/bash

# Day 9: Movie Theater - Bash with AWK
# Part 1: Find largest rectangle with red tiles as opposite corners
# Part 2: Find largest rectangle inside polygon formed by red tiles
#
# Algorithm:
# - Part 1: Try all pairs of red tiles as opposite corners, calculate area
# - Part 2: Build polygon from red tiles connected by edges
#   - Check if rectangle is valid: no edge crossings through interior
#   - Use ray casting to verify rectangle center is inside polygon
#   - AWK handles all computation with arrays for points and edges

INPUT_FILE="${1:-../input.txt}"

# Part 1: Find largest rectangle area using two red tiles as opposite corners
part1() {
    awk -F, '
    BEGIN {
        count = 0
    }
    {
        x[count] = $1
        y[count] = $2
        count++
    }
    END {
        max_area = 0

        # Check all pairs of points as opposite corners
        for (i = 0; i < count; i++) {
            for (j = i + 1; j < count; j++) {
                # Rectangle area = width * height (inclusive)
                width = (x[j] > x[i] ? x[j] - x[i] : x[i] - x[j]) + 1
                height = (y[j] > y[i] ? y[j] - y[i] : y[i] - y[j]) + 1
                area = width * height

                if (area > max_area) {
                    max_area = area
                }
            }
        }

        print max_area
    }
    ' "$INPUT_FILE"
}

# Part 2: Find largest rectangle inside the polygon
part2() {
    awk -F, '
    BEGIN {
        count = 0
    }
    {
        x[count] = $1
        y[count] = $2
        count++
    }
    END {
        # Build polygon edges (horizontal and vertical)
        h_count = 0
        v_count = 0

        for (i = 0; i < count; i++) {
            next_i = (i + 1) % count
            x1 = x[i]
            y1 = y[i]
            x2 = x[next_i]
            y2 = y[next_i]

            if (y1 == y2) {  # Horizontal edge
                h_y[h_count] = y1
                h_xmin[h_count] = (x1 < x2 ? x1 : x2)
                h_xmax[h_count] = (x1 > x2 ? x1 : x2)
                h_count++
            } else {  # Vertical edge
                v_x[v_count] = x1
                v_ymin[v_count] = (y1 < y2 ? y1 : y2)
                v_ymax[v_count] = (y1 > y2 ? y1 : y2)
                v_count++
            }
        }

        max_area = 0

        # Check all pairs of red tiles as corners
        for (i = 0; i < count; i++) {
            for (j = i + 1; j < count; j++) {
                x1 = x[i]
                y1 = y[i]
                x2 = x[j]
                y2 = y[j]

                # Get rectangle bounds
                min_x = (x1 < x2 ? x1 : x2)
                max_x = (x1 > x2 ? x1 : x2)
                min_y = (y1 < y2 ? y1 : y2)
                max_y = (y1 > y2 ? y1 : y2)

                # Check if rectangle is valid
                valid = 1

                # Check if any vertical edge crosses through rectangle interior
                for (vi = 0; vi < v_count && valid; vi++) {
                    vx = v_x[vi]
                    if (min_x < vx && vx < max_x) {
                        # Vertical edge x is inside rectangle x range
                        vy_min = v_ymin[vi]
                        vy_max = v_ymax[vi]
                        # Check if edge overlaps with rectangle y range
                        if (!(vy_max <= min_y || vy_min >= max_y)) {
                            valid = 0
                        }
                    }
                }

                # Check if any horizontal edge crosses through rectangle interior
                for (hi = 0; hi < h_count && valid; hi++) {
                    hy = h_y[hi]
                    if (min_y < hy && hy < max_y) {
                        # Horizontal edge y is inside rectangle y range
                        hx_min = h_xmin[hi]
                        hx_max = h_xmax[hi]
                        # Check if edge overlaps with rectangle x range
                        if (!(hx_max <= min_x || hx_min >= max_x)) {
                            valid = 0
                        }
                    }
                }

                # Check if rectangle center is inside polygon using ray casting
                if (valid) {
                    center_x = (min_x + max_x) / 2.0
                    center_y = (min_y + max_y) / 2.0

                    crossings = 0
                    for (vi = 0; vi < v_count; vi++) {
                        vx = v_x[vi]
                        if (vx > center_x) {  # Ray goes to the right
                            vy_min = v_ymin[vi]
                            vy_max = v_ymax[vi]

                            if (vy_min < center_y && center_y < vy_max) {
                                # Strict crossing
                                crossings++
                            } else if (center_y == vy_min || center_y == vy_max) {
                                # On corner - count as 0.5
                                crossings += 0.5
                            }
                        }
                    }

                    # Check if inside (odd number of crossings)
                    if (int(crossings) % 2 == 0 && crossings != int(crossings)) {
                        # Handle half-crossings
                        valid = (int(crossings * 2) % 2 == 1)
                    } else {
                        valid = (int(crossings) % 2 == 1)
                    }
                }

                if (valid) {
                    width = max_x - min_x + 1
                    height = max_y - min_y + 1
                    area = width * height

                    if (area > max_area) {
                        max_area = area
                    }
                }
            }
        }

        print max_area
    }
    ' "$INPUT_FILE"
}

# Run both parts
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
