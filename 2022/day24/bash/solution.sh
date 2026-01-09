#!/bin/bash

# Day 24: Blizzard Basin
# BFS through a grid with moving blizzards

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Use AWK for the heavy computation with precomputed blizzard positions
awk '
function gcd(a, b) {
    while (b != 0) {
        t = b
        b = a % b
        a = t
    }
    return a
}

function lcm(a, b) {
    return (a * b) / gcd(a, b)
}

function mod(a, m) {
    return ((a % m) + m) % m
}

# Precompute all blizzard positions for all times in one period
function precompute_blizzards(    i, r, c, d, ir, ic, nr, nc, t, cell) {
    for (t = 0; t < period; t++) {
        for (i = 0; i < num_blizzards; i++) {
            r = bliz_r[i]
            c = bliz_c[i]
            d = bliz_d[i]

            # Adjust to inner coordinates (subtract 2 for 1-indexed walls)
            ir = r - 2
            ic = c - 2

            if (d == "^") {
                nr = mod(ir - t, inner_h)
                nc = ic
            } else if (d == "v") {
                nr = mod(ir + t, inner_h)
                nc = ic
            } else if (d == "<") {
                nr = ir
                nc = mod(ic - t, inner_w)
            } else if (d == ">") {
                nr = ir
                nc = mod(ic + t, inner_w)
            }

            # Store as numeric key: time*cell_count + cell_id
            cell = (nr + 1) * width + (nc + 2)
            bliz_cache[t * cell_count + cell] = 1
        }
    }
}

# BFS from (sr, sc) to (er, ec) starting at time start_t
function bfs(sr, sc, er, ec, start_t,    t, nt, tm, list_len, next_len, i, cell, r, c, di, nr, nc, ncell, key, goal_cell) {
    # Directions: wait, up, down, left, right
    dr[0] = 0; dc[0] = 0
    dr[1] = -1; dc[1] = 0
    dr[2] = 1; dc[2] = 0
    dr[3] = 0; dc[3] = -1
    dr[4] = 0; dc[4] = 1

    delete seen
    list_len = 1
    curr[0] = (sr - 1) * width + sc
    goal_cell = (er - 1) * width + ec
    t = start_t
    tm = mod(t, period)
    seen[tm * cell_count + curr[0]] = 1

    while (1) {
        for (i = 0; i < list_len; i++) {
            if (curr[i] == goal_cell) {
                return t
            }
        }

        nt = t + 1
        tm = mod(nt, period)
        next_len = 0

        for (i = 0; i < list_len; i++) {
            cell = curr[i]
            r = row_of[cell]
            c = col_of[cell]

            for (di = 0; di < 5; di++) {
                nr = r + dr[di]
                nc = c + dc[di]

                if ((nr == sr && nc == sc) || (nr == er && nc == ec)) {
                    # Start or end position - always valid
                } else if (nr < 2 || nr > height - 1 || nc < 2 || nc > width - 1) {
                    # Wall position (1-indexed: row 1 and row height are walls,
                    # col 1 and col width are walls)
                    continue
                }

                ncell = (nr - 1) * width + nc
                if ((tm * cell_count + ncell) in bliz_cache) {
                    continue
                }

                key = tm * cell_count + ncell
                if (!(key in seen)) {
                    seen[key] = 1
                    nxt[next_len] = ncell
                    next_len++
                }
            }
        }

        if (next_len == 0) {
            return -1
        }

        delete curr
        for (i = 0; i < next_len; i++) {
            curr[i] = nxt[i]
        }
        delete nxt
        list_len = next_len
        t = nt
    }
}

BEGIN {
    num_blizzards = 0
}

{
    lines[NR] = $0
    height = NR
    width = length($0)
}

END {
    inner_h = height - 2
    inner_w = width - 2
    period = lcm(inner_h, inner_w)
    cell_count = height * width

    # Parse blizzards
    for (r = 1; r <= height; r++) {
        n = split(lines[r], chars, "")
        for (c = 1; c <= n; c++) {
            ch = chars[c]
            if (ch == "^" || ch == "v" || ch == "<" || ch == ">") {
                bliz_r[num_blizzards] = r
                bliz_c[num_blizzards] = c
                bliz_d[num_blizzards] = ch
                num_blizzards++
            }
        }
    }

    # Precompute all blizzard positions
    precompute_blizzards()

    # Find start and end
    n = split(lines[1], chars, "")
    for (c = 1; c <= n; c++) {
        if (chars[c] == ".") {
            start_r = 1
            start_c = c
            break
        }
    }

    n = split(lines[height], chars, "")
    for (c = 1; c <= n; c++) {
        if (chars[c] == ".") {
            end_r = height
            end_c = c
            break
        }
    }

    # Precompute row/col from cell id
    for (r = 1; r <= height; r++) {
        for (c = 1; c <= width; c++) {
            cell = (r - 1) * width + c
            row_of[cell] = r
            col_of[cell] = c
        }
    }
    # Part 1: start to end
    t1 = bfs(start_r, start_c, end_r, end_c, 0)
    print "Part 1:", t1

    # Part 2: start -> end -> start -> end
    t2 = bfs(end_r, end_c, start_r, start_c, t1)
    t3 = bfs(start_r, start_c, end_r, end_c, t2)
    print "Part 2:", t3
}
' "$INPUT_FILE"
