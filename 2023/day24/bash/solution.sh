#!/usr/bin/env bash
# Day 24: Never Tell Me The Odds
# Uses bc for arbitrary precision arithmetic

INPUT_FILE="${1:-../input.txt}"

# Read hailstones into arrays
declare -a PX PY PZ VX VY VZ
n=0

while IFS= read -r line || [[ -n "$line" ]]; do
    # Parse: px, py, pz @ vx, vy, vz
    line="${line//,/ }"
    line="${line//@/ }"
    read -r px py pz vx vy vz <<< "$line"
    PX[n]="$px"
    PY[n]="$py"
    PZ[n]="$pz"
    VX[n]="$vx"
    VY[n]="$vy"
    VZ[n]="$vz"
    ((n++))
done < "$INPUT_FILE"

# Part 1: Count 2D intersections in the future within bounds
MIN_COORD="200000000000000"
MAX_COORD="400000000000000"

part1() {
    # Generate all bc commands at once and pipe to bc
    local bc_script=""
    local count=0
    local i j

    for ((i = 0; i < n; i++)); do
        for ((j = i + 1; j < n; j++)); do
            local px1="${PX[i]}" py1="${PY[i]}" vx1="${VX[i]}" vy1="${VY[i]}"
            local px2="${PX[j]}" py2="${PY[j]}" vx2="${VX[j]}" vy2="${VY[j]}"

            # Build bc script for this pair
            bc_script+="
det = $vx2 * ($vy1) - ($vx1) * ($vy2)
if (det != 0) {
    dx = $px2 - ($px1)
    dy = $py2 - ($py1)
    t1_num = $vx2 * dy - dx * ($vy2)
    t2_num = $vx1 * dy - dx * ($vy1)

    /* Check signs for future (t >= 0) */
    t1_ok = 0
    t2_ok = 0

    if (det > 0) {
        if (t1_num >= 0) t1_ok = 1
        if (t2_num >= 0) t2_ok = 1
    } else {
        if (t1_num <= 0) t1_ok = 1
        if (t2_num <= 0) t2_ok = 1
    }

    if (t1_ok && t2_ok) {
        /* x = px1 + vx1 * t1_num / det */
        /* Check: MIN_COORD <= x <= MAX_COORD */
        x_num = ($px1) * det + ($vx1) * t1_num
        y_num = ($py1) * det + ($vy1) * t1_num

        if (det > 0) {
            min_b = $MIN_COORD * det
            max_b = $MAX_COORD * det
        } else {
            min_b = $MAX_COORD * det
            max_b = $MIN_COORD * det
        }

        if (x_num >= min_b && x_num <= max_b && y_num >= min_b && y_num <= max_b) {
            print \"1\\n\"
        }
    }
}
"
        done
    done

    count=$(echo "$bc_script" | bc 2>/dev/null | wc -l | tr -d ' ')
    echo "$count"
}

# Part 2: Find rock position and velocity that hits all hailstones
# Uses Gaussian elimination with exact fractions via bc

part2() {
    # Use only first 5 hailstones for the 4 equations
    # Build system for XY plane: coeffs [vy1-vy2, vx2-vx1, py2-py1, px1-px2] = px1*vy1 - py1*vx1 - (px2*vy2 - py2*vx2)

    # bc with arrays (1D only in standard bc)
    # Store 4x5 matrix as a[row*5+col]
    local bc_script="scale = 30
"

    # XY system: 4 equations for [rx, ry, rvx, rvy]
    for k in 0 1 2 3; do
        local i=$k j=$((k + 1))
        local px1="${PX[i]}" py1="${PY[i]}" vx1="${VX[i]}" vy1="${VY[i]}"
        local px2="${PX[j]}" py2="${PY[j]}" vx2="${VX[j]}" vy2="${VY[j]}"

        local idx=$((k * 5))
        bc_script+="
a[$((idx + 0))] = ($vy1) - ($vy2)
a[$((idx + 1))] = ($vx2) - ($vx1)
a[$((idx + 2))] = ($py2) - ($py1)
a[$((idx + 3))] = ($px1) - ($px2)
a[$((idx + 4))] = ($px1) * ($vy1) - ($py1) * ($vx1) - (($px2) * ($vy2) - ($py2) * ($vx2))
"
    done

    bc_script+='
/* Forward elimination - 4x5 augmented matrix */
/* Row 0: eliminate column 0 in rows 1,2,3 */
piv = a[0]
for (r = 1; r < 4; r++) {
    if (a[r*5] != 0) {
        fac = a[r*5] / piv
        for (c = 0; c < 5; c++) {
            a[r*5+c] = a[r*5+c] - fac * a[c]
        }
    }
}

/* Row 1: eliminate column 1 in rows 2,3 */
piv = a[6]
for (r = 2; r < 4; r++) {
    if (a[r*5+1] != 0) {
        fac = a[r*5+1] / piv
        for (c = 1; c < 5; c++) {
            a[r*5+c] = a[r*5+c] - fac * a[5+c]
        }
    }
}

/* Row 2: eliminate column 2 in row 3 */
piv = a[12]
if (a[17] != 0) {
    fac = a[17] / piv
    for (c = 2; c < 5; c++) {
        a[15+c] = a[15+c] - fac * a[10+c]
    }
}

/* Back substitution */
x3 = a[19] / a[18]
x2 = (a[14] - a[13] * x3) / a[12]
x1 = (a[9] - a[8] * x3 - a[7] * x2) / a[6]
x0 = (a[4] - a[3] * x3 - a[2] * x2 - a[1] * x1) / a[0]

rx = x0
ry = x1
'

    # XZ system
    for k in 0 1 2 3; do
        local i=$k j=$((k + 1))
        local px1="${PX[i]}" pz1="${PZ[i]}" vx1="${VX[i]}" vz1="${VZ[i]}"
        local px2="${PX[j]}" pz2="${PZ[j]}" vx2="${VX[j]}" vz2="${VZ[j]}"

        local idx=$((k * 5))
        bc_script+="
a[$((idx + 0))] = ($vz1) - ($vz2)
a[$((idx + 1))] = ($vx2) - ($vx1)
a[$((idx + 2))] = ($pz2) - ($pz1)
a[$((idx + 3))] = ($px1) - ($px2)
a[$((idx + 4))] = ($px1) * ($vz1) - ($pz1) * ($vx1) - (($px2) * ($vz2) - ($pz2) * ($vx2))
"
    done

    bc_script+='
/* Forward elimination for XZ */
piv = a[0]
for (r = 1; r < 4; r++) {
    if (a[r*5] != 0) {
        fac = a[r*5] / piv
        for (c = 0; c < 5; c++) {
            a[r*5+c] = a[r*5+c] - fac * a[c]
        }
    }
}

piv = a[6]
for (r = 2; r < 4; r++) {
    if (a[r*5+1] != 0) {
        fac = a[r*5+1] / piv
        for (c = 1; c < 5; c++) {
            a[r*5+c] = a[r*5+c] - fac * a[5+c]
        }
    }
}

piv = a[12]
if (a[17] != 0) {
    fac = a[17] / piv
    for (c = 2; c < 5; c++) {
        a[15+c] = a[15+c] - fac * a[10+c]
    }
}

/* Back substitution for XZ */
x3 = a[19] / a[18]
x2 = (a[14] - a[13] * x3) / a[12]
x1 = (a[9] - a[8] * x3 - a[7] * x2) / a[6]
x0 = (a[4] - a[3] * x3 - a[2] * x2 - a[1] * x1) / a[0]

rz = x1

/* Round to nearest integer and sum */
scale = 0
if (rx >= 0) { rx = (rx + 0.5) / 1 } else { rx = (rx - 0.5) / 1 }
if (ry >= 0) { ry = (ry + 0.5) / 1 } else { ry = (ry - 0.5) / 1 }
if (rz >= 0) { rz = (rz + 0.5) / 1 } else { rz = (rz - 0.5) / 1 }
print rx + ry + rz
print "\n"
'

    echo "$bc_script" | bc
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
