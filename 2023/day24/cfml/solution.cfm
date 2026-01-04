<cfscript>
/**
 * Day 24: Never Tell Me The Odds
 *
 * Part 1: Find 2D intersections of hailstone trajectories in the XY plane,
 *         counting pairs that intersect within bounds [200000000000000, 400000000000000]
 * Part 2: Find rock position and velocity that hits ALL hailstones,
 *         return sum of x, y, z starting coordinates
 */

// Java BigInteger for large number arithmetic
BigIntegerClass = createObject("java", "java.math.BigInteger");
ZERO = BigIntegerClass.ZERO;
ONE = BigIntegerClass.ONE;

// Helper functions to invoke BigInteger methods using Method.invoke (Lucee wraps them incorrectly)
function biMethod(obj, methodName, args) {
    var clazz = obj.getClass();
    var methods = clazz.getMethods();
    for (var m in methods) {
        if (m.getName() == methodName && arrayLen(m.getParameterTypes()) == arrayLen(args)) {
            return m.invoke(obj, javacast("java.lang.Object[]", args));
        }
    }
    throw("Method not found: " & methodName);
}

function biMod(a, b) { return biMethod(a, "mod", [b]); }
function biAbs(a) { return biMethod(a, "abs", []); }
function biGcd(a, b) { return biMethod(a, "gcd", [b]); }
function biMul(a, b) { return biMethod(a, "multiply", [b]); }
function biAdd(a, b) { return biMethod(a, "add", [b]); }
function biSub(a, b) { return biMethod(a, "subtract", [b]); }
function biDiv(a, b) { return biMethod(a, "divide", [b]); }
function biNeg(a) { return biMethod(a, "negate", []); }
function biCmp(a, b) { return biMethod(a, "compareTo", [b]); }
function biEq(a, b) { return biMethod(a, "equals", [b]); }
function biDbl(a) { return biMethod(a, "doubleValue", []); }
function biStr(a) { return biMethod(a, "toString", []); }

/**
 * Check if a value is a BigInteger
 */
function isBigInteger(value) {
    try {
        return value.getClass().getName() == "java.math.BigInteger";
    } catch (any e) {
        return false;
    }
}

/**
 * Create a BigInteger from a number or string
 * IMPORTANT: isSimpleValue() returns true for BigInteger in Lucee,
 * and toString() loses precision. We must check explicitly for BigInteger.
 */
function bi(value) {
    // If already a BigInteger, return as-is
    if (isBigInteger(value)) {
        return value;
    }
    // For small numbers or strings, convert to BigInteger
    // Note: We use javacast to ensure it's treated as a string
    return BigIntegerClass.init(javacast("string", trim(toString(value))));
}

/**
 * Fraction class for exact arithmetic using BigInteger
 * Stores numerator and denominator as BigInteger
 */
function fractionCreate(num, den) {
    // Convert to BigInteger using bi() which handles precision correctly
    num = bi(num);
    if (!isDefined("den")) {
        den = ONE;
    } else {
        den = bi(den);
    }

    // Handle zero denominator
    if (biEq(den, ZERO)) {
        throw("Division by zero");
    }

    // Normalize sign (denominator always positive)
    if (biCmp(den, ZERO) < 0) {
        num = biNeg(num);
        den = biNeg(den);
    }

    // Reduce by GCD
    var g = biGcd(biAbs(num), biAbs(den));

    return {
        num: biDiv(num, g),
        den: biDiv(den, g)
    };
}

/**
 * Fraction addition: a/b + c/d = (ad + bc) / bd
 */
function fractionAdd(f1, f2) {
    var newNum = biAdd(biMul(f1.num, f2.den), biMul(f2.num, f1.den));
    var newDen = biMul(f1.den, f2.den);
    return fractionCreate(newNum, newDen);
}

/**
 * Fraction subtraction: a/b - c/d = (ad - bc) / bd
 */
function fractionSub(f1, f2) {
    var newNum = biSub(biMul(f1.num, f2.den), biMul(f2.num, f1.den));
    var newDen = biMul(f1.den, f2.den);
    return fractionCreate(newNum, newDen);
}

/**
 * Fraction multiplication: (a/b) * (c/d) = ac / bd
 */
function fractionMul(f1, f2) {
    return fractionCreate(
        biMul(f1.num, f2.num),
        biMul(f1.den, f2.den)
    );
}

/**
 * Fraction division: (a/b) / (c/d) = ad / bc
 */
function fractionDiv(f1, f2) {
    return fractionCreate(
        biMul(f1.num, f2.den),
        biMul(f1.den, f2.num)
    );
}

/**
 * Check if fraction is zero
 */
function fractionIsZero(f) {
    return biEq(f.num, ZERO);
}

/**
 * Absolute value of fraction
 */
function fractionAbs(f) {
    return fractionCreate(biAbs(f.num), f.den);
}

/**
 * Compare fractions: returns true if f1 > f2
 */
function fractionGt(f1, f2) {
    // Compare a/b > c/d as a*d > c*b (assuming positive denominators)
    return biCmp(biMul(f1.num, f2.den), biMul(f2.num, f1.den)) > 0;
}

/**
 * Convert fraction to BigInteger (must be exact integer)
 */
function fractionToBigInt(f) {
    if (!biEq(biMod(f.num, f.den), ZERO)) {
        throw("Not an integer: " & biStr(f.num) & "/" & biStr(f.den));
    }
    return biDiv(f.num, f.den);
}

// Read and parse input
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputContent = fileRead(inputPath).trim();
lines = listToArray(inputContent, chr(10));

// Parse hailstones
hailstones = [];
for (line in lines) {
    if (len(trim(line)) == 0) continue;

    var parts = listToArray(line, "@");
    var posStr = trim(parts[1]);
    var velStr = trim(parts[2]);

    var posParts = listToArray(posStr, ",");
    var velParts = listToArray(velStr, ",");

    arrayAppend(hailstones, {
        px: bi(trim(posParts[1])),
        py: bi(trim(posParts[2])),
        pz: bi(trim(posParts[3])),
        vx: bi(trim(velParts[1])),
        vy: bi(trim(velParts[2])),
        vz: bi(trim(velParts[3]))
    });
}

/**
 * Find intersection of two hailstone paths in 2D (XY plane)
 * Returns struct with x, y or null if parallel or in the past
 */
function findIntersection2D(h1, h2) {
    var px1 = h1.px;
    var py1 = h1.py;
    var vx1 = h1.vx;
    var vy1 = h1.vy;
    var px2 = h2.px;
    var py2 = h2.py;
    var vx2 = h2.vx;
    var vy2 = h2.vy;

    // det = vx1 * (-vy2) - (-vx2) * vy1 = -vx1*vy2 + vx2*vy1
    var det = biSub(biMul(vx2, vy1), biMul(vx1, vy2));

    if (biEq(det, ZERO)) {
        return javacast("null", "");
    }

    var dx = biSub(px2, px1);
    var dy = biSub(py2, py1);

    // t1Num = dx * (-vy2) - (-vx2) * dy = -dx*vy2 + vx2*dy
    var t1Num = biSub(biMul(vx2, dy), biMul(dx, vy2));
    // t2Num = vx1 * dy - dx * vy1
    var t2Num = biSub(biMul(vx1, dy), biMul(dx, vy1));

    // Check signs (same sign as det means positive time)
    var t1Positive = (biCmp(t1Num, ZERO) >= 0) == (biCmp(det, ZERO) > 0);
    var t2Positive = (biCmp(t2Num, ZERO) >= 0) == (biCmp(det, ZERO) > 0);

    if (!t1Positive || !t2Positive) {
        return javacast("null", "");
    }

    // Calculate intersection point using double precision for bounds checking
    var t1 = biDbl(t1Num) / biDbl(det);
    var x = biDbl(px1) + biDbl(vx1) * t1;
    var y = biDbl(py1) + biDbl(vy1) * t1;

    return {x: x, y: y};
}

/**
 * Part 1: Count intersections within test area, in the future for both hailstones
 */
function part1(hailstones) {
    var minCoord = 200000000000000;
    var maxCoord = 400000000000000;
    var count = 0;

    var n = arrayLen(hailstones);
    for (var i = 1; i <= n; i++) {
        for (var j = i + 1; j <= n; j++) {
            var result = findIntersection2D(hailstones[i], hailstones[j]);
            if (isNull(result)) continue;

            if (result.x >= minCoord && result.x <= maxCoord &&
                result.y >= minCoord && result.y <= maxCoord) {
                count++;
            }
        }
    }

    return count;
}

/**
 * Solve system of linear equations using Gaussian elimination with fractions
 * matrix: 2D array of BigInteger values
 * rhs: array of BigInteger values
 * Returns array of Fraction solutions
 */
function solveSystem(matrix, rhs) {
    var n = arrayLen(matrix);

    // Create augmented matrix with fractions
    var aug = [];
    for (var i = 1; i <= n; i++) {
        var row = [];
        for (var j = 1; j <= n; j++) {
            arrayAppend(row, fractionCreate(matrix[i][j]));
        }
        arrayAppend(row, fractionCreate(rhs[i]));
        arrayAppend(aug, row);
    }

    // Forward elimination with partial pivoting
    for (var col = 1; col <= n; col++) {
        // Find pivot
        var maxRow = col;
        for (var row = col + 1; row <= n; row++) {
            if (fractionGt(fractionAbs(aug[row][col]), fractionAbs(aug[maxRow][col]))) {
                maxRow = row;
            }
        }
        // Swap rows
        var temp = aug[col];
        aug[col] = aug[maxRow];
        aug[maxRow] = temp;

        if (fractionIsZero(aug[col][col])) continue;

        // Eliminate column
        for (var row = col + 1; row <= n; row++) {
            if (!fractionIsZero(aug[row][col])) {
                var factor = fractionDiv(aug[row][col], aug[col][col]);
                for (var jj = col; jj <= n + 1; jj++) {
                    aug[row][jj] = fractionSub(aug[row][jj], fractionMul(factor, aug[col][jj]));
                }
            }
        }
    }

    // Back substitution
    var solution = [];
    for (var i = 1; i <= n; i++) {
        arrayAppend(solution, fractionCreate(0));
    }

    for (var i = n; i >= 1; i--) {
        solution[i] = aug[i][n + 1];
        for (var jj = i + 1; jj <= n; jj++) {
            solution[i] = fractionSub(solution[i], fractionMul(aug[i][jj], solution[jj]));
        }
        solution[i] = fractionDiv(solution[i], aug[i][i]);
    }

    return solution;
}

/**
 * Part 2: Find rock position and velocity that hits all hailstones
 * Returns sum of x, y, z starting coordinates
 */
function part2(hailstones) {
    // Use first 5 hailstones to build 4 linear equations
    var h = [];
    for (var i = 1; i <= 5; i++) {
        arrayAppend(h, hailstones[i]);
    }

    // Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
    // For hailstones i and j, the linearized equation is:
    // (vyi - vyj)*rx + (-vxi + vxj)*ry + (-pyi + pyj)*rvx + (pxi - pxj)*rvy
    //     = pxi*vyi - pyi*vxi - (pxj*vyj - pyj*vxj)
    var matrixXY = [];
    var rhsXY = [];

    for (var i = 1; i <= 4; i++) {
        var px1 = h[i].px;
        var py1 = h[i].py;
        var vx1 = h[i].vx;
        var vy1 = h[i].vy;
        var px2 = h[i + 1].px;
        var py2 = h[i + 1].py;
        var vx2 = h[i + 1].vx;
        var vy2 = h[i + 1].vy;

        // Coefficients for rx, ry, rvx, rvy
        var a = biSub(vy1, vy2);
        var b = biSub(vx2, vx1);
        var c = biSub(py2, py1);
        var d = biSub(px1, px2);
        var e = biSub(
            biSub(biMul(px1, vy1), biMul(py1, vx1)),
            biSub(biMul(px2, vy2), biMul(py2, vx2))
        );

        arrayAppend(matrixXY, [a, b, c, d]);
        arrayAppend(rhsXY, e);
    }

    var solXY = solveSystem(matrixXY, rhsXY);
    var rx = solXY[1];
    var ry = solXY[2];
    var rvx = solXY[3];
    var rvy = solXY[4];

    // Build system for XZ plane (4 equations, 4 unknowns: rx, rz, rvx, rvz)
    var matrixXZ = [];
    var rhsXZ = [];

    for (var i = 1; i <= 4; i++) {
        var px1 = h[i].px;
        var pz1 = h[i].pz;
        var vx1 = h[i].vx;
        var vz1 = h[i].vz;
        var px2 = h[i + 1].px;
        var pz2 = h[i + 1].pz;
        var vx2 = h[i + 1].vx;
        var vz2 = h[i + 1].vz;

        // Same structure as XY but with Z instead of Y
        var a = biSub(vz1, vz2);
        var b = biSub(vx2, vx1);
        var c = biSub(pz2, pz1);
        var d = biSub(px1, px2);
        var e = biSub(
            biSub(biMul(px1, vz1), biMul(pz1, vx1)),
            biSub(biMul(px2, vz2), biMul(pz2, vx2))
        );

        arrayAppend(matrixXZ, [a, b, c, d]);
        arrayAppend(rhsXZ, e);
    }

    var solXZ = solveSystem(matrixXZ, rhsXZ);
    var rz = solXZ[2];

    // Convert to BigInteger and sum
    var rxBig = fractionToBigInt(rx);
    var ryBig = fractionToBigInt(ry);
    var rzBig = fractionToBigInt(rz);

    return biStr(biAdd(biAdd(rxBig, ryBig), rzBig));
}

// Run solutions
writeOutput("Part 1: " & part1(hailstones) & chr(10));
try {
    writeOutput("Part 2: " & part2(hailstones) & chr(10));
} catch (any e) {
    writeOutput("Part 2 Error: " & e.message & chr(10));
}
</cfscript>
