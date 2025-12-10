<cfscript>
/**
 * Day 10: Factory - Linear algebra over GF(2) and rational arithmetic
 *
 * Note on indexing:
 * CFML uses 1-indexed arrays, but the algorithm logic is clearer with 0-indexed thinking.
 * Helper functions convert between 0-indexed algorithm logic and 1-indexed CFML arrays.
 */

// Array indexing helpers
// These convert between 0-indexed algorithm logic and 1-indexed CFML arrays
function getAt(arr, zeroIdx) {
    // Access array using 0-based index
    return arr[zeroIdx + 1];
}

function setAt(arr, zeroIdx, value) {
    // Set array element using 0-based index
    arr[zeroIdx + 1] = value;
}

function newArray(size, defaultValue) {
    // Create array of given size with default value
    var arr = [];
    for (var i = 1; i <= size; i++) {
        arrayAppend(arr, defaultValue);
    }
    return arr;
}

// Fraction functions - using structs instead of components
function gcd(a, b) {
    a = abs(a);
    b = abs(b);
    while (b != 0) {
        var t = b;
        b = a mod b;
        a = t;
    }
    return a;
}

function newFraction(num, den = 1) {
    if (den == 0) {
        throw(message="Denominator cannot be zero");
    }
    var g = gcd(num, den);
    var numerator = num / g;
    var denominator = den / g;

    // Keep denominator positive
    if (denominator < 0) {
        numerator = -numerator;
        denominator = -denominator;
    }
    return {numerator: numerator, denominator: denominator};
}

function fracAdd(f1, f2) {
    var num = f1.numerator * f2.denominator + f2.numerator * f1.denominator;
    var den = f1.denominator * f2.denominator;
    return newFraction(num, den);
}

function fracSubtract(f1, f2) {
    var num = f1.numerator * f2.denominator - f2.numerator * f1.denominator;
    var den = f1.denominator * f2.denominator;
    return newFraction(num, den);
}

function fracMultiply(f1, f2) {
    var num = f1.numerator * f2.numerator;
    var den = f1.denominator * f2.denominator;
    return newFraction(num, den);
}

function fracDivide(f1, f2) {
    var num = f1.numerator * f2.denominator;
    var den = f1.denominator * f2.numerator;
    return newFraction(num, den);
}

function fracEquals(f1, f2) {
    return f1.numerator == f2.numerator && f1.denominator == f2.denominator;
}

function fracLessThan(f1, f2) {
    return f1.numerator * f2.denominator < f2.numerator * f1.denominator;
}

function fracGreaterThan(f1, f2) {
    return f1.numerator * f2.denominator > f2.numerator * f1.denominator;
}

function fracToFloat(f) {
    return f.numerator / f.denominator;
}

function fracIsInteger(f) {
    return f.denominator == 1;
}

function fracToInt(f) {
    return f.numerator / f.denominator;
}

function parseLine(line) {
    // Extract indicator pattern [.##.] - find content between square brackets
    var leftBracket = chr(91);  // [
    var rightBracket = chr(93);  // ]
    var hash = chr(35);  // #

    var start = find(leftBracket, line) + 1;
    var end = find(rightBracket, line);
    var indicator = mid(line, start, end - start);
    var nLights = len(indicator);

    // Target state: 1 where # appears
    var target = 0;
    for (var i = 1; i <= len(indicator); i++) {
        if (mid(indicator, i, 1) == hash) {
            target = bitOr(target, bitSHLN(1, i - 1));
        }
    }

    // Extract button schematics (0,1,2) etc.
    var buttons = [];
    var pos = 1;
    var openParen = chr(40);  // (
    var closeParen = chr(41);  // )

    while (true) {
        var openPos = find(openParen, line, pos);
        if (openPos == 0) break;
        var closePos = find(closeParen, line, openPos);
        if (closePos == 0) break;

        var buttonStr = mid(line, openPos + 1, closePos - openPos - 1);
        var indices = listToArray(buttonStr);
        var mask = 0;
        for (var idx in indices) {
            mask = bitOr(mask, bitSHLN(1, val(idx)));
        }
        arrayAppend(buttons, mask);
        pos = closePos + 1;
    }

    return {nLights: nLights, target: target, buttons: buttons};
}

function parseLinePart2(line) {
    // Extract joltage requirements {3,5,4,7}
    var leftBrace = chr(123);  // {
    var rightBrace = chr(125);  // }

    var start = find(leftBrace, line) + 1;
    var end = find(rightBrace, line);
    var joltageStr = mid(line, start, end - start);
    var joltage = [];
    for (var j in listToArray(joltageStr)) {
        arrayAppend(joltage, val(j));
    }
    var nCounters = arrayLen(joltage);

    // Extract button schematics
    var buttons = [];
    var pos = 1;
    var openParen = chr(40);  // (
    var closeParen = chr(41);  // )

    while (true) {
        var openPos = find(openParen, line, pos);
        if (openPos == 0) break;
        var closePos = find(closeParen, line, openPos);
        if (closePos == 0) break;

        var buttonStr = mid(line, openPos + 1, closePos - openPos - 1);
        var indices = [];
        for (var idx in listToArray(buttonStr)) {
            arrayAppend(indices, val(idx));
        }
        arrayAppend(buttons, indices);
        pos = closePos + 1;
    }

    return {nCounters: nCounters, joltage: joltage, buttons: buttons};
}

function countBits(n) {
    var count = 0;
    while (n > 0) {
        count += bitAnd(n, 1);
        n = bitSHRN(n, 1);
    }
    return count;
}

function solveMachineBrute(nLights, target, buttons) {
    var nButtons = arrayLen(buttons);
    var minPresses = 999999999;

    // Try all 2^nButtons combinations
    for (var mask = 0; mask < bitSHLN(1, nButtons); mask++) {
        var state = 0;
        var presses = 0;
        // Loop using 0-indexed logic, use getAt for array access
        for (var i = 0; i < nButtons; i++) {
            if (bitAnd(mask, bitSHLN(1, i)) != 0) {
                state = bitXor(state, getAt(buttons, i));
                presses++;
            }
        }

        if (state == target) {
            minPresses = min(minPresses, presses);
        }
    }

    return minPresses < 999999999 ? minPresses : 0;
}

function part1(lines) {
    var total = 0;
    for (var line in lines) {
        line = trim(line);
        if (len(line) == 0) continue;

        var parsed = parseLine(line);
        var minPresses = solveMachineBrute(parsed.nLights, parsed.target, parsed.buttons);
        total += minPresses;
    }
    return total;
}

function searchNDimensional(nButtons, particular, nullVectors, nFree, bound) {
    // Recursive search for N-dimensional parameter space (4+ free variables)
    var zero = newFraction(0);
    var minTotal = 999999999;

    // We'll use a simple bounded grid search
    // Generate all combinations of parameters in [-bound, bound]
    var params = [];
    for (var i = 0; i < nFree; i++) {
        arrayAppend(params, -bound);
    }

    // Iterate through all combinations
    var done = false;
    while (!done) {
        // Compute solution for current parameters
        var solution = [];
        for (var j = 0; j < nButtons; j++) {
            var val = getAt(particular, j);
            for (var i = 0; i < nFree; i++) {
                var t = newFraction(getAt(params, i));
                var nullVec = getAt(nullVectors, i);
                val = fracAdd(val, fracMultiply(t, getAt(nullVec, j)));
            }
            arrayAppend(solution, val);
        }

        // Check if solution is valid (all non-negative integers)
        var valid = true;
        var total = 0;
        for (var j = 0; j < nButtons; j++) {
            var val = getAt(solution, j);
            if (fracLessThan(val, zero) || !fracIsInteger(val)) {
                valid = false;
                break;
            }
            total += fracToInt(val);
        }

        if (valid && total < minTotal) {
            minTotal = total;
        }

        // Increment parameters (like a counter)
        var carry = true;
        for (var i = 0; i < nFree && carry; i++) {
            var newVal = getAt(params, i) + 1;
            if (newVal > bound) {
                setAt(params, i, -bound);
            } else {
                setAt(params, i, newVal);
                carry = false;
            }
        }
        if (carry) done = true;
    }

    return minTotal < 999999999 ? minTotal : 0;
}

function solveMachinePart2(nCounters, joltage, buttons) {
    var nButtons = arrayLen(buttons);
    var zero = newFraction(0);
    var one = newFraction(1);
    var negOne = newFraction(-1);

    if (nButtons == 0) {
        var allZero = true;
        for (var j in joltage) {
            if (j != 0) allZero = false;
        }
        return allZero ? 0 : 999999999;
    }

    // Build matrix A (nCounters x nButtons)
    // Using 0-indexed logic: A[i][j] represents counter i affected by button j
    var A = [];
    for (var i = 0; i < nCounters; i++) {
        arrayAppend(A, newArray(nButtons, zero));
    }

    // For each button, mark which counters it affects
    for (var j = 0; j < nButtons; j++) {
        var indices = getAt(buttons, j);  // Get button's counter indices
        for (var idx in indices) {
            if (idx < nCounters) {
                setAt(getAt(A, idx), j, one);  // A[idx][j] = 1
            }
        }
    }

    // Build target vector b from joltage requirements
    var b = [];
    for (var j in joltage) {
        arrayAppend(b, newFraction(j));
    }

    // Build augmented matrix [A | b]
    // Each row has nButtons columns for A plus 1 column for b
    var aug = [];
    for (var i = 0; i < nCounters; i++) {
        var row = [];
        for (var j = 0; j < nButtons; j++) {
            arrayAppend(row, getAt(getAt(A, i), j));
        }
        arrayAppend(row, getAt(b, i));
        arrayAppend(aug, row);
    }

    // Gaussian elimination with 0-indexed logic
    // pivotCols stores {col, row} pairs tracking which column was pivoted in which row
    var pivotCols = [];
    var pivotRow = 0;  // 0-indexed current pivot row

    // Process each column looking for pivots
    for (var col = 0; col < nButtons; col++) {
        // Find non-zero entry in this column at or below pivotRow
        var foundRow = -1;
        for (var row = pivotRow; row < nCounters; row++) {
            if (!fracEquals(getAt(getAt(aug, row), col), zero)) {
                foundRow = row;
                break;
            }
        }

        if (foundRow == -1) continue;  // No pivot in this column

        // Swap rows if needed
        if (foundRow != pivotRow) {
            var temp = getAt(aug, pivotRow);
            setAt(aug, pivotRow, getAt(aug, foundRow));
            setAt(aug, foundRow, temp);
        }

        // Record this pivot
        arrayAppend(pivotCols, {col: col, row: pivotRow});

        // Scale pivot row so pivot element becomes 1
        var pivotVal = getAt(getAt(aug, pivotRow), col);
        for (var c = 0; c <= nButtons; c++) {  // Include augmented column
            var currentVal = getAt(getAt(aug, pivotRow), c);
            setAt(getAt(aug, pivotRow), c, fracDivide(currentVal, pivotVal));
        }

        // Eliminate this column in all other rows
        for (var row = 0; row < nCounters; row++) {
            if (row != pivotRow) {
                var factor = getAt(getAt(aug, row), col);
                if (!fracEquals(factor, zero)) {
                    for (var c = 0; c <= nButtons; c++) {
                        var oldVal = getAt(getAt(aug, row), c);
                        var pivotRowVal = getAt(getAt(aug, pivotRow), c);
                        setAt(getAt(aug, row), c, fracSubtract(oldVal, fracMultiply(factor, pivotRowVal)));
                    }
                }
            }
        }

        pivotRow++;
    }

    // Check for inconsistency (rows with 0=b where b!=0)
    for (var row = pivotRow; row < nCounters; row++) {
        if (!fracEquals(getAt(getAt(aug, row), nButtons), zero)) {
            return 999999999; // No solution - inconsistent system
        }
    }

    // Identify free variables (columns that don't have pivots)
    var pivotColSet = {};
    for (var pc in pivotCols) {
        pivotColSet[pc.col] = true;
    }

    var freeVars = [];
    for (var c = 0; c < nButtons; c++) {
        if (!structKeyExists(pivotColSet, c)) {
            arrayAppend(freeVars, c);
        }
    }

    // If no free variables, we have a unique solution
    if (arrayLen(freeVars) == 0) {
        var solution = newArray(nButtons, zero);

        // Extract solution values from augmented matrix
        for (var pc in pivotCols) {
            setAt(solution, pc.col, getAt(getAt(aug, pc.row), nButtons));
        }

        // Verify solution is non-negative integers
        var total = 0;
        for (var i = 0; i < nButtons; i++) {
            var val = getAt(solution, i);
            if (fracLessThan(val, zero) || !fracIsInteger(val)) {
                return 999999999;  // Invalid solution
            }
            total += fracToInt(val);
        }
        return total;
    }

    // Extract null space vectors
    // For each free variable, create a vector in the null space
    var nullVectors = [];
    for (var fv in freeVars) {
        var vec = newArray(nButtons, zero);
        setAt(vec, fv, one);  // Free variable itself is 1

        // Set pivot variables based on how they depend on this free variable
        for (var pc in pivotCols) {
            var coeff = getAt(getAt(aug, pc.row), fv);
            setAt(vec, pc.col, fracMultiply(coeff, negOne));
        }
        arrayAppend(nullVectors, vec);
    }

    // Extract particular solution (set all free vars to 0)
    var particular = newArray(nButtons, zero);
    for (var pc in pivotCols) {
        var rhsVal = getAt(getAt(aug, pc.row), nButtons);
        setAt(particular, pc.col, rhsVal);
    }

    var nFree = arrayLen(freeVars);
    var minTotal = 999999999;

    // Handle single free variable case
    // Solution: x = particular + t * nullVector, find valid integer t minimizing sum
    if (nFree == 1) {
        var tLow = -999999999;
        var tHigh = 999999999;
        var nullVec = getAt(nullVectors, 0);  // Only one null vector

        // For each button, require particular[j] + t*nullVec[j] >= 0
        for (var j = 0; j < nButtons; j++) {
            var p = getAt(particular, j);
            var nv = getAt(nullVec, j);

            if (fracEquals(nv, zero)) {
                // No dependence on t, just check if p >= 0
                if (fracLessThan(p, zero)) {
                    return 999999999;  // Infeasible
                }
            } else if (fracGreaterThan(nv, zero)) {
                // t >= -p/nv, so tLow = ceil(-p/nv)
                var bound = fracToFloat(fracDivide(fracMultiply(p, negOne), nv));
                tLow = max(tLow, ceiling(bound));
            } else {
                // t <= -p/nv, so tHigh = floor(-p/nv)
                var bound = fracToFloat(fracDivide(fracMultiply(p, negOne), nv));
                tHigh = min(tHigh, floor(bound));
            }
        }

        if (tLow > tHigh) return 999999999;  // No valid t

        // Try all valid t values and find minimum sum
        for (var t = tLow; t <= tHigh; t++) {
            var tFrac = newFraction(t);
            var total = 0;
            var valid = true;

            for (var j = 0; j < nButtons; j++) {
                var val = fracAdd(getAt(particular, j), fracMultiply(tFrac, getAt(nullVec, j)));
                if (fracLessThan(val, zero) || !fracIsInteger(val)) {
                    valid = false;
                    break;
                }
                total += fracToInt(val);
            }

            if (valid) {
                minTotal = min(minTotal, total);
            }
        }

        return minTotal < 999999999 ? minTotal : 0;
    }

    // For 2+ free variables, use bounded search
    var maxJ = 0;
    for (var j in joltage) {
        maxJ = max(maxJ, j);
    }

    // Handle 2 free variables case
    // Solution: x = particular + t0*nullVec[0] + t1*nullVec[1]
    if (nFree == 2) {
        var nullVec0 = getAt(nullVectors, 0);
        var nullVec1 = getAt(nullVectors, 1);

        for (var t0 = -maxJ; t0 <= maxJ; t0++) {
            var t0Frac = newFraction(t0);

            // Compute intermediate = particular + t0*nullVec0
            var intermediate = [];
            for (var j = 0; j < nButtons; j++) {
                arrayAppend(intermediate, fracAdd(getAt(particular, j), fracMultiply(t0Frac, getAt(nullVec0, j))));
            }

            // Find bounds on t1 such that intermediate + t1*nullVec1 >= 0
            var t1Low = -999999999;
            var t1High = 999999999;

            for (var j = 0; j < nButtons; j++) {
                var p = fracToFloat(getAt(intermediate, j));
                var nv = fracToFloat(getAt(nullVec1, j));

                if (nv > 0) {
                    t1Low = max(t1Low, ceiling(-p / nv));
                } else if (nv < 0) {
                    t1High = min(t1High, floor(-p / nv));
                }
            }

            // Try all valid t1 values
            for (var t1 = t1Low; t1 <= t1High; t1++) {
                var t1Frac = newFraction(t1);
                var valid = true;
                var total = 0;

                for (var j = 0; j < nButtons; j++) {
                    var val = fracAdd(getAt(intermediate, j), fracMultiply(t1Frac, getAt(nullVec1, j)));
                    if (fracLessThan(val, zero) || !fracIsInteger(val)) {
                        valid = false;
                        break;
                    }
                    total += fracToInt(val);
                }

                if (valid && total < minTotal) {
                    minTotal = total;
                }
            }
        }

        return minTotal < 999999999 ? minTotal : 0;
    }

    // Handle 3 free variables with optimized search
    // Solution: x = particular + t0*nullVec[0] + t1*nullVec[1] + t2*nullVec[2]
    if (nFree == 3) {
        var bound = min(maxJ, 50);  // Limit search space
        var nullVec0 = getAt(nullVectors, 0);
        var nullVec1 = getAt(nullVectors, 1);
        var nullVec2 = getAt(nullVectors, 2);

        for (var t0 = -bound; t0 <= bound; t0++) {
            var t0Frac = newFraction(t0);

            // Compute inter0 = particular + t0*nullVec0
            var inter0 = [];
            for (var j = 0; j < nButtons; j++) {
                arrayAppend(inter0, fracAdd(getAt(particular, j), fracMultiply(t0Frac, getAt(nullVec0, j))));
            }

            for (var t1 = -bound; t1 <= bound; t1++) {
                var t1Frac = newFraction(t1);

                // Compute inter1 = inter0 + t1*nullVec1
                var inter1 = [];
                for (var j = 0; j < nButtons; j++) {
                    arrayAppend(inter1, fracAdd(getAt(inter0, j), fracMultiply(t1Frac, getAt(nullVec1, j))));
                }

                // Compute tight bounds for t2 such that inter1 + t2*nullVec2 >= 0
                var t2Low = -999999999;
                var t2High = 999999999;
                for (var j = 0; j < nButtons; j++) {
                    var p = fracToFloat(getAt(inter1, j));
                    var nv = fracToFloat(getAt(nullVec2, j));
                    if (nv > 0) {
                        t2Low = max(t2Low, ceiling(-p / nv));
                    } else if (nv < 0) {
                        t2High = min(t2High, floor(-p / nv));
                    }
                }

                // Try all valid t2 values
                for (var t2 = t2Low; t2 <= t2High; t2++) {
                    var t2Frac = newFraction(t2);
                    var valid = true;
                    var total = 0;

                    for (var j = 0; j < nButtons; j++) {
                        var val = fracAdd(getAt(inter1, j), fracMultiply(t2Frac, getAt(nullVec2, j)));
                        if (fracLessThan(val, zero) || !fracIsInteger(val)) {
                            valid = false;
                            break;
                        }
                        total += fracToInt(val);
                    }

                    if (valid && total < minTotal) {
                        minTotal = total;
                    }
                }
            }
        }

        return minTotal < 999999999 ? minTotal : 0;
    }

    // Handle 4+ free variables using generalized bounded search
    // Solution: x = particular + sum(t_i * nullVec[i])
    if (nFree >= 4) {
        var bound = min(maxJ, 20);  // Smaller bound for higher dimensions

        // Generate all combinations of parameter values
        return searchNDimensional(nButtons, particular, nullVectors, nFree, bound);
    }

    return 0;
}

function part2(lines) {
    var total = 0;
    for (var line in lines) {
        line = trim(line);
        if (len(line) == 0) continue;

        var parsed = parseLinePart2(line);
        var minPresses = solveMachinePart2(parsed.nCounters, parsed.joltage, parsed.buttons);
        total += minPresses;
    }
    return total;
}

// Main execution
var scriptPath = getDirectoryFromPath(getCurrentTemplatePath());
var inputFile = scriptPath & "../input.txt";
var content = fileRead(inputFile);
var lines = listToArray(content, chr(10));

writeOutput("Part 1: " & part1(lines) & chr(10));
writeOutput("Part 2: " & part2(lines) & chr(10));
</cfscript>
