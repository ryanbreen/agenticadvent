<cfscript>
/**
 * Day 10: Factory - Linear algebra over GF(2) and rational arithmetic
 */

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
        for (var i = 0; i < nButtons; i++) {
            if (bitAnd(mask, bitSHLN(1, i)) != 0) {
                state = bitXor(state, buttons[i + 1]);
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
    var A = [];
    for (var i = 1; i <= nCounters; i++) {
        var row = [];
        for (var j = 1; j <= nButtons; j++) {
            arrayAppend(row, zero);
        }
        arrayAppend(A, row);
    }

    for (var j = 1; j <= nButtons; j++) {
        var indices = buttons[j];
        for (var idx in indices) {
            if (idx < nCounters) {
                A[idx + 1][j] = one;
            }
        }
    }

    var b = [];
    for (var j in joltage) {
        arrayAppend(b, newFraction(j));
    }

    // Augmented matrix [A | b]
    var aug = [];
    for (var i = 1; i <= nCounters; i++) {
        var row = [];
        for (var j = 1; j <= nButtons; j++) {
            arrayAppend(row, A[i][j]);
        }
        arrayAppend(row, b[i]);
        arrayAppend(aug, row);
    }

    // Gaussian elimination
    var pivotCols = [];
    var pivotRow = 1;

    for (var col = 1; col <= nButtons; col++) {
        // Find non-zero entry
        var found = 0;
        for (var row = pivotRow; row <= nCounters; row++) {
            if (!fracEquals(aug[row][col], zero)) {
                found = row;
                break;
            }
        }

        if (found == 0) continue;

        // Swap rows
        var temp = aug[pivotRow];
        aug[pivotRow] = aug[found];
        aug[found] = temp;

        arrayAppend(pivotCols, {col: col, row: pivotRow});

        // Scale pivot row
        var scale = aug[pivotRow][col];
        for (var c = 1; c <= nButtons + 1; c++) {
            aug[pivotRow][c] = fracDivide(aug[pivotRow][c], scale);
        }

        // Eliminate column
        for (var row = 1; row <= nCounters; row++) {
            if (row != pivotRow && !fracEquals(aug[row][col], zero)) {
                var factor = aug[row][col];
                for (var c = 1; c <= nButtons + 1; c++) {
                    aug[row][c] = fracSubtract(aug[row][c], fracMultiply(factor, aug[pivotRow][c]));
                }
            }
        }

        pivotRow++;
    }

    // Check for inconsistency
    for (var row = pivotRow; row <= nCounters; row++) {
        if (!fracEquals(aug[row][nButtons + 1], zero)) {
            return 999999999; // No solution
        }
    }

    // Identify free variables
    var pivotColSet = {};
    for (var pc in pivotCols) {
        pivotColSet[pc.col] = true;
    }

    var freeVars = [];
    for (var c = 1; c <= nButtons; c++) {
        if (!structKeyExists(pivotColSet, c)) {
            arrayAppend(freeVars, c);
        }
    }

    // If no free variables, unique solution
    if (arrayLen(freeVars) == 0) {
        var solution = [];
        for (var i = 1; i <= nButtons; i++) {
            arrayAppend(solution, zero);
        }

        for (var pc in pivotCols) {
            solution[pc.col] = aug[pc.row][nButtons + 1];
        }

        var total = 0;
        for (var val in solution) {
            if (fracLessThan(val, zero) || !fracIsInteger(val)) {
                return 999999999;
            }
            total += fracToInt(val);
        }
        return total;
    }

    // Extract null space vectors
    var nullVectors = [];
    for (var fv in freeVars) {
        var vec = [];
        for (var i = 1; i <= nButtons; i++) {
            arrayAppend(vec, zero);
        }
        vec[fv] = one;

        for (var pc in pivotCols) {
            vec[pc.col] = fracMultiply(aug[pc.row][fv], negOne);
        }
        arrayAppend(nullVectors, vec);
    }

    // Particular solution
    var particular = [];
    for (var i = 1; i <= nButtons; i++) {
        arrayAppend(particular, zero);
    }
    for (var pc in pivotCols) {
        particular[pc.col] = aug[pc.row][nButtons + 1];
    }

    var nFree = arrayLen(freeVars);
    var minTotal = 999999999;

    // Handle single free variable case
    if (nFree == 1) {
        var tLow = -999999999;
        var tHigh = 999999999;

        for (var j = 1; j <= nButtons; j++) {
            var p = particular[j];
            var nv = nullVectors[1][j];

            if (fracEquals(nv, zero)) {
                if (fracLessThan(p, zero)) {
                    return 999999999;
                }
            } else if (fracGreaterThan(nv, zero)) {
                var bound = fracToFloat(fracDivide(fracMultiply(p, negOne), nv));
                tLow = max(tLow, ceiling(bound));
            } else {
                var bound = fracToFloat(fracDivide(fracMultiply(p, negOne), nv));
                tHigh = min(tHigh, floor(bound));
            }
        }

        if (tLow > tHigh) return 999999999;

        for (var t = tLow; t <= tHigh; t++) {
            var tFrac = newFraction(t);
            var total = 0;
            var valid = true;

            for (var j = 1; j <= nButtons; j++) {
                var val = fracAdd(particular[j], fracMultiply(tFrac, nullVectors[1][j]));
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

    // For simplicity with 2+ free vars, use bounded search
    var maxJ = 0;
    for (var j in joltage) {
        maxJ = max(maxJ, j);
    }

    if (nFree == 2) {
        for (var t0 = -maxJ; t0 <= maxJ; t0++) {
            var t0Frac = newFraction(t0);
            var intermediate = [];
            for (var j = 1; j <= nButtons; j++) {
                arrayAppend(intermediate, fracAdd(particular[j], fracMultiply(t0Frac, nullVectors[1][j])));
            }

            var t1Low = -999999999;
            var t1High = 999999999;

            for (var j = 1; j <= nButtons; j++) {
                var p = fracToFloat(intermediate[j]);
                var nv = fracToFloat(nullVectors[2][j]);

                if (nv > 0) {
                    t1Low = max(t1Low, ceiling(-p / nv));
                } else if (nv < 0) {
                    t1High = min(t1High, floor(-p / nv));
                }
            }

            for (var t1 = t1Low; t1 <= t1High; t1++) {
                var t1Frac = newFraction(t1);
                var valid = true;
                var total = 0;

                for (var j = 1; j <= nButtons; j++) {
                    var val = fracAdd(intermediate[j], fracMultiply(t1Frac, nullVectors[2][j]));
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
    if (nFree == 3) {
        var bound = min(maxJ, 50);  // Limit search space

        for (var t0 = -bound; t0 <= bound; t0++) {
            var t0Frac = newFraction(t0);
            var inter0 = [];
            for (var j = 1; j <= nButtons; j++) {
                arrayAppend(inter0, fracAdd(particular[j], fracMultiply(t0Frac, nullVectors[1][j])));
            }

            for (var t1 = -bound; t1 <= bound; t1++) {
                var t1Frac = newFraction(t1);
                var inter1 = [];
                for (var j = 1; j <= nButtons; j++) {
                    arrayAppend(inter1, fracAdd(inter0[j], fracMultiply(t1Frac, nullVectors[2][j])));
                }

                // Compute tight bounds for t2
                var t2Low = -999999999;
                var t2High = 999999999;
                for (var j = 1; j <= nButtons; j++) {
                    var p = fracToFloat(inter1[j]);
                    var nv = fracToFloat(nullVectors[3][j]);
                    if (nv > 0) {
                        t2Low = max(t2Low, ceiling(-p / nv));
                    } else if (nv < 0) {
                        t2High = min(t2High, floor(-p / nv));
                    }
                }

                for (var t2 = t2Low; t2 <= t2High; t2++) {
                    var t2Frac = newFraction(t2);
                    var valid = true;
                    var total = 0;

                    for (var j = 1; j <= nButtons; j++) {
                        var val = fracAdd(inter1[j], fracMultiply(t2Frac, nullVectors[3][j]));
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

    // For 4+ free vars, return 0
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
