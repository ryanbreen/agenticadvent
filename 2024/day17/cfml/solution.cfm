<cfscript>
/**
 * Day 17: Chronospatial Computer - 3-bit VM emulator
 */

// Read input
inputPath = getDirectoryFromPath(getCurrentTemplatePath()) & "../input.txt";
inputText = fileRead(inputPath);
// Third parameter (includeEmptyFields=true) preserves blank lines (line 4 is blank between registers and program)
lines = listToArray(inputText, chr(10), true);

// Parse registers
regA = val(reReplace(lines[1], "Register A: (\d+)", "\1"));
regB = val(reReplace(lines[2], "Register B: (\d+)", "\1"));
regC = val(reReplace(lines[3], "Register C: (\d+)", "\1"));

// Parse program - line 5 has the program (line 4 is blank)
programLine = lines[5];
programStr = reReplace(programLine, "Program: ([\d,]+)", "\1");
programArr = listToArray(programStr, ",");
program = arrayMap(programArr, function(p) { return val(p); });

/**
 * Run the 3-bit VM program using BigInteger for large number support
 */
function runProgram(aVal, bVal, cVal, prog) {
    var BigInteger = createObject("java", "java.math.BigInteger");
    var a = BigInteger.valueOf(javaCast("long", aVal));
    var b = BigInteger.valueOf(javaCast("long", bVal));
    var c = BigInteger.valueOf(javaCast("long", cVal));
    var SEVEN = BigInteger.valueOf(7);
    var ZERO = BigInteger.valueOf(0);

    var ip = 1; // CFML arrays are 1-indexed
    var output = [];
    var progLen = arrayLen(prog);

    while (ip <= progLen) {
        var opcode = prog[ip];
        var operand = prog[ip + 1];

        // Get combo value as BigInteger
        var comboVal = ZERO;
        if (operand <= 3) {
            comboVal = BigInteger.valueOf(operand);
        } else if (operand == 4) {
            comboVal = a;
        } else if (operand == 5) {
            comboVal = b;
        } else if (operand == 6) {
            comboVal = c;
        }

        switch (opcode) {
            case 0: // adv - A = A >> combo
                a = a.shiftRight(comboVal.intValue());
                break;
            case 1: // bxl - B = B XOR literal
                b = b.xor(BigInteger.valueOf(operand));
                break;
            case 2: // bst - B = combo & 7
                b = comboVal.and(SEVEN);
                break;
            case 3: // jnz - jump if A != 0
                if (!a.equals(ZERO)) {
                    ip = operand + 1; // Convert to 1-indexed
                    continue;
                }
                break;
            case 4: // bxc - B = B XOR C
                b = b.xor(c);
                break;
            case 5: // out - output combo & 7
                arrayAppend(output, comboVal.and(SEVEN).intValue());
                break;
            case 6: // bdv - B = A >> combo
                b = a.shiftRight(comboVal.intValue());
                break;
            case 7: // cdv - C = A >> combo
                c = a.shiftRight(comboVal.intValue());
                break;
        }
        ip += 2;
    }

    return output;
}

/**
 * Part 1: Run program and return comma-separated output
 */
function part1(a, b, c, prog) {
    var output = runProgram(a, b, c, prog);
    return arrayToList(output, ",");
}

/**
 * Part 2: Find initial A value that makes program output itself
 * Work backwards from the last digit, building A 3 bits at a time
 */
function part2(b, c, prog) {
    var BigInteger = createObject("java", "java.math.BigInteger");
    var progLen = arrayLen(prog);

    // Recursive search function
    var search = function(targetIdx, currentA) {
        if (targetIdx < 1) {
            return currentA;
        }

        // Try all 8 possible 3-bit values
        for (var bits = 0; bits < 8; bits++) {
            var candidateA = currentA.shiftLeft(3).or(BigInteger.valueOf(bits));

            // Skip A=0 at the start
            if (candidateA.equals(BigInteger.valueOf(0)) && targetIdx == progLen) {
                continue;
            }

            var output = runProgram(candidateA.longValue(), b, c, prog);

            // Check if output matches the suffix of the program
            var expectedLen = progLen - targetIdx + 1;
            var expectedSuffix = arraySlice(prog, targetIdx, expectedLen);
            if (output.equals(expectedSuffix)) {
                var result = search(targetIdx - 1, candidateA);
                if (!isNull(result)) {
                    return result;
                }
            }
        }

        return javaCast("null", "");
    };

    var result = search(progLen, BigInteger.valueOf(0));
    return result.toString();
}

// Run solutions
writeOutput("Part 1: " & part1(regA, regB, regC, program) & chr(10));
writeOutput("Part 2: " & part2(regB, regC, program) & chr(10));
</cfscript>
