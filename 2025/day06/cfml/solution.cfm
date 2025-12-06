<cfscript>
// Read input file
currentDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = currentDir & "../input.txt";
inputText = fileRead(inputPath).trim();
lines = listToArray(inputText, chr(10));

/**
 * Parse the worksheet into an array of structs with numbers and operator
 */
function parseProblems(lines) {
    if (arrayLen(lines) == 0) {
        return [];
    }

    // Find the operator row (last non-empty row with only +, *, and spaces)
    var opRowIdx = arrayLen(lines);
    while (opRowIdx >= 1) {
        var line = lines[opRowIdx];
        if (len(trim(line)) == 0) {
            opRowIdx--;
            continue;
        }

        // Check if line contains only +, *, and spaces
        var valid = true;
        for (var i = 1; i <= len(line); i++) {
            var ch = mid(line, i, 1);
            if (ch != '+' && ch != '*' && ch != ' ') {
                valid = false;
                break;
            }
        }

        if (valid) {
            break;
        }
        opRowIdx--;
    }

    if (opRowIdx < 1) {
        return [];
    }

    var opRow = lines[opRowIdx];
    var numberRows = [];
    for (var i = 1; i < opRowIdx; i++) {
        arrayAppend(numberRows, lines[i]);
    }

    // Find max width
    var maxWidth = 0;
    for (var line in lines) {
        if (len(line) > maxWidth) {
            maxWidth = len(line);
        }
    }

    // Pad all rows to the same width (pad on the right with spaces)
    var paddedNumberRows = [];
    for (var row in numberRows) {
        var padded = row & repeatString(" ", maxWidth - len(row));
        arrayAppend(paddedNumberRows, padded);
    }
    var paddedOpRow = opRow & repeatString(" ", maxWidth - len(opRow));

    // Helper function to check if column is all spaces
    var isColumnAllSpaces = function(col) {
        for (var row in paddedNumberRows) {
            if (mid(row, col, 1) != ' ') {
                return false;
            }
        }
        return mid(paddedOpRow, col, 1) == ' ';
    };

    // Find problem boundaries
    var problems = [];
    var col = 1;

    while (col <= maxWidth) {
        // Skip separator columns (all spaces)
        while (col <= maxWidth && isColumnAllSpaces(col)) {
            col++;
        }

        if (col > maxWidth) {
            break;
        }

        // Find the end of this problem
        var startCol = col;
        while (col <= maxWidth) {
            if (isColumnAllSpaces(col)) {
                break;
            }
            col++;
        }

        var endCol = col - 1;

        // Extract numbers and operator for this problem
        var numbers = [];
        for (var row in paddedNumberRows) {
            var numStr = trim(mid(row, startCol, endCol - startCol + 1));
            if (len(numStr) > 0) {
                arrayAppend(numbers, numStr);
            }
        }

        var opStr = trim(mid(paddedOpRow, startCol, endCol - startCol + 1));
        if (len(opStr) > 0 && arrayLen(numbers) > 0) {
            arrayAppend(problems, {
                "numbers": numbers,
                "operator": opStr
            });
        }
    }

    return problems;
}

/**
 * Solve a single problem given numbers and operator
 */
function solveProblem(numbers, op) {
    if (op == '+') {
        var sum = "0";
        for (var n in numbers) {
            sum = precisionEvaluate(sum & " + " & n);
        }
        return sum;
    } else if (op == '*') {
        var product = "1";
        for (var n in numbers) {
            product = precisionEvaluate(product & " * " & n);
        }
        return product;
    }
    return "0";
}

/**
 * Part 1: Read problems left-to-right
 */
function part1() {
    var problems = parseProblems(lines);
    var total = "0";
    for (var problem in problems) {
        var result = solveProblem(problem.numbers, problem.operator);
        total = precisionEvaluate(total & " + " & result);
    }
    return total;
}

/**
 * Parse the worksheet for part 2 - reading right-to-left columns
 */
function parseProblemsPart2(lines) {
    if (arrayLen(lines) == 0) {
        return [];
    }

    // Find the operator row (last non-empty row with only +, *, and spaces)
    var opRowIdx = arrayLen(lines);
    while (opRowIdx >= 1) {
        var line = lines[opRowIdx];
        if (len(trim(line)) == 0) {
            opRowIdx--;
            continue;
        }

        // Check if line contains only +, *, and spaces
        var valid = true;
        for (var i = 1; i <= len(line); i++) {
            var ch = mid(line, i, 1);
            if (ch != '+' && ch != '*' && ch != ' ') {
                valid = false;
                break;
            }
        }

        if (valid) {
            break;
        }
        opRowIdx--;
    }

    if (opRowIdx < 1) {
        return [];
    }

    var opRow = lines[opRowIdx];
    var numberRows = [];
    for (var i = 1; i < opRowIdx; i++) {
        arrayAppend(numberRows, lines[i]);
    }

    // Find max width
    var maxWidth = 0;
    for (var line in lines) {
        if (len(line) > maxWidth) {
            maxWidth = len(line);
        }
    }

    // Pad all rows to the same width (pad on the right with spaces)
    var paddedNumberRows = [];
    for (var row in numberRows) {
        var padded = row & repeatString(" ", maxWidth - len(row));
        arrayAppend(paddedNumberRows, padded);
    }
    var paddedOpRow = opRow & repeatString(" ", maxWidth - len(opRow));

    // Helper function to check if column is all spaces
    var isColumnAllSpaces = function(col) {
        for (var row in paddedNumberRows) {
            if (mid(row, col, 1) != ' ') {
                return false;
            }
        }
        return mid(paddedOpRow, col, 1) == ' ';
    };

    // Find problem boundaries
    var problems = [];
    var col = 1;

    while (col <= maxWidth) {
        // Skip separator columns (all spaces)
        while (col <= maxWidth && isColumnAllSpaces(col)) {
            col++;
        }

        if (col > maxWidth) {
            break;
        }

        // Find the end of this problem
        var startCol = col;
        while (col <= maxWidth) {
            if (isColumnAllSpaces(col)) {
                break;
            }
            col++;
        }

        var endCol = col - 1;

        // For Part 2: Read columns right-to-left, each column forms a number
        // reading top-to-bottom as most-to-least significant digit
        var numbers = [];
        for (var c = endCol; c >= startCol; c--) {  // Right to left
            var digits = [];
            for (var row in paddedNumberRows) {
                var ch = mid(row, c, 1);
                if (isNumeric(ch)) {
                    arrayAppend(digits, ch);
                }
            }
            if (arrayLen(digits) > 0) {
                // Join digits to form number (top=most significant, bottom=least)
                var num = arrayToList(digits, "");
                arrayAppend(numbers, num);
            }
        }

        var opStr = trim(mid(paddedOpRow, startCol, endCol - startCol + 1));
        if (len(opStr) > 0 && arrayLen(numbers) > 0) {
            arrayAppend(problems, {
                "numbers": numbers,
                "operator": opStr
            });
        }
    }

    return problems;
}

/**
 * Part 2: Read problems right-to-left in columns
 */
function part2() {
    var problems = parseProblemsPart2(lines);
    var total = "0";
    for (var problem in problems) {
        var result = solveProblem(problem.numbers, problem.operator);
        total = precisionEvaluate(total & " + " & result);
    }
    return total;
}

// Run both parts
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
