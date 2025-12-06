<cfscript>
try {
    // Locate and validate input file
    currentDir = getDirectoryFromPath(getCurrentTemplatePath());
    inputPath = currentDir & "../input.txt";

    // Validate file exists before attempting to read
    if (!fileExists(inputPath)) {
        echo("Error: Input file not found at " & inputPath & chr(10));
        abort;
    }

    // Read and parse input file into lines
    inputText = fileRead(inputPath).trim();
    lines = listToArray(inputText, chr(10));

    /**
     * Check if a character is a symbol (not a digit and not a period)
     */
    function isSymbol(char) {
        return char != "." && !isNumeric(char);
    }

    /**
     * Get all valid adjacent positions for a given cell
     */
    function getAdjacentPositions(row, col, maxRow, maxCol) {
        var positions = [];
        var deltas = [
            [-1, -1], [-1, 0], [-1, 1],
            [0, -1],           [0, 1],
            [1, -1],  [1, 0],  [1, 1]
        ];

        for (var delta in deltas) {
            var newRow = row + delta[1];
            var newCol = col + delta[2];
            if (newRow >= 1 && newRow <= maxRow && newCol >= 1 && newCol <= maxCol) {
                arrayAppend(positions, [newRow, newCol]);
            }
        }

        return positions;
    }

    /**
     * Part 1: Find all numbers adjacent to symbols and sum them
     *
     * @return {numeric} Sum of all part numbers
     */
    function part1() {
        var total = 0;
        var numRows = arrayLen(lines);

        for (var row = 1; row <= numRows; row++) {
            var line = lines[row];
            var numCols = len(line);
            var col = 1;

            while (col <= numCols) {
                var char = mid(line, col, 1);

                // If we find a digit, extract the full number
                if (isNumeric(char)) {
                    var numStart = col;
                    var numStr = "";
                    var positions = [];

                    // Extract the complete number and track its positions
                    while (col <= numCols && isNumeric(mid(line, col, 1))) {
                        numStr &= mid(line, col, 1);
                        arrayAppend(positions, col);
                        col++;
                    }

                    var number = int(numStr);
                    var isPartNumber = false;

                    // Check if any position of this number is adjacent to a symbol
                    for (var pos in positions) {
                        var adjacent = getAdjacentPositions(row, pos, numRows, numCols);
                        for (var adjPos in adjacent) {
                            var adjRow = adjPos[1];
                            var adjCol = adjPos[2];
                            var adjChar = mid(lines[adjRow], adjCol, 1);

                            if (isSymbol(adjChar)) {
                                isPartNumber = true;
                                break;
                            }
                        }
                        if (isPartNumber) break;
                    }

                    if (isPartNumber) {
                        total += number;
                    }
                } else {
                    col++;
                }
            }
        }

        return total;
    }

    /**
     * Part 2: Find all gears (stars with exactly 2 adjacent numbers) and sum their ratios
     *
     * @return {numeric} Sum of all gear ratios
     */
    function part2() {
        var numRows = arrayLen(lines);

        // First, collect all numbers with their positions
        var numbers = [];

        for (var row = 1; row <= numRows; row++) {
            var line = lines[row];
            var numCols = len(line);
            var col = 1;

            while (col <= numCols) {
                var char = mid(line, col, 1);

                if (isNumeric(char)) {
                    var numStr = "";
                    var positions = [];

                    while (col <= numCols && isNumeric(mid(line, col, 1))) {
                        numStr &= mid(line, col, 1);
                        arrayAppend(positions, [row, col]);
                        col++;
                    }

                    arrayAppend(numbers, {
                        value: int(numStr),
                        positions: positions
                    });
                } else {
                    col++;
                }
            }
        }

        // Now find all stars and check which numbers are adjacent to each
        var total = 0;

        for (var row = 1; row <= numRows; row++) {
            var line = lines[row];
            var numCols = len(line);

            for (var col = 1; col <= numCols; col++) {
                var char = mid(line, col, 1);

                if (char == "*") {
                    // Find all numbers adjacent to this star
                    var adjacentNumbers = [];
                    var adjacentPositions = getAdjacentPositions(row, col, numRows, numCols);

                    for (var num in numbers) {
                        var isAdjacent = false;

                        // Check if any position of this number is adjacent to the star
                        for (var numPos in num.positions) {
                            for (var adjPos in adjacentPositions) {
                                if (numPos[1] == adjPos[1] && numPos[2] == adjPos[2]) {
                                    isAdjacent = true;
                                    break;
                                }
                            }
                            if (isAdjacent) break;
                        }

                        if (isAdjacent) {
                            arrayAppend(adjacentNumbers, num.value);
                        }
                    }

                    // If exactly 2 numbers are adjacent, it's a gear
                    if (arrayLen(adjacentNumbers) == 2) {
                        var gearRatio = adjacentNumbers[1] * adjacentNumbers[2];
                        total += gearRatio;
                    }
                }
            }
        }

        return total;
    }

    // Execute both parts and display results
    echo("Part 1: " & part1() & chr(10));
    echo("Part 2: " & part2() & chr(10));

} catch (any e) {
    // Graceful error handling with detailed error information
    echo("Error: " & e.message & chr(10));
    if (structKeyExists(e, "detail")) {
        echo("Detail: " & e.detail & chr(10));
    }
    abort;
}
</cfscript>
