<cfscript>
/**
 * Advent of Code 2023 - Day 11: Cosmic Expansion
 * ColdFusion/CFScript Solution
 */

// Read input file - get directory of this script
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
fileContent = fileRead(inputPath);
lines = listToArray(fileContent, chr(10));

// Remove trailing empty lines and carriage returns
while (arrayLen(lines) > 0 && trim(lines[arrayLen(lines)]) == "") {
    arrayDeleteAt(lines, arrayLen(lines));
}
// Clean up any carriage returns
for (i = 1; i <= arrayLen(lines); i++) {
    lines[i] = replace(lines[i], chr(13), "", "all");
}

/**
 * Parse the grid and find all galaxy positions
 */
function parseGrid(lines) {
    var galaxies = [];
    for (var r = 1; r <= arrayLen(lines); r++) {
        var line = lines[r];
        for (var c = 1; c <= len(line); c++) {
            if (mid(line, c, 1) == "##") {
                arrayAppend(galaxies, {row: r, col: c});
            }
        }
    }
    return galaxies;
}

/**
 * Find rows and columns that contain no galaxies
 */
function findEmptyRowsAndCols(lines) {
    var rows = arrayLen(lines);
    var cols = rows > 0 ? len(lines[1]) : 0;

    var emptyRows = {};
    var emptyCols = {};

    // Find empty rows (no ## in the line)
    for (var r = 1; r <= rows; r++) {
        if (find("##", lines[r]) == 0) {
            emptyRows[r] = true;
        }
    }

    // Find empty columns
    for (var c = 1; c <= cols; c++) {
        var hasGalaxy = false;
        for (var r = 1; r <= rows; r++) {
            if (mid(lines[r], c, 1) == "##") {
                hasGalaxy = true;
                break;
            }
        }
        if (!hasGalaxy) {
            emptyCols[c] = true;
        }
    }

    return {rows: emptyRows, cols: emptyCols};
}

/**
 * Calculate sum of Manhattan distances between all pairs of galaxies
 * Empty rows/columns are expanded by the expansion factor
 */
function calculateDistances(galaxies, emptyRows, emptyCols, expansionFactor) {
    var total = 0;
    var numGalaxies = arrayLen(galaxies);

    // Iterate through all unique pairs
    for (var i = 1; i <= numGalaxies - 1; i++) {
        for (var j = i + 1; j <= numGalaxies; j++) {
            var g1 = galaxies[i];
            var g2 = galaxies[j];

            var r1 = g1.row;
            var c1 = g1.col;
            var r2 = g2.row;
            var c2 = g2.col;

            // Calculate row distance with expansion
            var minR = min(r1, r2);
            var maxR = max(r1, r2);
            var rowDist = maxR - minR;
            for (var r = minR; r < maxR; r++) {
                if (structKeyExists(emptyRows, r)) {
                    rowDist += expansionFactor - 1;
                }
            }

            // Calculate column distance with expansion
            var minC = min(c1, c2);
            var maxC = max(c1, c2);
            var colDist = maxC - minC;
            for (var c = minC; c < maxC; c++) {
                if (structKeyExists(emptyCols, c)) {
                    colDist += expansionFactor - 1;
                }
            }

            total += rowDist + colDist;
        }
    }

    return total;
}

/**
 * Part 1: Expansion factor of 2
 */
function part1(lines) {
    var galaxies = parseGrid(lines);
    var empty = findEmptyRowsAndCols(lines);
    return calculateDistances(galaxies, empty.rows, empty.cols, 2);
}

/**
 * Part 2: Expansion factor of 1,000,000
 */
function part2(lines) {
    var galaxies = parseGrid(lines);
    var empty = findEmptyRowsAndCols(lines);
    return calculateDistances(galaxies, empty.rows, empty.cols, 1000000);
}

// Run the solution
writeOutput("Part 1: " & part1(lines) & chr(10));
writeOutput("Part 2: " & part2(lines) & chr(10));
</cfscript>
