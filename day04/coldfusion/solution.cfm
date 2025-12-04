<cfscript>
// Read input file
baseDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = baseDir & "../input.txt";
inputText = fileRead(inputPath).trim();
lines = listToArray(inputText, chr(10));

// Part 1: Count rolls with fewer than 4 adjacent rolls
function part1() {
    var grid = lines;
    var rows = arrayLen(grid);
    var cols = rows > 0 ? len(grid[1]) : 0;

    // Directions for 8 neighbors (including diagonals)
    var directions = [
        {dr: -1, dc: -1}, {dr: -1, dc: 0}, {dr: -1, dc: 1},
        {dr: 0, dc: -1},                   {dr: 0, dc: 1},
        {dr: 1, dc: -1},  {dr: 1, dc: 0},  {dr: 1, dc: 1}
    ];

    var accessibleCount = 0;

    for (var r = 1; r <= rows; r++) {
        for (var c = 1; c <= cols; c++) {
            if (mid(grid[r], c, 1) == '@') {
                // Count adjacent rolls
                var adjacentRolls = 0;

                for (var i = 1; i <= arrayLen(directions); i++) {
                    var dir = directions[i];
                    var nr = r + dir.dr;
                    var nc = c + dir.dc;

                    // Check bounds
                    if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
                        if (mid(grid[nr], nc, 1) == '@') {
                            adjacentRolls++;
                        }
                    }
                }

                // Accessible if fewer than 4 adjacent rolls
                if (adjacentRolls < 4) {
                    accessibleCount++;
                }
            }
        }
    }

    return accessibleCount;
}

// Part 2: Repeatedly remove accessible rolls until none remain
function part2() {
    var grid = [];

    // Create mutable copy of grid (convert strings to arrays of chars)
    for (var i = 1; i <= arrayLen(lines); i++) {
        var line = lines[i];
        var chars = [];
        for (var j = 1; j <= len(line); j++) {
            arrayAppend(chars, mid(line, j, 1));
        }
        arrayAppend(grid, chars);
    }

    var rows = arrayLen(grid);
    var cols = rows > 0 ? arrayLen(grid[1]) : 0;

    // Directions for 8 neighbors (including diagonals)
    var directions = [
        {dr: -1, dc: -1}, {dr: -1, dc: 0}, {dr: -1, dc: 1},
        {dr: 0, dc: -1},                   {dr: 0, dc: 1},
        {dr: 1, dc: -1},  {dr: 1, dc: 0},  {dr: 1, dc: 1}
    ];

    var totalRemoved = 0;

    while (true) {
        // Find all rolls that can be removed in this iteration
        var removable = [];

        for (var r = 1; r <= rows; r++) {
            for (var c = 1; c <= cols; c++) {
                if (grid[r][c] == '@') {
                    // Count adjacent rolls
                    var adjacentRolls = 0;

                    for (var i = 1; i <= arrayLen(directions); i++) {
                        var dir = directions[i];
                        var nr = r + dir.dr;
                        var nc = c + dir.dc;

                        // Check bounds
                        if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
                            if (grid[nr][nc] == '@') {
                                adjacentRolls++;
                            }
                        }
                    }

                    // Can be removed if fewer than 4 adjacent rolls
                    if (adjacentRolls < 4) {
                        arrayAppend(removable, {r: r, c: c});
                    }
                }
            }
        }

        // If no rolls can be removed, we're done
        if (arrayLen(removable) == 0) {
            break;
        }

        // Remove all accessible rolls
        for (var i = 1; i <= arrayLen(removable); i++) {
            var pos = removable[i];
            grid[pos.r][pos.c] = '.';
        }

        totalRemoved += arrayLen(removable);
    }

    return totalRemoved;
}

// Run both parts
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
