<cfscript>
// Read and parse input
currentDir = getDirectoryFromPath(getCurrentTemplatePath());
inputFile = currentDir & "../input.txt";
inputText = trim(fileRead(inputFile));
lines = listToArray(inputText, chr(10));

// Parse input into grid
grid = [];
for (line in lines) {
    row = [];
    for (i = 1; i <= len(line); i++) {
        arrayAppend(row, val(mid(line, i, 1)));
    }
    arrayAppend(grid, row);
}

rows = arrayLen(grid);
cols = arrayLen(grid[1]);

// Directions: up, down, left, right
DIRS = [
    {dr: -1, dc: 0},
    {dr: 1, dc: 0},
    {dr: 0, dc: -1},
    {dr: 0, dc: 1}
];


/**
 * Find all positions with height 0 (trailheads)
 */
function findTrailheads() {
    var trailheads = [];
    for (var r = 1; r <= rows; r++) {
        for (var c = 1; c <= cols; c++) {
            if (grid[r][c] == 0) {
                arrayAppend(trailheads, {r: r, c: c});
            }
        }
    }
    return trailheads;
}


/**
 * BFS to find all 9s reachable from a trailhead
 */
function countReachableNines(startR, startC) {
    var visited = {};
    var queue = [];
    var nines = {};
    var key = startR & "_" & startC;

    visited[key] = true;
    arrayAppend(queue, {r: startR, c: startC});

    while (arrayLen(queue) > 0) {
        var pos = queue[1];
        arrayDeleteAt(queue, 1);
        var r = pos.r;
        var c = pos.c;
        var currentHeight = grid[r][c];

        if (currentHeight == 9) {
            nines[r & "_" & c] = true;
            continue;
        }

        // Try all four directions
        for (var dir in DIRS) {
            var nr = r + dir.dr;
            var nc = c + dir.dc;

            if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
                var nkey = nr & "_" & nc;
                if (!structKeyExists(visited, nkey)) {
                    if (grid[nr][nc] == currentHeight + 1) {
                        visited[nkey] = true;
                        arrayAppend(queue, {r: nr, c: nc});
                    }
                }
            }
        }
    }

    return structCount(nines);
}


/**
 * Part 1: Sum of scores of all trailheads
 */
function part1() {
    var trailheads = findTrailheads();
    var totalScore = 0;

    for (var th in trailheads) {
        totalScore += countReachableNines(th.r, th.c);
    }

    return totalScore;
}


/**
 * DFS to count all distinct trails from a trailhead to any 9
 */
function countDistinctTrails(startR, startC) {
    function dfs(r, c) {
        var currentHeight = grid[r][c];

        if (currentHeight == 9) {
            return 1;
        }

        var total = 0;
        for (var dir in DIRS) {
            var nr = r + dir.dr;
            var nc = c + dir.dc;

            if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
                if (grid[nr][nc] == currentHeight + 1) {
                    total += dfs(nr, nc);
                }
            }
        }

        return total;
    }

    return dfs(startR, startC);
}


/**
 * Part 2: Sum of ratings of all trailheads
 */
function part2() {
    var trailheads = findTrailheads();
    var totalRating = 0;

    for (var th in trailheads) {
        totalRating += countDistinctTrails(th.r, th.c);
    }

    return totalRating;
}


// Main execution
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
