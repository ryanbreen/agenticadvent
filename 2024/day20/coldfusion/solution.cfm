<cfscript>
// Day 20: Race Condition

WALL = chr(35);  // '#'
START = "S";
END = "E";

// Parse grid, find S (start) and E (end)
function parseInput(filename) {
    var inputFile = fileRead(filename);
    var lines = listToArray(inputFile, chr(10));
    var grid = [];
    var startPos = {};
    var endPos = {};

    for (var r = 1; r <= arrayLen(lines); r++) {
        var line = lines[r];
        if (len(trim(line)) == 0) {
            continue;
        }
        var row = listToArray(line, "");
        arrayAppend(grid, row);
        var gridRow = arrayLen(grid);
        for (var c = 1; c <= arrayLen(row); c++) {
            if (row[c] == START) {
                startPos = {r: gridRow, c: c};
            } else if (row[c] == END) {
                endPos = {r: gridRow, c: c};
            }
        }
    }

    return {grid: grid, start: startPos, end: endPos};
}

// BFS to trace the single path from start to end
// Returns distance from start for each track cell
function tracePath(grid, startPos, endPos) {
    var rows = arrayLen(grid);
    var cols = arrayLen(grid[1]);
    var dist = {};
    var startKey = startPos.r & "," & startPos.c;
    dist[startKey] = 0;

    var queue = [];
    arrayAppend(queue, startPos);

    var directions = [
        {dr: -1, dc: 0},
        {dr: 1, dc: 0},
        {dr: 0, dc: -1},
        {dr: 0, dc: 1}
    ];

    while (arrayLen(queue) > 0) {
        var pos = queue[1];
        arrayDeleteAt(queue, 1);
        var r = pos.r;
        var c = pos.c;

        if (r == endPos.r && c == endPos.c) {
            break;
        }

        var currentKey = r & "," & c;
        var currentDist = dist[currentKey];

        for (var dir in directions) {
            var nr = r + dir.dr;
            var nc = c + dir.dc;

            if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
                if (grid[nr][nc] != WALL) {
                    var neighborKey = nr & "," & nc;
                    if (!structKeyExists(dist, neighborKey)) {
                        dist[neighborKey] = currentDist + 1;
                        arrayAppend(queue, {r: nr, c: nc});
                    }
                }
            }
        }
    }

    return dist;
}

// Count cheats that save at least minSavings picoseconds
function countCheats(dist, maxCheatTime, minSavings) {
    var count = 0;
    var trackPositions = structKeyArray(dist);

    for (var key1 in trackPositions) {
        var parts1 = listToArray(key1, ",");
        var r1 = val(parts1[1]);
        var c1 = val(parts1[2]);
        var d1 = dist[key1];

        for (var key2 in trackPositions) {
            var parts2 = listToArray(key2, ",");
            var r2 = val(parts2[1]);
            var c2 = val(parts2[2]);

            // Manhattan distance is the cheat cost
            var cheatCost = abs(r2 - r1) + abs(c2 - c1);

            if (cheatCost <= maxCheatTime) {
                var d2 = dist[key2];
                var savings = d2 - d1 - cheatCost;
                if (savings >= minSavings) {
                    count++;
                }
            }
        }
    }

    return count;
}

// Part 1: max cheat time = 2
function part1(grid, startPos, endPos) {
    var dist = tracePath(grid, startPos, endPos);
    return countCheats(dist, 2, 100);
}

// Part 2: max cheat time = 20
function part2(grid, startPos, endPos) {
    var dist = tracePath(grid, startPos, endPos);
    return countCheats(dist, 20, 100);
}

// Main execution
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
parsed = parseInput(inputPath);

writeOutput("Part 1: " & part1(parsed.grid, parsed.start, parsed.end) & chr(10));
writeOutput("Part 2: " & part2(parsed.grid, parsed.start, parsed.end) & chr(10));
</cfscript>
