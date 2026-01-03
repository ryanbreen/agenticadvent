<cfscript>
/**
 * Day 23: A Long Walk
 * Longest path through hiking trails.
 * Part 1: Respect slope directions (^, v, <, >)
 * Part 2: Ignore slopes (treat as regular paths)
 */

// Read input file
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputText = fileRead(inputPath);
gridLines = listToArray(inputText, chr(10));

// Clean up any trailing empty lines
while (arrayLen(gridLines) > 0 && len(trim(gridLines[arrayLen(gridLines)])) == 0) {
    arrayDeleteAt(gridLines, arrayLen(gridLines));
}

/**
 * Parse the grid into a 2D array for easier access.
 * Returns array of arrays (1-indexed).
 */
function parseGrid(lines) {
    var grid = [];
    for (var line in lines) {
        line = trim(line);
        if (len(line) == 0) continue;
        // Convert string to array of characters
        var row = [];
        for (var i = 1; i <= len(line); i++) {
            arrayAppend(row, mid(line, i, 1));
        }
        arrayAppend(grid, row);
    }
    return grid;
}

/**
 * Make a coordinate key for struct lookups.
 */
function makeKey(r, c) {
    return r & "," & c;
}

/**
 * Parse a coordinate key back to row,col.
 */
function parseKey(key) {
    var parts = listToArray(key, ",");
    return { "r": int(parts[1]), "c": int(parts[2]) };
}

/**
 * Find all junction points (start, end, and intersections).
 * Junctions are cells with 3+ walkable neighbors.
 */
function findJunctions(grid) {
    var rows = arrayLen(grid);
    var cols = arrayLen(grid[1]);
    var junctions = {};
    var WALL = chr(35);  // '#' character

    // Find start point (first row, find the '.')
    var startCol = 0;
    for (var c = 1; c <= cols; c++) {
        if (grid[1][c] == ".") {
            startCol = c;
            break;
        }
    }
    var startKey = makeKey(1, startCol);
    junctions[startKey] = true;

    // Find end point (last row, find the '.')
    var endCol = 0;
    for (var c = 1; c <= cols; c++) {
        if (grid[rows][c] == ".") {
            endCol = c;
            break;
        }
    }
    var endKey = makeKey(rows, endCol);
    junctions[endKey] = true;

    // Directions: up, down, left, right
    var dirs = [
        { "dr": -1, "dc": 0 },
        { "dr": 1, "dc": 0 },
        { "dr": 0, "dc": -1 },
        { "dr": 0, "dc": 1 }
    ];

    // Find intersections (cells with 3+ walkable neighbors)
    for (var r = 1; r <= rows; r++) {
        for (var c = 1; c <= cols; c++) {
            if (grid[r][c] == WALL) continue;

            var neighbors = 0;
            for (var d in dirs) {
                var nr = r + d.dr;
                var nc = c + d.dc;
                if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
                    if (grid[nr][nc] != WALL) {
                        neighbors++;
                    }
                }
            }

            if (neighbors >= 3) {
                junctions[makeKey(r, c)] = true;
            }
        }
    }

    return junctions;
}

/**
 * Build a compressed graph of junctions with edge weights (distances).
 * For each junction, BFS to find reachable junctions and their distances.
 */
function buildGraph(grid, junctions, respectSlopes) {
    var rows = arrayLen(grid);
    var cols = arrayLen(grid[1]);
    var WALL = chr(35);  // '#' character

    // Slope direction mappings
    var slopeDirs = {
        "^": { "dr": -1, "dc": 0 },
        "v": { "dr": 1, "dc": 0 },
        "<": { "dr": 0, "dc": -1 },
        ">": { "dr": 0, "dc": 1 }
    };

    // Directions: up, down, left, right
    var dirs = [
        { "dr": -1, "dc": 0 },
        { "dr": 1, "dc": 0 },
        { "dr": 0, "dc": -1 },
        { "dr": 0, "dc": 1 }
    ];

    // Graph: graph[junctionKey] = struct of { neighborKey: distance }
    var graph = {};

    // Initialize graph for all junctions
    for (var junctionKey in junctions) {
        graph[junctionKey] = {};
    }

    // BFS from each junction to find reachable junctions
    for (var startKey in junctions) {
        var start = parseKey(startKey);

        // Stack for DFS: each element is { "r", "c", "dist" }
        var stack = [];
        arrayAppend(stack, { "r": start.r, "c": start.c, "dist": 0 });

        // Visited set
        var visited = {};
        visited[startKey] = true;

        while (arrayLen(stack) > 0) {
            var current = stack[arrayLen(stack)];
            arrayDeleteAt(stack, arrayLen(stack));

            var r = current.r;
            var c = current.c;
            var dist = current.dist;
            var currentKey = makeKey(r, c);

            // If we reached another junction (not the start), record the edge
            if (dist > 0 && structKeyExists(junctions, currentKey)) {
                graph[startKey][currentKey] = dist;
                continue;
            }

            // Explore neighbors
            for (var d in dirs) {
                var nr = r + d.dr;
                var nc = c + d.dc;

                // Bounds check
                if (nr < 1 || nr > rows || nc < 1 || nc > cols) {
                    continue;
                }

                // Wall check
                if (grid[nr][nc] == WALL) {
                    continue;
                }

                var neighborKey = makeKey(nr, nc);

                // Already visited check
                if (structKeyExists(visited, neighborKey)) {
                    continue;
                }

                // Check slope constraints for Part 1
                if (respectSlopes) {
                    var cell = grid[r][c];
                    if (structKeyExists(slopeDirs, cell)) {
                        var reqDir = slopeDirs[cell];
                        if (d.dr != reqDir.dr || d.dc != reqDir.dc) {
                            continue;
                        }
                    }
                }

                visited[neighborKey] = true;
                arrayAppend(stack, { "r": nr, "c": nc, "dist": dist + 1 });
            }
        }
    }

    return graph;
}

/**
 * Find longest path using DFS with backtracking.
 * This is the core algorithm - brute force search through all paths.
 */
function longestPathDFS(graph, startKey, endKey) {
    // Use iterative DFS with explicit stack to avoid stack overflow
    // Each stack element: { "node", "neighborIdx", "dist", "visitedSnapshot" }

    // But for simplicity, let's use recursive DFS with visited set
    var visited = {};

    var result = dfsRecursive(graph, startKey, endKey, visited);
    return result;
}

function dfsRecursive(graph, nodeKey, endKey, visited) {
    if (nodeKey == endKey) {
        return 0;
    }

    visited[nodeKey] = true;
    var maxDist = -999999999;  // Use large negative instead of -inf

    var neighbors = graph[nodeKey];
    for (var neighborKey in neighbors) {
        if (!structKeyExists(visited, neighborKey)) {
            var edgeDist = neighbors[neighborKey];
            var result = dfsRecursive(graph, neighborKey, endKey, visited);
            if (result != -999999999) {
                var totalDist = edgeDist + result;
                if (totalDist > maxDist) {
                    maxDist = totalDist;
                }
            }
        }
    }

    structDelete(visited, nodeKey);
    return maxDist;
}

/**
 * Solve for either part.
 */
function solve(grid, respectSlopes) {
    var rows = arrayLen(grid);
    var cols = arrayLen(grid[1]);

    // Find start and end
    var startCol = 0;
    for (var c = 1; c <= cols; c++) {
        if (grid[1][c] == ".") {
            startCol = c;
            break;
        }
    }
    var startKey = makeKey(1, startCol);

    var endCol = 0;
    for (var c = 1; c <= cols; c++) {
        if (grid[rows][c] == ".") {
            endCol = c;
            break;
        }
    }
    var endKey = makeKey(rows, endCol);

    var junctions = findJunctions(grid);
    var graph = buildGraph(grid, junctions, respectSlopes);

    return longestPathDFS(graph, startKey, endKey);
}

/**
 * Part 1: Respect slope directions.
 */
function part1(grid) {
    return solve(grid, true);
}

/**
 * Part 2: Ignore slopes (treat as regular paths).
 */
function part2(grid) {
    return solve(grid, false);
}

// Main execution
grid = parseGrid(gridLines);
writeOutput("Part 1: " & part1(grid) & chr(10));
writeOutput("Part 2: " & part2(grid) & chr(10));
</cfscript>
