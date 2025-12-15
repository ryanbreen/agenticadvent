<cfscript>
// Define constants to avoid hash character issues
WALL = chr(35);  // '#'
BOX = "O";
EMPTY = ".";
ROBOT = "@";
BOX_LEFT = "[";
BOX_RIGHT = "]";

// Cache direction deltas at file scope to avoid recreation
DELTAS = {
    "<": {dr: 0, dc: -1},
    ">": {dr: 0, dc: 1},
    "^": {dr: -1, dc: 0},
    "v": {dr: 1, dc: 0}
};

// Helper function to create numeric key instead of string concatenation
// Uses bit shifting to pack row/col into single number (max 10000x10000 grid)
function makeKey(r, c) {
    return r * 10000 + c;
}

function parseInput(text) {
    // Split on blank line (double newline)
    var blankLinePos = find(chr(10) & chr(10), text);
    var gridText = left(text, blankLinePos - 1);
    var movesText = mid(text, blankLinePos + 2, len(text));

    var gridLines = listToArray(gridText, chr(10));
    var grid = [];

    for (var line in gridLines) {
        if (len(trim(line)) > 0) {
            arrayAppend(grid, listToArray(line, ""));
        }
    }

    var moves = reReplace(movesText, "\s+", "", "all");

    return {grid: grid, moves: moves};
}

function findRobot(grid) {
    var gridHeight = arrayLen(grid);
    for (var r = 1; r <= gridHeight; r++) {
        var rowWidth = arrayLen(grid[r]);
        for (var c = 1; c <= rowWidth; c++) {
            if (grid[r][c] == ROBOT) {
                return {r: r, c: c};
            }
        }
    }
    return {};
}

function moveRobot(grid, robotPos, direction) {
    var delta = DELTAS[direction];
    var r = robotPos.r;
    var c = robotPos.c;
    var nr = r + delta.dr;
    var nc = c + delta.dc;

    // Cache grid dimensions
    var gridHeight = arrayLen(grid);
    var gridWidth = arrayLen(grid[1]);

    // Check bounds
    if (nr < 1 || nr > gridHeight || nc < 1 || nc > gridWidth) {
        return robotPos;
    }

    // Hit a wall
    if (grid[nr][nc] == WALL) {
        return robotPos;
    }

    // Empty space
    if (grid[nr][nc] == EMPTY) {
        grid[r][c] = EMPTY;
        grid[nr][nc] = ROBOT;
        return {r: nr, c: nc};
    }

    // Box in the way
    if (grid[nr][nc] == BOX) {
        var checkR = nr;
        var checkC = nc;

        // Find end of box chain (use cached dimensions)
        while (checkR >= 1 && checkR <= gridHeight &&
               checkC >= 1 && checkC <= gridWidth &&
               grid[checkR][checkC] == BOX) {
            checkR += delta.dr;
            checkC += delta.dc;
        }

        // Check if we can push
        if (checkR < 1 || checkR > gridHeight ||
            checkC < 1 || checkC > gridWidth ||
            grid[checkR][checkC] == WALL) {
            return robotPos;
        }

        // Push boxes
        grid[checkR][checkC] = BOX;
        grid[r][c] = EMPTY;
        grid[nr][nc] = ROBOT;
        return {r: nr, c: nc};
    }

    return robotPos;
}

function calculateGPS(grid, boxChar) {
    var total = 0;
    var gridHeight = arrayLen(grid);
    for (var r = 1; r <= gridHeight; r++) {
        var rowWidth = arrayLen(grid[r]);
        for (var c = 1; c <= rowWidth; c++) {
            if (grid[r][c] == boxChar) {
                total += 100 * (r - 1) + (c - 1);
            }
        }
    }
    return total;
}

function part1(inputText) {
    var parsed = parseInput(inputText);
    var grid = parsed.grid;
    var moves = parsed.moves;
    var robotPos = findRobot(grid);

    for (var i = 1; i <= len(moves); i++) {
        var move = mid(moves, i, 1);
        robotPos = moveRobot(grid, robotPos, move);
    }

    return calculateGPS(grid, BOX);
}

function scaleGrid(grid) {
    var newGrid = [];
    for (var row in grid) {
        var newRow = [];
        for (var cell in row) {
            if (cell == WALL) {
                arrayAppend(newRow, WALL);
                arrayAppend(newRow, WALL);
            } else if (cell == BOX) {
                arrayAppend(newRow, BOX_LEFT);
                arrayAppend(newRow, BOX_RIGHT);
            } else if (cell == EMPTY) {
                arrayAppend(newRow, EMPTY);
                arrayAppend(newRow, EMPTY);
            } else if (cell == ROBOT) {
                arrayAppend(newRow, ROBOT);
                arrayAppend(newRow, EMPTY);
            }
        }
        arrayAppend(newGrid, newRow);
    }
    return newGrid;
}

function canMoveBoxVertical(grid, boxLeftC, r, dr, gridHeight) {
    var nr = r + dr;
    var leftC = boxLeftC;
    var rightC = boxLeftC + 1;

    // Check bounds
    if (nr < 1 || nr > gridHeight) {
        return false;
    }

    var leftTarget = grid[nr][leftC];
    var rightTarget = grid[nr][rightC];

    // Wall check
    if (leftTarget == WALL || rightTarget == WALL) {
        return false;
    }

    // Find boxes in the way - inline logic to avoid creating HashMaps in hot path
    // Use simple array since we have at most 2 boxes to check
    var boxesToCheck = [];
    var seenKeys = {};

    if (leftTarget == BOX_LEFT) {
        var key1 = makeKey(nr, leftC);
        if (!structKeyExists(seenKeys, key1)) {
            arrayAppend(boxesToCheck, {r: nr, c: leftC});
            seenKeys[key1] = true;
        }
    } else if (leftTarget == BOX_RIGHT) {
        var leftCMinus1 = leftC - 1;
        var key2 = makeKey(nr, leftCMinus1);
        if (!structKeyExists(seenKeys, key2)) {
            arrayAppend(boxesToCheck, {r: nr, c: leftCMinus1});
            seenKeys[key2] = true;
        }
    }

    if (rightTarget == BOX_LEFT) {
        var key3 = makeKey(nr, rightC);
        if (!structKeyExists(seenKeys, key3)) {
            arrayAppend(boxesToCheck, {r: nr, c: rightC});
            seenKeys[key3] = true;
        }
    } else if (rightTarget == BOX_RIGHT) {
        var rightCMinus1 = rightC - 1;
        var key4 = makeKey(nr, rightCMinus1);
        if (!structKeyExists(seenKeys, key4)) {
            arrayAppend(boxesToCheck, {r: nr, c: rightCMinus1});
            seenKeys[key4] = true;
        }
    }

    // Recursively check
    for (var box in boxesToCheck) {
        if (!canMoveBoxVertical(grid, box.c, box.r, dr, gridHeight)) {
            return false;
        }
    }

    return true;
}

function collectBoxesVertical(grid, boxLeftC, r, dr, collected) {
    var key = makeKey(r, boxLeftC);
    collected.put(key, {r: r, c: boxLeftC});

    var nr = r + dr;
    var leftC = boxLeftC;
    var rightC = boxLeftC + 1;

    var leftTarget = grid[nr][leftC];
    var rightTarget = grid[nr][rightC];

    // Use simple array since we have at most 2 boxes to check
    var boxesToCheck = [];
    var seenKeys = {};

    if (leftTarget == BOX_LEFT) {
        var key1 = makeKey(nr, leftC);
        if (!structKeyExists(seenKeys, key1)) {
            arrayAppend(boxesToCheck, {key: key1, r: nr, c: leftC});
            seenKeys[key1] = true;
        }
    } else if (leftTarget == BOX_RIGHT) {
        var leftCMinus1 = leftC - 1;
        var key2 = makeKey(nr, leftCMinus1);
        if (!structKeyExists(seenKeys, key2)) {
            arrayAppend(boxesToCheck, {key: key2, r: nr, c: leftCMinus1});
            seenKeys[key2] = true;
        }
    }

    if (rightTarget == BOX_LEFT) {
        var key3 = makeKey(nr, rightC);
        if (!structKeyExists(seenKeys, key3)) {
            arrayAppend(boxesToCheck, {key: key3, r: nr, c: rightC});
            seenKeys[key3] = true;
        }
    } else if (rightTarget == BOX_RIGHT) {
        var rightCMinus1 = rightC - 1;
        var key4 = makeKey(nr, rightCMinus1);
        if (!structKeyExists(seenKeys, key4)) {
            arrayAppend(boxesToCheck, {key: key4, r: nr, c: rightCMinus1});
            seenKeys[key4] = true;
        }
    }

    for (var box in boxesToCheck) {
        if (!collected.containsKey(box.key)) {
            collectBoxesVertical(grid, box.c, box.r, dr, collected);
        }
    }
}

function moveRobotWide(grid, robotPos, direction) {
    var delta = DELTAS[direction];
    var r = robotPos.r;
    var c = robotPos.c;
    var nr = r + delta.dr;
    var nc = c + delta.dc;

    // Cache grid dimensions
    var gridHeight = arrayLen(grid);
    var gridWidth = arrayLen(grid[1]);

    // Check bounds
    if (nr < 1 || nr > gridHeight || nc < 1 || nc > gridWidth) {
        return robotPos;
    }

    var target = grid[nr][nc];

    // Wall
    if (target == WALL) {
        return robotPos;
    }

    // Empty
    if (target == EMPTY) {
        grid[r][c] = EMPTY;
        grid[nr][nc] = ROBOT;
        return {r: nr, c: nc};
    }

    // Box
    if (target == BOX_LEFT || target == BOX_RIGHT) {
        // Horizontal movement
        if (delta.dc != 0) {
            var checkC = nc;

            // Find end of box chain (use cached width)
            while (checkC >= 1 && checkC <= gridWidth &&
                   (grid[r][checkC] == BOX_LEFT || grid[r][checkC] == BOX_RIGHT)) {
                checkC += delta.dc;
            }

            // Check if we can push
            if (checkC < 1 || checkC > gridWidth || grid[r][checkC] == WALL) {
                return robotPos;
            }

            // Shift boxes
            if (delta.dc > 0) {
                // Moving right
                for (var col = checkC; col > nc; col--) {
                    grid[r][col] = grid[r][col - 1];
                }
            } else {
                // Moving left
                for (var col = checkC; col < nc; col++) {
                    grid[r][col] = grid[r][col + 1];
                }
            }

            grid[r][c] = EMPTY;
            grid[nr][nc] = ROBOT;
            return {r: nr, c: nc};

        } else {
            // Vertical movement
            var boxLeftC = (target == BOX_LEFT) ? nc : nc - 1;

            // Check if we can move (pass cached height)
            if (!canMoveBoxVertical(grid, boxLeftC, nr, delta.dr, gridHeight)) {
                return robotPos;
            }

            // Collect all boxes - use Java HashMap
            var boxesToMove = createObject("java", "java.util.HashMap").init();
            collectBoxesVertical(grid, boxLeftC, nr, delta.dr, boxesToMove);

            // Sort boxes by row
            var sortedBoxes = [];
            var keySet = boxesToMove.keySet().toArray();
            for (var i = 1; i <= arrayLen(keySet); i++) {
                var key = keySet[i];
                arrayAppend(sortedBoxes, boxesToMove.get(key));
            }

            // Sort based on direction
            if (delta.dr > 0) {
                // Moving down - process from bottom to top
                arraySort(sortedBoxes, function(a, b) {
                    return b.r - a.r;
                });
            } else {
                // Moving up - process from top to bottom
                arraySort(sortedBoxes, function(a, b) {
                    return a.r - b.r;
                });
            }

            // Move all boxes
            for (var box in sortedBoxes) {
                grid[box.r][box.c] = EMPTY;
                grid[box.r][box.c + 1] = EMPTY;
                grid[box.r + delta.dr][box.c] = BOX_LEFT;
                grid[box.r + delta.dr][box.c + 1] = BOX_RIGHT;
            }

            // Move robot
            grid[r][c] = EMPTY;
            grid[nr][nc] = ROBOT;
            return {r: nr, c: nc};
        }
    }

    return robotPos;
}

function part2(inputText) {
    var parsed = parseInput(inputText);
    var grid = scaleGrid(parsed.grid);
    var moves = parsed.moves;
    var robotPos = findRobot(grid);

    for (var i = 1; i <= len(moves); i++) {
        var move = mid(moves, i, 1);
        robotPos = moveRobotWide(grid, robotPos, move);
    }

    return calculateGPS(grid, BOX_LEFT);
}

// Main execution
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputText = fileRead(inputPath);

writeOutput("Part 1: " & part1(inputText) & chr(10));
writeOutput("Part 2: " & part2(inputText) & chr(10));
</cfscript>
