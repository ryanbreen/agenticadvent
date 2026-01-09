component {

    /**
     * Day 22: Monkey Map - Advent of Code 2022
     * Navigate a 2D map with flat wrapping (Part 1) and cube wrapping (Part 2)
     */

    function run() {
        var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
        var inputFile = scriptDir & "../input.txt";
        var text = fileRead(inputFile);

        var parsed = parseInput(text);
        var grid = parsed.grid;
        var instructions = parsed.instructions;

        writeOutput("Part 1: " & part1(grid, instructions) & chr(10));
        writeOutput("Part 2: " & part2(grid, instructions) & chr(10));
    }

    /**
     * Parse the map and path instructions
     */
    function parseInput(text) {
        var parts = listToArray(text, chr(10) & chr(10), false, true);
        var gridText = parts[1];
        var pathText = trim(parts[2]);

        var gridLines = listToArray(gridText, chr(10), true);

        // Find dimensions
        var height = arrayLen(gridLines);
        var width = 0;
        for (var line in gridLines) {
            if (len(line) > width) {
                width = len(line);
            }
        }

        // Create grid (pad lines to consistent width)
        var grid = [];
        for (var line in gridLines) {
            // Right-pad with spaces
            var padded = line;
            while (len(padded) < width) {
                padded = padded & " ";
            }
            arrayAppend(grid, padded);
        }

        // Parse path into moves and turns
        var instructions = [];
        var i = 1;
        var pathLen = len(pathText);

        while (i <= pathLen) {
            var ch = mid(pathText, i, 1);
            if (isNumeric(ch)) {
                var j = i;
                while (j <= pathLen && isNumeric(mid(pathText, j, 1))) {
                    j++;
                }
                arrayAppend(instructions, {type: "move", value: int(mid(pathText, i, j - i))});
                i = j;
            } else {
                arrayAppend(instructions, {type: "turn", value: ch});
                i++;
            }
        }

        return {grid: grid, instructions: instructions};
    }

    /**
     * Get direction delta for row
     */
    function getDR(facing) {
        if (facing == 1) return 0;
        if (facing == 2) return 1;
        if (facing == 3) return 0;
        return -1;
    }

    /**
     * Get direction delta for column
     */
    function getDC(facing) {
        if (facing == 1) return 1;
        if (facing == 2) return 0;
        if (facing == 3) return -1;
        return 0;
    }

    /**
     * Part 1: Navigate the map with 2D flat wrapping
     */
    function part1(grid, instructions) {
        var height = arrayLen(grid);
        var width = len(grid[1]);

        // Find starting position (leftmost open tile on top row)
        var row = 1;
        var col = find(".", grid[1]);
        var facing = 1; // Start facing right (1-indexed: 1=right)

        for (var instr in instructions) {
            if (instr.type == "move") {
                var steps = instr.value;
                for (var step = 1; step <= steps; step++) {
                    var dr = getDR(facing);
                    var dc = getDC(facing);
                    var nr = row + dr;
                    var nc = col + dc;

                    // Wrap around if needed
                    if (facing == 1) { // Right
                        if (nc > width || mid(grid[nr], nc, 1) == " ") {
                            nc = 1;
                            while (mid(grid[nr], nc, 1) == " ") {
                                nc++;
                            }
                        }
                    } else if (facing == 3) { // Left
                        if (nc < 1 || mid(grid[nr], nc, 1) == " ") {
                            nc = width;
                            while (mid(grid[nr], nc, 1) == " ") {
                                nc--;
                            }
                        }
                    } else if (facing == 2) { // Down
                        if (nr > height || mid(grid[nr], nc, 1) == " ") {
                            nr = 1;
                            while (mid(grid[nr], nc, 1) == " ") {
                                nr++;
                            }
                        }
                    } else if (facing == 4) { // Up
                        if (nr < 1 || mid(grid[nr], nc, 1) == " ") {
                            nr = height;
                            while (mid(grid[nr], nc, 1) == " ") {
                                nr--;
                            }
                        }
                    }

                    // Check if we hit a wall
                    if (mid(grid[nr], nc, 1) == chr(35)) {
                        break;
                    }

                    // Move to new position
                    row = nr;
                    col = nc;
                }
            } else {
                // Turn
                if (instr.value == "R") {
                    facing = (facing % 4) + 1;
                } else {
                    facing = ((facing - 2 + 4) % 4) + 1;
                }
            }
        }

        // Calculate password: 1000*row + 4*col + facing
        // facing needs to be 0-indexed: 0=right, 1=down, 2=left, 3=up
        return 1000 * row + 4 * col + (facing - 1);
    }

    /**
     * Determine which face and local coordinates based on the specific cube layout
     */
    function getCubeFaceAndLocal(row, col, faceSize) {
        // For the actual input, the cube layout is:
        //   12
        //   3
        //  45
        //  6
        // With face_size = 50, using 1-indexed coordinates

        var faceRow = int((row - 1) / faceSize);
        var faceCol = int((col - 1) / faceSize);
        var localR = ((row - 1) mod faceSize);
        var localC = ((col - 1) mod faceSize);

        var face = -1;
        if (faceRow == 0 && faceCol == 1) {
            face = 1;
        } else if (faceRow == 0 && faceCol == 2) {
            face = 2;
        } else if (faceRow == 1 && faceCol == 1) {
            face = 3;
        } else if (faceRow == 2 && faceCol == 0) {
            face = 4;
        } else if (faceRow == 2 && faceCol == 1) {
            face = 5;
        } else if (faceRow == 3 && faceCol == 0) {
            face = 6;
        }

        return {face: face, localR: localR, localC: localC};
    }

    /**
     * Handle cube wrapping for the actual input layout
     * facing uses 1-indexed: 1=right, 2=down, 3=left, 4=up
     */
    function wrapCube(row, col, facing, faceSize) {
        var S = faceSize;
        var faceInfo = getCubeFaceAndLocal(row, col, S);
        var face = faceInfo.face;
        var lr = faceInfo.localR;
        var lc = faceInfo.localC;

        var newRow = row;
        var newCol = col;
        var newFacing = facing;

        // Based on the direction and which edge we're leaving
        // facing: 1=right, 2=down, 3=left, 4=up

        if (face == 1) {
            if (facing == 4) { // Up: goes to face 6, from left, facing right
                newRow = 3*S + lc + 1;
                newCol = 1;
                newFacing = 1;
            } else if (facing == 3) { // Left: goes to face 4, from left, facing right (inverted)
                newRow = 3*S - lr;
                newCol = 1;
                newFacing = 1;
            }
        } else if (face == 2) {
            if (facing == 1) { // Right: goes to face 5, from right, facing left (inverted)
                newRow = 3*S - lr;
                newCol = 2*S;
                newFacing = 3;
            } else if (facing == 2) { // Down: goes to face 3, from right, facing left
                newRow = S + lc + 1;
                newCol = 2*S;
                newFacing = 3;
            } else if (facing == 4) { // Up: goes to face 6, from bottom, facing up
                newRow = 4*S;
                newCol = lc + 1;
                newFacing = 4;
            }
        } else if (face == 3) {
            if (facing == 1) { // Right: goes to face 2, from bottom, facing up
                newRow = S;
                newCol = 2*S + lr + 1;
                newFacing = 4;
            } else if (facing == 3) { // Left: goes to face 4, from top, facing down
                newRow = 2*S + 1;
                newCol = lr + 1;
                newFacing = 2;
            }
        } else if (face == 4) {
            if (facing == 4) { // Up: goes to face 3, from left, facing right
                newRow = S + lc + 1;
                newCol = S + 1;
                newFacing = 1;
            } else if (facing == 3) { // Left: goes to face 1, from left, facing right (inverted)
                newRow = S - lr;
                newCol = S + 1;
                newFacing = 1;
            }
        } else if (face == 5) {
            if (facing == 1) { // Right: goes to face 2, from right, facing left (inverted)
                newRow = S - lr;
                newCol = 3*S;
                newFacing = 3;
            } else if (facing == 2) { // Down: goes to face 6, from right, facing left
                newRow = 3*S + lc + 1;
                newCol = S;
                newFacing = 3;
            }
        } else if (face == 6) {
            if (facing == 1) { // Right: goes to face 5, from bottom, facing up
                newRow = 3*S;
                newCol = S + lr + 1;
                newFacing = 4;
            } else if (facing == 2) { // Down: goes to face 2, from top, facing down
                newRow = 1;
                newCol = 2*S + lc + 1;
                newFacing = 2;
            } else if (facing == 3) { // Left: goes to face 1, from top, facing down
                newRow = 1;
                newCol = S + lr + 1;
                newFacing = 2;
            }
        }

        return {row: newRow, col: newCol, facing: newFacing};
    }

    /**
     * Part 2: Navigate the map with cube wrapping
     */
    function part2(grid, instructions) {
        var height = arrayLen(grid);
        var width = len(grid[1]);

        // Determine face size
        var faceSize = (height > 50) ? 50 : 4;

        // Find starting position
        var row = 1;
        var col = find(".", grid[1]);
        var facing = 1; // Start facing right

        for (var instr in instructions) {
            if (instr.type == "move") {
                var steps = instr.value;
                for (var step = 1; step <= steps; step++) {
                    var dr = getDR(facing);
                    var dc = getDC(facing);
                    var nr = row + dr;
                    var nc = col + dc;
                    var nf = facing;

                    // Check if we need to wrap (off grid or hitting a space)
                    var needWrap = false;
                    if (nr < 1 || nr > height || nc < 1 || nc > width) {
                        needWrap = true;
                    } else if (mid(grid[nr], nc, 1) == " ") {
                        needWrap = true;
                    }

                    if (needWrap) {
                        var wrapped = wrapCube(row, col, facing, faceSize);
                        nr = wrapped.row;
                        nc = wrapped.col;
                        nf = wrapped.facing;
                    }

                    // Check for wall
                    if (mid(grid[nr], nc, 1) == chr(35)) {
                        break;
                    }

                    row = nr;
                    col = nc;
                    facing = nf;
                }
            } else {
                if (instr.value == "R") {
                    facing = (facing % 4) + 1;
                } else {
                    facing = ((facing - 2 + 4) % 4) + 1;
                }
            }
        }

        // Return password with 0-indexed facing
        return 1000 * row + 4 * col + (facing - 1);
    }
}
