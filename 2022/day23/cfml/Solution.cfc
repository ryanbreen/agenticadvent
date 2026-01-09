component {

    /**
     * Day 23: Unstable Diffusion
     * Simulates elves spreading out on a grid
     */

    public void function run() {
        var inputPath = getDirectoryFromPath(getCurrentTemplatePath()) & "../input.txt";
        var text = fileRead(inputPath);

        writeOutput("Part 1: " & part1(text) & chr(10));
        writeOutput("Part 2: " & part2(text) & chr(10));
    }

    /**
     * Parse elf positions from grid input.
     * Returns a struct where keys are "row,col" strings representing elf positions.
     */
    private struct function parseInput(required string text) {
        var elves = {};
        var lines = listToArray(trim(arguments.text), chr(10));

        for (var r = 1; r <= arrayLen(lines); r++) {
            var line = lines[r];
            for (var c = 1; c <= len(line); c++) {
                if (mid(line, c, 1) == "##") {
                    elves[r & "," & c] = true;
                }
            }
        }
        return elves;
    }

    /**
     * Create position key from row and column.
     */
    private string function posKey(required numeric row, required numeric col) {
        return arguments.row & "," & arguments.col;
    }

    /**
     * Parse position key back to row and column.
     */
    private struct function parsePos(required string key) {
        var parts = listToArray(arguments.key, ",");
        return {row: parts[1], col: parts[2]};
    }

    /**
     * Run one round of simulation.
     * Returns a struct with 'elves' (new positions) and 'moved' (boolean).
     */
    private struct function simulateRound(required struct elves, required array directions) {
        // Direction checks: positions to check and move delta
        var dirChecks = {
            "N": {checks: [[-1, -1], [-1, 0], [-1, 1]], delta: [-1, 0]},
            "S": {checks: [[1, -1], [1, 0], [1, 1]], delta: [1, 0]},
            "W": {checks: [[-1, -1], [0, -1], [1, -1]], delta: [0, -1]},
            "E": {checks: [[-1, 1], [0, 1], [1, 1]], delta: [0, 1]}
        };

        // All 8 neighbors
        var allNeighbors = [
            [-1, -1], [-1, 0], [-1, 1],
            [0, -1],           [0, 1],
            [1, -1],  [1, 0],  [1, 1]
        ];

        // Phase 1: Each elf proposes a move
        var proposals = {};      // elfKey -> proposedKey
        var proposalCounts = {}; // proposedKey -> count

        for (var elfKey in arguments.elves) {
            var pos = parsePos(elfKey);
            var r = pos.row;
            var c = pos.col;

            // Check if any neighbors exist
            var hasNeighbor = false;
            for (var n = 1; n <= arrayLen(allNeighbors); n++) {
                var neighbor = allNeighbors[n];
                var neighborKey = posKey(r + neighbor[1], c + neighbor[2]);
                if (structKeyExists(arguments.elves, neighborKey)) {
                    hasNeighbor = true;
                    break;
                }
            }

            if (!hasNeighbor) {
                continue; // Don't move
            }

            // Try each direction in order
            for (var d = 1; d <= arrayLen(arguments.directions); d++) {
                var dir = arguments.directions[d];
                var dirInfo = dirChecks[dir];
                var canMove = true;

                // Check if all positions in this direction are empty
                for (var chk = 1; chk <= arrayLen(dirInfo.checks); chk++) {
                    var checkOffset = dirInfo.checks[chk];
                    var checkKey = posKey(r + checkOffset[1], c + checkOffset[2]);
                    if (structKeyExists(arguments.elves, checkKey)) {
                        canMove = false;
                        break;
                    }
                }

                if (canMove) {
                    var newPos = posKey(r + dirInfo.delta[1], c + dirInfo.delta[2]);
                    proposals[elfKey] = newPos;
                    if (!structKeyExists(proposalCounts, newPos)) {
                        proposalCounts[newPos] = 0;
                    }
                    proposalCounts[newPos]++;
                    break;
                }
            }
        }

        // Phase 2: Execute moves (only if unique proposal)
        var newElves = {};
        var moved = false;

        for (var elfKey in arguments.elves) {
            if (structKeyExists(proposals, elfKey)) {
                var newPos = proposals[elfKey];
                if (proposalCounts[newPos] == 1) {
                    newElves[newPos] = true;
                    moved = true;
                } else {
                    newElves[elfKey] = true;
                }
            } else {
                newElves[elfKey] = true;
            }
        }

        return {elves: newElves, moved: moved};
    }

    /**
     * Count empty tiles in bounding rectangle.
     */
    private numeric function boundingRectEmpty(required struct elves) {
        var minR = javaCast("int", 2147483647);
        var maxR = javaCast("int", -2147483648);
        var minC = javaCast("int", 2147483647);
        var maxC = javaCast("int", -2147483648);

        for (var elfKey in arguments.elves) {
            var pos = parsePos(elfKey);
            var r = pos.row;
            var c = pos.col;
            if (r < minR) minR = r;
            if (r > maxR) maxR = r;
            if (c < minC) minC = c;
            if (c > maxC) maxC = c;
        }

        var area = (maxR - minR + 1) * (maxC - minC + 1);
        return area - structCount(arguments.elves);
    }

    /**
     * Part 1: Count empty tiles after 10 rounds.
     */
    public numeric function part1(required string text) {
        var elves = parseInput(arguments.text);
        var directions = ["N", "S", "W", "E"];

        for (var i = 1; i <= 10; i++) {
            var result = simulateRound(elves, directions);
            elves = result.elves;
            // Rotate directions: move first to end
            arrayAppend(directions, directions[1]);
            arrayDeleteAt(directions, 1);
        }

        return boundingRectEmpty(elves);
    }

    /**
     * Part 2: Find first round where no elf moves.
     */
    public numeric function part2(required string text) {
        var elves = parseInput(arguments.text);
        var directions = ["N", "S", "W", "E"];

        var roundNum = 0;
        while (true) {
            roundNum++;
            var result = simulateRound(elves, directions);
            elves = result.elves;

            if (!result.moved) {
                return roundNum;
            }

            // Rotate directions: move first to end
            arrayAppend(directions, directions[1]);
            arrayDeleteAt(directions, 1);
        }
    }

}
