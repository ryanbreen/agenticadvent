component {

    variables.WIDTH = 7;

    // Rock shapes as arrays of [dx, dy] offsets from bottom-left
    variables.ROCKS = [
        [[0, 0], [1, 0], [2, 0], [3, 0]],           // Horizontal line
        [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]],   // Plus
        [[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]],   // L shape
        [[0, 0], [0, 1], [0, 2], [0, 3]],           // Vertical line
        [[0, 0], [1, 0], [0, 1], [1, 1]]            // Square
    ];

    function run() {
        var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
        var inputFile = scriptDir & "../input.txt";
        var jets = trim(fileRead(inputFile));

        systemOutput("Part 1: " & simulate(jets, 2022), true);
        systemOutput("Part 2: " & simulate(jets, 1000000000000), true);
    }

    function simulate(required string jets, required numeric numRocks) {
        var occupied = {};
        var height = 0;
        var jetIdx = 1;
        var jetsLen = len(jets);

        // For cycle detection
        var states = {};
        var heights = [];

        for (var rockNum = 0; rockNum < numRocks; rockNum = rockNum + 1) {
            var rockType = (rockNum mod 5) + 1;
            var rock = variables.ROCKS[rockType];

            // Starting position: left edge at x=2, bottom at y=height+3
            var x = 2;
            var y = height + 3;

            while (true) {
                // Jet push
                var jet = mid(jets, jetIdx, 1);
                jetIdx = (jetIdx mod jetsLen) + 1;

                var dx = 0;
                if (jet == ">") {
                    dx = 1;
                } else {
                    dx = -1;
                }

                // Check if can move horizontally
                var canMove = true;
                for (var i = 1; i <= arrayLen(rock); i = i + 1) {
                    var rx = rock[i][1];
                    var ry = rock[i][2];
                    var nx = x + rx + dx;
                    var ny = y + ry;
                    if (nx < 0 || nx >= variables.WIDTH || structKeyExists(occupied, nx & "," & ny)) {
                        canMove = false;
                        break;
                    }
                }

                if (canMove) {
                    x = x + dx;
                }

                // Fall down
                var canFall = true;
                for (var i = 1; i <= arrayLen(rock); i = i + 1) {
                    var rx = rock[i][1];
                    var ry = rock[i][2];
                    var nx = x + rx;
                    var ny = y + ry - 1;
                    if (ny < 0 || structKeyExists(occupied, nx & "," & ny)) {
                        canFall = false;
                        break;
                    }
                }

                if (canFall) {
                    y = y - 1;
                } else {
                    // Rock stops
                    for (var i = 1; i <= arrayLen(rock); i = i + 1) {
                        var rx = rock[i][1];
                        var ry = rock[i][2];
                        occupied[(x + rx) & "," & (y + ry)] = true;
                        if (y + ry + 1 > height) {
                            height = y + ry + 1;
                        }
                    }
                    break;
                }
            }

            arrayAppend(heights, height);

            // Cycle detection for Part 2
            if (numRocks > 10000) {
                // Create state key from surface profile
                var profileDepth = 30;
                var profile = [];
                for (var col = 0; col < variables.WIDTH; col = col + 1) {
                    var found = false;
                    for (var row = 0; row < profileDepth; row = row + 1) {
                        if (structKeyExists(occupied, col & "," & (height - 1 - row))) {
                            arrayAppend(profile, col & ":" & row);
                            found = true;
                            break;
                        }
                    }
                    if (not found) {
                        arrayAppend(profile, col & ":" & profileDepth);
                    }
                }

                var stateKey = rockType & "|" & jetIdx & "|" & arrayToList(profile, ";");

                if (structKeyExists(states, stateKey)) {
                    // Found cycle
                    var cycleStart = states[stateKey];
                    var cycleLen = rockNum - cycleStart;
                    var cycleHeight = height - heights[cycleStart + 1];

                    // Calculate final height using precisionEvaluate for large numbers
                    var remaining = precisionEvaluate(numRocks - rockNum - 1);
                    var fullCycles = int(precisionEvaluate(remaining / cycleLen));
                    var leftover = precisionEvaluate(remaining mod cycleLen);

                    var finalHeight = precisionEvaluate(height + fullCycles * cycleHeight);
                    if (leftover > 0) {
                        finalHeight = precisionEvaluate(finalHeight + heights[cycleStart + 1 + leftover] - heights[cycleStart + 1]);
                    }

                    return finalHeight;
                }

                states[stateKey] = rockNum;
            }
        }

        return height;
    }
}
