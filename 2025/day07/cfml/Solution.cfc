component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();
        var lines = listToArray(inputText, chr(10));

        // Solve both parts
        var part1Result = part1(lines);
        var part2Result = part2(lines);

        print.line("Part 1: " & part1Result);
        print.line("Part 2: " & part2Result);
    }

    function part1(lines) {
        var rows = arrayLen(lines);
        var cols = rows > 0 ? len(lines[1]) : 0;

        // Find starting position S
        var startCol = 0;
        for (var col = 1; col <= cols; col++) {
            if (mid(lines[1], col, 1) == 'S') {
                startCol = col;
                break;
            }
        }

        if (startCol == 0) {
            return 0;
        }

        // Track active beam columns at each row
        // Use a struct to simulate a set (handle beam merging)
        var activeBeams = {};
        activeBeams[startCol] = true;
        var splitCount = 0;

        // Process row by row starting from row 2 (below S)
        for (var row = 2; row <= rows; row++) {
            var newBeams = {};

            for (var col in activeBeams) {
                var colNum = val(col);
                if (colNum >= 1 && colNum <= cols) {
                    var cell = mid(lines[row], colNum, 1);

                    if (cell == '^') {
                        // Beam hits splitter - count it and emit left/right
                        splitCount++;

                        // Left beam goes to col-1, right beam goes to col+1
                        if (colNum - 1 >= 1) {
                            newBeams[colNum - 1] = true;
                        }
                        if (colNum + 1 <= cols) {
                            newBeams[colNum + 1] = true;
                        }
                    } else if (cell == '.') {
                        // Beam continues straight down
                        newBeams[colNum] = true;
                    } else {
                        // Other characters - beam continues
                        newBeams[colNum] = true;
                    }
                }
            }

            activeBeams = newBeams;

            // If no more beams, stop
            if (structIsEmpty(activeBeams)) {
                break;
            }
        }

        return splitCount;
    }

    function part2(lines) {
        var rows = arrayLen(lines);
        var cols = rows > 0 ? len(lines[1]) : 0;

        // Find starting position S
        var startCol = 0;
        for (var col = 1; col <= cols; col++) {
            if (mid(lines[1], col, 1) == 'S') {
                startCol = col;
                break;
            }
        }

        if (startCol == 0) {
            return 0;
        }

        // For Part 2, we need to track the number of timelines at each position
        // This grows exponentially, so we need Java BigInteger for large numbers
        var BigInteger = createObject("java", "java.math.BigInteger");

        // Track number of timelines at each column position
        // Use a struct: col -> count of timelines (as BigInteger)
        var timelines = {};
        timelines[startCol] = BigInteger.init("1");

        // Process row by row starting from row 2 (below S)
        for (var row = 2; row <= rows; row++) {
            var newTimelines = {};

            for (var col in timelines) {
                var colNum = val(col);
                if (colNum >= 1 && colNum <= cols) {
                    var cell = mid(lines[row], colNum, 1);
                    var count = timelines[col];

                    if (cell == '^') {
                        // Each timeline splits into 2 (left and right)
                        if (colNum - 1 >= 1) {
                            var leftKey = colNum - 1;
                            if (structKeyExists(newTimelines, leftKey)) {
                                newTimelines[leftKey] = newTimelines[leftKey].add(count);
                            } else {
                                newTimelines[leftKey] = count;
                            }
                        }
                        if (colNum + 1 <= cols) {
                            var rightKey = colNum + 1;
                            if (structKeyExists(newTimelines, rightKey)) {
                                newTimelines[rightKey] = newTimelines[rightKey].add(count);
                            } else {
                                newTimelines[rightKey] = count;
                            }
                        }
                    } else if (cell == '.') {
                        // Timelines continue straight down
                        if (structKeyExists(newTimelines, colNum)) {
                            newTimelines[colNum] = newTimelines[colNum].add(count);
                        } else {
                            newTimelines[colNum] = count;
                        }
                    } else {
                        // Other characters - timelines continue
                        if (structKeyExists(newTimelines, colNum)) {
                            newTimelines[colNum] = newTimelines[colNum].add(count);
                        } else {
                            newTimelines[colNum] = count;
                        }
                    }
                }
            }

            timelines = newTimelines;

            // If no more timelines, stop
            if (structIsEmpty(timelines)) {
                break;
            }
        }

        // Total number of timelines - sum all BigInteger values
        var total = BigInteger.init("0");
        for (var col in timelines) {
            total = total.add(timelines[col]);
        }

        return total.toString();
    }
}
