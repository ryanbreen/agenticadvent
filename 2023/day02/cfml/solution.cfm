<cfscript>
// Read input file
currentDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = currentDir & "../input.txt";
inputText = fileRead(inputPath).trim();
lines = listToArray(inputText, chr(10));

// Part 1: Check which games are possible with 12 red, 13 green, 14 blue
function part1() {
    var maxRed = 12;
    var maxGreen = 13;
    var maxBlue = 14;
    var sumOfIds = 0;

    for (var line in lines) {
        // Parse game ID
        var parts = listToArray(line, ":");
        var gameIdStr = listGetAt(parts[1], 2, " ");
        var gameId = val(gameIdStr);
        var gameSets = parts[2];

        // Check each set of cubes revealed
        var sets = listToArray(gameSets, ";");
        var isPossible = true;

        for (var set in sets) {
            // Parse individual cube counts
            var cubes = listToArray(set, ",");

            for (var cube in cubes) {
                cube = trim(cube);
                var cubeParts = listToArray(cube, " ");
                var count = val(cubeParts[1]);
                var color = cubeParts[2];

                if (color == "red" && count > maxRed) {
                    isPossible = false;
                    break;
                } else if (color == "green" && count > maxGreen) {
                    isPossible = false;
                    break;
                } else if (color == "blue" && count > maxBlue) {
                    isPossible = false;
                    break;
                }
            }

            if (!isPossible) break;
        }

        if (isPossible) {
            sumOfIds += gameId;
        }
    }

    return sumOfIds;
}

// Part 2: Find minimum cubes needed for each game and sum the powers
function part2() {
    var sumOfPowers = 0;

    for (var line in lines) {
        // Parse game
        var parts = listToArray(line, ":");
        var gameSets = parts[2];

        // Track minimum cubes needed for this game
        var minRed = 0;
        var minGreen = 0;
        var minBlue = 0;

        // Check each set of cubes revealed
        var sets = listToArray(gameSets, ";");

        for (var set in sets) {
            // Parse individual cube counts
            var cubes = listToArray(set, ",");

            for (var cube in cubes) {
                cube = trim(cube);
                var cubeParts = listToArray(cube, " ");
                var count = val(cubeParts[1]);
                var color = cubeParts[2];

                if (color == "red" && count > minRed) {
                    minRed = count;
                } else if (color == "green" && count > minGreen) {
                    minGreen = count;
                } else if (color == "blue" && count > minBlue) {
                    minBlue = count;
                }
            }
        }

        // Calculate power (red * green * blue)
        var power = minRed * minGreen * minBlue;
        sumOfPowers += power;
    }

    return sumOfPowers;
}

// Output results
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
