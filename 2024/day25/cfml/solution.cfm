<cfscript>
/**
 * Day 25: Code Chronicle - Lock and key matching
 */

function parseInput(text) {
    var locks = [];
    var keys = [];

    // Split by double newlines to get individual schematics
    var schematics = listToArray(text.trim(), chr(10) & chr(10), false, true);

    for (var schematic in schematics) {
        var lines = listToArray(schematic.trim(), chr(10), false, true);
        var hashChar = chr(35);
        var fiveHashes = repeatString(hashChar, 5);

        // Lock: top row is all hash marks, bottom is all dots.
        // Key: top row is all dots, bottom is all hash marks
        if (lines[1] == fiveHashes) {
            // It's a lock - count hash marks from top (excluding top row)
            var heights = [];
            for (var col = 1; col <= 5; col++) {
                var height = 0;
                for (var row = 2; row <= 7; row++) { // rows 2-7 (1-indexed)
                    if (mid(lines[row], col, 1) == hashChar) {
                        height++;
                    } else {
                        break;
                    }
                }
                arrayAppend(heights, height);
            }
            arrayAppend(locks, heights);
        } else {
            // It's a key - count hash marks from bottom (excluding bottom row)
            var heights = [];
            for (var col = 1; col <= 5; col++) {
                var height = 0;
                for (var row = 6; row >= 1; row--) { // rows 6 down to 1
                    if (mid(lines[row], col, 1) == hashChar) {
                        height++;
                    } else {
                        break;
                    }
                }
                arrayAppend(heights, height);
            }
            arrayAppend(keys, heights);
        }
    }

    return {
        "locks": locks,
        "keys": keys
    };
}

function fits(lock, key) {
    // Check if a key fits a lock (no column exceeds 5)
    for (var i = 1; i <= 5; i++) {
        if (lock[i] + key[i] > 5) {
            return false;
        }
    }
    return true;
}

function part1(locks, keys) {
    // Count unique lock/key pairs that fit together
    var count = 0;
    for (var lock in locks) {
        for (var key in keys) {
            if (fits(lock, key)) {
                count++;
            }
        }
    }
    return count;
}

// Main execution
inputFile = getDirectoryFromPath(getCurrentTemplatePath()) & "../input.txt";
text = fileRead(inputFile);

parsed = parseInput(text);
locks = parsed.locks;
keys = parsed.keys;

answer1 = part1(locks, keys);
writeOutput("Part 1: " & answer1 & chr(10));

// Day 25 typically only has Part 1
writeOutput("Part 2: Merry Christmas!" & chr(10));
</cfscript>
