<cfscript>
// Read input file
inputPath = getDirectoryFromPath(getCurrentTemplatePath()) & "../input.txt";
inputText = fileRead(inputPath).trim();

// Part 1: Find invalid IDs (pattern repeated twice)
function part1() {
    // Parse ranges from input
    var ranges = [];
    var parts = listToArray(inputText, ",");

    for (var part in parts) {
        part = trim(part);
        if (find("-", part) > 0) {
            // Split on dash to get start and end
            var dashPos = find("-", part);
            var start = val(left(part, dashPos - 1));
            var end = val(mid(part, dashPos + 1, len(part)));
            arrayAppend(ranges, {start: start, end: end});
        }
    }

    var total = 0;

    for (var range in ranges) {
        for (var num = range.start; num <= range.end; num++) {
            if (isInvalidId(num)) {
                total += num;
            }
        }
    }

    return total;
}

// Check if a number is invalid (a pattern repeated twice)
function isInvalidId(num) {
    var s = toString(num);
    var length = len(s);

    // Must have even length to be repeated twice
    if (length % 2 != 0) {
        return false;
    }

    // Check if it starts with 0 (leading zeros not allowed)
    if (left(s, 1) == "0") {
        return false;
    }

    // Split in half and check if both halves are identical
    var mid = int(length / 2);
    var firstHalf = left(s, mid);
    var secondHalf = right(s, mid);

    return firstHalf == secondHalf;
}

// Part 2: Find invalid IDs (pattern repeated at least twice)
function part2() {
    // Parse ranges from input
    var ranges = [];
    var parts = listToArray(inputText, ",");

    for (var part in parts) {
        part = trim(part);
        if (find("-", part) > 0) {
            // Split on dash to get start and end
            var dashPos = find("-", part);
            var start = val(left(part, dashPos - 1));
            var end = val(mid(part, dashPos + 1, len(part)));
            arrayAppend(ranges, {start: start, end: end});
        }
    }

    var total = 0;

    for (var range in ranges) {
        for (var num = range.start; num <= range.end; num++) {
            if (isInvalidIdRepeated(num)) {
                total += num;
            }
        }
    }

    return total;
}

// Check if a number is invalid (a pattern repeated at least twice)
function isInvalidIdRepeated(num) {
    var s = toString(num);
    var length = len(s);

    // Check if it starts with 0 (leading zeros not allowed)
    if (left(s, 1) == "0") {
        return false;
    }

    // Try all possible pattern lengths from 1 to length//2
    // The pattern must be repeated at least twice, so max pattern length is length//2
    for (var patternLength = 1; patternLength <= int(length / 2); patternLength++) {
        // Check if the string length is divisible by pattern_length
        if (length % patternLength == 0) {
            var pattern = left(s, patternLength);
            var repetitions = int(length / patternLength);

            // Check if repeating the pattern gives us the original string
            var repeated = "";
            for (var i = 1; i <= repetitions; i++) {
                repeated &= pattern;
            }

            if (repeated == s) {
                return true;
            }
        }
    }

    return false;
}

// Output results
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
