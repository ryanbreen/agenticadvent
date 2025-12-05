<cfscript>
// Read input file
baseDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = baseDir & "../input.txt";
inputText = fileRead(inputPath).trim();

// Split into two sections by double newline
sections = [];
if (find(chr(10) & chr(10), inputText) > 0) {
    // Unix line endings
    sections = inputText.split(chr(10) & chr(10));
} else if (find(chr(13) & chr(10) & chr(13) & chr(10), inputText) > 0) {
    // Windows line endings
    sections = inputText.split(chr(13) & chr(10) & chr(13) & chr(10));
} else {
    // Fallback - try single newline split and look for empty
    sections = [inputText, ""];
}

// Parse each section into lines
rangeLines = listToArray(sections[1], chr(10));
ingredientLines = [];
if (arrayLen(sections) > 1) {
    ingredientLines = listToArray(sections[2], chr(10));
}

// Helper function to parse large integer from string using Java BigInteger
function parseBigInt(str) {
    str = trim(str);
    // Remove any non-numeric characters
    str = reReplace(str, "[^0-9]", "", "ALL");
    return createObject("java", "java.math.BigInteger").init(str);
}

// Helper function to compare BigIntegers (returns -1, 0, or 1)
function compareBigInt(a, b) {
    return a.compareTo(b);
}

// Helper function to add BigIntegers
function addBigInt(a, b) {
    return a.add(b);
}

// Helper function to subtract BigIntegers
function subtractBigInt(a, b) {
    return a.subtract(b);
}

// Part 1: Count how many available IDs fall within ANY range
function part1() {
    // Parse ranges from the first section
    ranges = [];
    for (line in rangeLines) {
        if (trim(line) == "") continue;
        dashPos = find("-", line);
        startStr = left(line, dashPos - 1);
        endStr = mid(line, dashPos + 1, len(line));
        arrayAppend(ranges, {
            start: parseBigInt(startStr),
            end: parseBigInt(endStr)
        });
    }

    // Parse ingredient IDs from the second section
    ingredientIds = [];
    for (line in ingredientLines) {
        if (trim(line) != "") {
            arrayAppend(ingredientIds, parseBigInt(line));
        }
    }

    // Count how many ingredient IDs fall within any range
    freshCount = 0;
    for (ingredientId in ingredientIds) {
        found = false;
        for (range in ranges) {
            // Check if ingredientId >= range.start && ingredientId <= range.end
            if (compareBigInt(ingredientId, range.start) >= 0 && compareBigInt(ingredientId, range.end) <= 0) {
                freshCount++;
                found = true;
                break;
            }
        }
    }

    return freshCount;
}

// Part 2: Count total unique IDs covered by ALL ranges (merge overlapping ranges)
function part2() {
    // Parse ranges from the first section
    ranges = [];
    for (line in rangeLines) {
        if (trim(line) == "") continue;
        dashPos = find("-", line);
        startStr = left(line, dashPos - 1);
        endStr = mid(line, dashPos + 1, len(line));
        arrayAppend(ranges, {
            start: parseBigInt(startStr),
            end: parseBigInt(endStr)
        });
    }

    // Sort ranges by start position
    arraySort(ranges, function(a, b) {
        return compareBigInt(a.start, b.start);
    });

    // Merge overlapping ranges
    merged = [];
    ONE = parseBigInt("1");
    for (range in ranges) {
        if (arrayLen(merged) > 0) {
            lastRange = merged[arrayLen(merged)];
            // Check if range.start <= lastRange.end + 1
            if (compareBigInt(range.start, addBigInt(lastRange.end, ONE)) <= 0) {
                // Overlapping or adjacent - merge with the last range
                // lastRange.end = max(lastRange.end, range.end)
                if (compareBigInt(range.end, lastRange.end) > 0) {
                    lastRange.end = range.end;
                }
            } else {
                // No overlap - add as new range
                arrayAppend(merged, {
                    start: range.start,
                    end: range.end
                });
            }
        } else {
            // First range
            arrayAppend(merged, {
                start: range.start,
                end: range.end
            });
        }
    }

    // Count total unique IDs covered by merged ranges
    totalCount = parseBigInt("0");
    for (range in merged) {
        // totalCount += (range.end - range.start + 1)
        rangeSize = addBigInt(subtractBigInt(range.end, range.start), ONE);
        totalCount = addBigInt(totalCount, rangeSize);
    }

    return totalCount.toString();
}

// Run both parts
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
