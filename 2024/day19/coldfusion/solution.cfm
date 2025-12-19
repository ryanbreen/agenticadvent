<cfscript>
/**
 * Advent of Code 2024 - Day 19: Linen Layout
 *
 * Part 1: Count designs that CAN be formed by concatenating towel patterns
 * Part 2: Sum the NUMBER OF WAYS each design can be formed
 *
 * Uses dynamic programming with memoization
 */

// Read input file - get directory of this script
scriptPath = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptPath & "../input.txt";
inputText = trim(fileRead(inputPath));

// Parse input - split all lines first (includeEmptyFields=true to preserve blank line)
allLines = listToArray(inputText, chr(10), true);

// First line contains patterns (comma-separated)
patternLine = allLines[1];
patterns = [];
for (p in listToArray(patternLine, ",")) {
    arrayAppend(patterns, trim(p));
}

// Skip the blank line (line 2), remaining lines are designs
designs = [];
for (i = 3; i <= arrayLen(allLines); i++) {
    line = trim(allLines[i]);
    if (len(line) > 0) {
        arrayAppend(designs, line);
    }
}

/**
 * Count number of ways to form a design from patterns
 * Uses dynamic programming with memoization
 * Returns 0 if impossible, otherwise the count of ways
 */
function countWays(design, patterns) {
    var memo = {};
    var designLen = len(design);

    // Inner recursive function with memoization
    var dp = function(pos) {
        // Base case: reached end of design
        if (pos > designLen) {
            return 1;
        }

        // Check memo
        var key = pos;
        if (structKeyExists(memo, key)) {
            return memo[key];
        }

        var total = 0;

        // Try each pattern
        for (var pattern in patterns) {
            var plen = len(pattern);
            // Check if pattern matches at current position
            if (pos + plen - 1 <= designLen) {
                var substring = mid(design, pos, plen);
                if (substring == pattern) {
                    total += dp(pos + plen);
                }
            }
        }

        memo[key] = total;
        return total;
    };

    return dp(1); // CFML strings are 1-indexed
}

/**
 * Part 1: Count how many designs can be formed
 */
function part1(designs, patterns) {
    var count = 0;
    for (var design in designs) {
        if (countWays(design, patterns) > 0) {
            count++;
        }
    }
    return count;
}

/**
 * Part 2: Sum the number of ways each design can be formed
 */
function part2(designs, patterns) {
    var total = 0;
    for (var design in designs) {
        total += countWays(design, patterns);
    }
    return total;
}

// Run both parts
result1 = part1(designs, patterns);
result2 = part2(designs, patterns);

writeOutput("Part 1: " & result1 & chr(10));
writeOutput("Part 2: " & result2 & chr(10));
</cfscript>
