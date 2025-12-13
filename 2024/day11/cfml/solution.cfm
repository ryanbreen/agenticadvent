<cfscript>
// Read input file
inputText = fileRead("../input.txt").trim();

// Parse space-separated numbers
stones = listToArray(inputText, " ");

// Memoization cache - struct with "value,blinks" as keys
cache = {};

/**
 * Count how many stones result from a single stone after N blinks.
 * Uses memoization to avoid recalculating the same (value, blinks) pairs.
 */
function countStones(required numeric value, required numeric blinks) {
    // Base case
    if (arguments.blinks == 0) {
        return 1;
    }

    // Check cache
    var cacheKey = arguments.value & "," & arguments.blinks;
    if (structKeyExists(cache, cacheKey)) {
        return cache[cacheKey];
    }

    var result = 0;

    // Rule 1: 0 becomes 1
    if (arguments.value == 0) {
        result = countStones(1, arguments.blinks - 1);
    }
    // Rule 2: Even number of digits -> split
    else {
        var valueStr = toString(arguments.value);
        var digitCount = len(valueStr);

        if (digitCount % 2 == 0) {
            var mid = digitCount / 2;
            var leftStr = left(valueStr, mid);
            var rightStr = right(valueStr, mid);

            // Convert to numbers (this removes leading zeros)
            var leftNum = val(leftStr);
            var rightNum = val(rightStr);

            result = countStones(leftNum, arguments.blinks - 1) + countStones(rightNum, arguments.blinks - 1);
        }
        // Rule 3: Multiply by 2024
        else {
            // Use precisionEvaluate for big number math
            var newValue = precisionEvaluate(arguments.value * 2024);
            result = countStones(newValue, arguments.blinks - 1);
        }
    }

    // Cache the result
    cache[cacheKey] = result;

    return result;
}

function part1() {
    var total = 0;
    for (var stone in stones) {
        var stoneNum = val(stone);
        total += countStones(stoneNum, 25);
    }
    return total;
}

function part2() {
    // Clear cache between parts
    cache = {};

    var total = 0;
    for (var stone in stones) {
        var stoneNum = val(stone);
        total += countStones(stoneNum, 75);
    }
    return total;
}

// Run both parts
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
