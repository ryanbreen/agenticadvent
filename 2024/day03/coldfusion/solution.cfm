<cfscript>
// Advent of Code 2024 - Day 3: Mull It Over

function part1(data) {
    // Find all valid mul(X,Y) instructions and sum their products
    var pattern = "mul\((\d{1,3}),(\d{1,3})\)";
    var matches = reMatch(pattern, data);
    var total = 0;

    for (var match in matches) {
        // Extract the two numbers from the match
        var nums = reMatch("\d{1,3}", match);
        if (arrayLen(nums) == 2) {
            total += val(nums[1]) * val(nums[2]);
        }
    }

    return total;
}

function part2(data) {
    // Like part1, but do() enables and don't() disables mul instructions
    var total = 0;
    var enabled = true;
    var events = [];
    var i = 1;
    var dataLen = len(data);

    // Scan through the string looking for mul(), do(), and don't()
    while (i <= dataLen) {
        // Check for don't()
        if (mid(data, i, 7) == "don't()") {
            arrayAppend(events, {
                pos: i,
                type: "dont",
                x: 0,
                y: 0
            });
            i += 7;
        }
        // Check for do()
        else if (mid(data, i, 4) == "do()") {
            arrayAppend(events, {
                pos: i,
                type: "do",
                x: 0,
                y: 0
            });
            i += 4;
        }
        // Check for mul(
        else if (mid(data, i, 4) == "mul(") {
            var j = i + 4;
            var num1 = "";
            var num2 = "";

            // Read first number (1-3 digits)
            while (j <= dataLen && isNumeric(mid(data, j, 1)) && len(num1) < 3) {
                num1 &= mid(data, j, 1);
                j++;
            }

            // Check for comma
            if (j <= dataLen && mid(data, j, 1) == "," && len(num1) > 0) {
                j++;

                // Read second number (1-3 digits)
                while (j <= dataLen && isNumeric(mid(data, j, 1)) && len(num2) < 3) {
                    num2 &= mid(data, j, 1);
                    j++;
                }

                // Check for closing paren
                if (j <= dataLen && mid(data, j, 1) == ")" && len(num2) > 0) {
                    arrayAppend(events, {
                        pos: i,
                        type: "mul",
                        x: val(num1),
                        y: val(num2)
                    });
                }
            }
            i++;
        }
        else {
            i++;
        }
    }

    // Process events in order (they're already in order since we scanned linearly)
    for (var event in events) {
        if (event.type == "do") {
            enabled = true;
        } else if (event.type == "dont") {
            enabled = false;
        } else if (event.type == "mul" && enabled) {
            total += event.x * event.y;
        }
    }

    return total;
}

// Read input file
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
data = fileRead(inputPath);

// Solve both parts
writeOutput("Part 1: " & part1(data) & chr(10));
writeOutput("Part 2: " & part2(data) & chr(10));
</cfscript>
