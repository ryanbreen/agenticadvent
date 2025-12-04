<cfscript>
// Read input file
inputPath = getDirectoryFromPath(getCurrentTemplatePath()) & "../input.txt";
inputText = fileRead(inputPath).trim();
lines = listToArray(inputText, chr(10));

// Part 1: Count times dial lands on 0 at the end of a rotation
function part1() {
    var position = 50;  // Starting position
    var zeroCount = 0;

    for (var line in lines) {
        if (len(trim(line)) == 0) continue;  // Skip empty lines

        var direction = left(line, 1);
        var distance = val(mid(line, 2, len(line)));

        if (direction == 'L') {
            position = (position - distance) % 100;
            if (position < 0) position = position + 100;
        } else {  // direction == 'R'
            position = (position + distance) % 100;
        }

        if (position == 0) {
            zeroCount++;
        }
    }

    return zeroCount;
}

// Part 2: Count times dial passes through 0 during any rotation
function part2() {
    var position = 50;  // Starting position
    var zeroCount = 0;

    for (var line in lines) {
        if (len(trim(line)) == 0) continue;  // Skip empty lines

        var direction = left(line, 1);
        var distance = val(mid(line, 2, len(line)));

        if (direction == 'L') {
            // Moving left (toward lower numbers)
            // We hit 0 after exactly 'position' steps, then every 100 steps after that
            if (position > 0 && distance >= position) {
                zeroCount += 1 + int((distance - position) / 100);
            } else if (position == 0 && distance >= 100) {
                // Starting from 0, we hit it again after 100 steps, then every 100 steps
                zeroCount += int(distance / 100);
            }
        } else {  // direction == 'R'
            // Moving right (toward higher numbers)
            // We hit 0 after (100 - position) steps, then every 100 steps after that
            if (position > 0) {
                var stepsToZero = 100 - position;
                if (distance >= stepsToZero) {
                    zeroCount += 1 + int((distance - stepsToZero) / 100);
                }
            } else {  // position == 0
                // Starting from 0, we hit it again after 100 steps, then every 100 steps
                if (distance >= 100) {
                    zeroCount += int(distance / 100);
                }
            }
        }

        // Update position
        if (direction == 'L') {
            position = (position - distance) % 100;
            if (position < 0) position = position + 100;
        } else {
            position = (position + distance) % 100;
        }
    }

    return zeroCount;
}

// Output results
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
