<cfscript>
/**
 * Day 22: Sand Slabs
 * 3D falling bricks simulation.
 * Part 1: Count bricks that can be safely disintegrated.
 * Part 2: Count total bricks that would fall for each disintegration.
 */

// Read input file
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputLines = listToArray(fileRead(inputPath), chr(10));

/**
 * Parse brick coordinates from input
 * Each brick is stored as a struct with x1,y1,z1,x2,y2,z2
 */
function parseBricks(lines) {
    var bricks = [];

    for (var line in lines) {
        line = trim(line);
        if (len(line) == 0) continue;

        var parts = listToArray(line, "~");
        var left = listToArray(parts[1], ",");
        var right = listToArray(parts[2], ",");

        var x1 = int(left[1]);
        var y1 = int(left[2]);
        var z1 = int(left[3]);
        var x2 = int(right[1]);
        var y2 = int(right[2]);
        var z2 = int(right[3]);

        // Ensure z1 <= z2 for consistent processing
        if (z1 > z2) {
            var temp = x1; x1 = x2; x2 = temp;
            temp = y1; y1 = y2; y2 = temp;
            temp = z1; z1 = z2; z2 = temp;
        }

        arrayAppend(bricks, {
            "x1": x1, "y1": y1, "z1": z1,
            "x2": x2, "y2": y2, "z2": z2
        });
    }

    return bricks;
}

/**
 * Simulate bricks falling and settling.
 * Returns struct with:
 *   - settled: array of settled brick positions
 *   - supports: struct where supports[i] = set of brick indices that brick i supports
 *   - supporters: struct where supporters[i] = set of brick indices that support brick i
 */
function settleBricks(bricks) {
    var numBricks = arrayLen(bricks);

    // Create indexed array of bricks for sorting
    var sortedBricks = [];
    for (var i = 1; i <= numBricks; i++) {
        arrayAppend(sortedBricks, {
            "idx": i,
            "brick": bricks[i]
        });
    }

    // Sort by minimum z coordinate (numeric comparison)
    arraySort(sortedBricks, function(a, b) {
        if (a.brick.z1 < b.brick.z1) return -1;
        if (a.brick.z1 > b.brick.z1) return 1;
        return 0;
    });

    // Track occupied cells: "x,y,z" -> brick index
    var occupied = {};

    // Settled bricks (keyed by original index)
    var settled = {};

    // Support relationships
    var supports = {};     // supports[i] = bricks that i supports (above)
    var supporters = {};   // supporters[i] = bricks that support i (below)

    for (var i = 1; i <= numBricks; i++) {
        supports[i] = {};
        supporters[i] = {};
    }

    for (var item in sortedBricks) {
        var origIdx = item.idx;
        var brick = item.brick;

        var x1 = brick.x1;
        var y1 = brick.y1;
        var z1 = brick.z1;
        var x2 = brick.x2;
        var y2 = brick.y2;
        var z2 = brick.z2;

        // Find the lowest z where this brick can rest
        var drop = z1 - 1;  // Maximum drop (to z=1)

        // Get xy footprint of this brick
        var minX = min(x1, x2);
        var maxX = max(x1, x2);
        var minY = min(y1, y2);
        var maxY = max(y1, y2);

        for (var x = minX; x <= maxX; x++) {
            for (var y = minY; y <= maxY; y++) {
                // Check each z level below the brick
                for (var z = z1 - 1; z >= 1; z--) {
                    var key = x & "," & y & "," & z;
                    if (structKeyExists(occupied, key)) {
                        drop = min(drop, z1 - z - 1);
                        break;
                    }
                }
            }
        }

        // Drop the brick
        var newZ1 = z1 - drop;
        var newZ2 = z2 - drop;
        var newBrick = {
            "x1": x1, "y1": y1, "z1": newZ1,
            "x2": x2, "y2": y2, "z2": newZ2
        };
        settled[origIdx] = newBrick;

        // Mark cells as occupied and find supporters
        for (var x = minX; x <= maxX; x++) {
            for (var y = minY; y <= maxY; y++) {
                // Check if there's a brick directly below
                var belowKey = x & "," & y & "," & (newZ1 - 1);
                if (structKeyExists(occupied, belowKey)) {
                    var supporterIdx = occupied[belowKey];
                    supporters[origIdx][supporterIdx] = true;
                    supports[supporterIdx][origIdx] = true;
                }

                // Mark all cells of this brick as occupied
                for (var z = newZ1; z <= newZ2; z++) {
                    var cellKey = x & "," & y & "," & z;
                    occupied[cellKey] = origIdx;
                }
            }
        }
    }

    return {
        "settled": settled,
        "supports": supports,
        "supporters": supporters
    };
}

/**
 * Part 1: Count bricks that can be safely disintegrated
 */
function part1(bricks) {
    var result = settleBricks(bricks);
    var supports = result.supports;
    var supporters = result.supporters;
    var numBricks = arrayLen(bricks);

    var safeCount = 0;

    for (var i = 1; i <= numBricks; i++) {
        // Brick i can be safely removed if every brick it supports
        // has at least one other supporter
        var canRemove = true;

        for (var supported in supports[i]) {
            if (structCount(supporters[supported]) == 1) {
                canRemove = false;
                break;
            }
        }

        if (canRemove) {
            safeCount++;
        }
    }

    return safeCount;
}

/**
 * Part 2: Count total bricks that would fall for each disintegration
 */
function part2(bricks) {
    var result = settleBricks(bricks);
    var supports = result.supports;
    var supporters = result.supporters;
    var numBricks = arrayLen(bricks);

    var totalFalls = 0;

    for (var i = 1; i <= numBricks; i++) {
        // Simulate removing brick i and count chain reaction
        // BFS to find all bricks that would fall
        var falling = {};
        falling[i] = true;

        var queue = [];
        arrayAppend(queue, i);
        var queueIdx = 1;

        while (queueIdx <= arrayLen(queue)) {
            var brick = queue[queueIdx];
            queueIdx++;

            // Check all bricks that this brick supports
            for (var supported in supports[brick]) {
                if (structKeyExists(falling, supported)) {
                    continue;
                }

                // This brick falls if all its supporters have fallen
                var allSupportersFallen = true;
                for (var supporter in supporters[supported]) {
                    if (!structKeyExists(falling, supporter)) {
                        allSupportersFallen = false;
                        break;
                    }
                }

                if (allSupportersFallen) {
                    falling[supported] = true;
                    arrayAppend(queue, supported);
                }
            }
        }

        // Don't count the initial brick we removed
        totalFalls += structCount(falling) - 1;
    }

    return totalFalls;
}

// Main execution
bricks = parseBricks(inputLines);
writeOutput("Part 1: " & part1(bricks) & chr(10));
writeOutput("Part 2: " & part2(bricks) & chr(10));
</cfscript>
