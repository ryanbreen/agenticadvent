component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Parse moves
        var moves = parseInput(inputText);

        // Solve both parts
        var part1Result = simulateRope(moves, 2);
        var part2Result = simulateRope(moves, 10);

        systemOutput("Part 1: " & part1Result, true);
        systemOutput("Part 2: " & part2Result, true);
    }

    /**
     * Parse the input into an array of move structs {direction, count}
     */
    function parseInput(required string input) {
        var lines = listToArray(arguments.input, chr(10));
        var moves = [];

        for (var line in lines) {
            line = trim(line);
            if (len(line) == 0) continue;

            var parts = listToArray(line, " ");
            arrayAppend(moves, {
                direction: parts[1],
                count: val(parts[2])
            });
        }

        return moves;
    }

    /**
     * Get direction deltas for a given direction character
     */
    function getDirection(required string dir) {
        switch (arguments.dir) {
            case "U": return {dx: 0, dy: 1};
            case "D": return {dx: 0, dy: -1};
            case "L": return {dx: -1, dy: 0};
            case "R": return {dx: 1, dy: 0};
        }
        return {dx: 0, dy: 0};
    }

    /**
     * Get sign of a number (-1, 0, or 1)
     */
    function sign(required numeric x) {
        if (arguments.x == 0) return 0;
        return arguments.x > 0 ? 1 : -1;
    }

    /**
     * Move tail knot toward head knot if not adjacent
     * Returns new position as struct {x, y}
     */
    function moveTail(required struct head, required struct tail) {
        var dx = arguments.head.x - arguments.tail.x;
        var dy = arguments.head.y - arguments.tail.y;

        // If adjacent or overlapping, don't move
        if (abs(dx) <= 1 && abs(dy) <= 1) {
            return {x: arguments.tail.x, y: arguments.tail.y};
        }

        // Move toward head using sign of delta
        return {
            x: arguments.tail.x + sign(dx),
            y: arguments.tail.y + sign(dy)
        };
    }

    /**
     * Simulate rope with given length and return count of unique tail positions
     */
    function simulateRope(required array moves, required numeric ropeLength) {
        // Initialize all knots at origin
        var knots = [];
        for (var i = 1; i <= arguments.ropeLength; i++) {
            arrayAppend(knots, {x: 0, y: 0});
        }

        // Use struct as set for visited positions (key = "x,y")
        var visited = {};
        visited["0,0"] = true;

        // Process each move
        for (var move in arguments.moves) {
            var delta = getDirection(move.direction);

            // Repeat move count times
            for (var step = 1; step <= move.count; step++) {
                // Move head
                knots[1].x = knots[1].x + delta.dx;
                knots[1].y = knots[1].y + delta.dy;

                // Move each subsequent knot toward the one before it
                for (var k = 2; k <= arguments.ropeLength; k++) {
                    var newPos = moveTail(knots[k-1], knots[k]);
                    knots[k].x = newPos.x;
                    knots[k].y = newPos.y;
                }

                // Record tail position
                var tailKey = knots[arguments.ropeLength].x & "," & knots[arguments.ropeLength].y;
                visited[tailKey] = true;
            }
        }

        return structCount(visited);
    }

}
