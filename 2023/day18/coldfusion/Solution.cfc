component {

    /**
     * Day 18: Lavaduct Lagoon - Polygon area with Shoelace formula and Pick's theorem.
     *
     * Uses Shoelace formula for polygon area combined with Pick's theorem:
     * Total = interior + boundary = area + boundary/2 + 1
     */

    function run() {
        var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
        var inputPath = scriptDir & "../input.txt";
        var inputText = fileRead(inputPath);
        var instructions = parseInput(inputText);

        writeOutput("Part 1: " & part1(instructions) & chr(10));
        writeOutput("Part 2: " & part2(instructions) & chr(10));
    }

    function parseInput(required string text) {
        var lines = text.trim().split("\r?\n");
        var instructions = [];

        for (var line in lines) {
            var parts = line.split(" ");
            var direction = parts[1];
            var distance = int(parts[2]);
            // Extract hex color without (# and )
            var hexColor = mid(parts[3], 3, 6);
            arrayAppend(instructions, {
                direction: direction,
                distance: distance,
                color: hexColor
            });
        }

        return instructions;
    }

    function calculateArea(required array vertices, required numeric perimeter) {
        /**
         * Calculate total area using Shoelace formula and Pick's theorem.
         *
         * Shoelace gives us twice the signed area of the polygon.
         * Pick's theorem: A = i + b/2 - 1, where i = interior points, b = boundary points
         * We want: Total = i + b = A + b/2 + 1
         *
         * Uses precisionEvaluate for arbitrary precision arithmetic.
         */
        var n = arrayLen(vertices);
        var area = precisionEvaluate(0);

        for (var i = 1; i <= n; i++) {
            var j = (i mod n) + 1;
            // area += vertices[i].x * vertices[j].y
            area = precisionEvaluate(area + precisionEvaluate(vertices[i].x * vertices[j].y));
            // area -= vertices[j].x * vertices[i].y
            area = precisionEvaluate(area - precisionEvaluate(vertices[j].x * vertices[i].y));
        }

        // area = abs(area) / 2
        if (area < 0) {
            area = precisionEvaluate(-1 * area);
        }
        area = precisionEvaluate(area / 2);

        // Total points = interior + boundary
        // From Pick's theorem: interior = area - boundary/2 + 1
        // Total = interior + boundary = area + boundary/2 + 1
        var result = precisionEvaluate(area + precisionEvaluate(perimeter / 2) + 1);
        return result;
    }

    function part1(required array instructions) {
        /**
         * Part 1: Follow the dig plan directions.
         * R=right, D=down, L=left, U=up
         */
        var directionMap = {
            "R": {dr: 0, dc: 1},
            "D": {dr: 1, dc: 0},
            "L": {dr: 0, dc: -1},
            "U": {dr: -1, dc: 0}
        };

        var vertices = [{x: 0, y: 0}];
        var perimeter = 0;
        var r = 0;
        var c = 0;

        for (var instr in instructions) {
            var dir = directionMap[instr.direction];
            r += dir.dr * instr.distance;
            c += dir.dc * instr.distance;
            arrayAppend(vertices, {x: r, y: c});
            perimeter += instr.distance;
        }

        return calculateArea(vertices, perimeter);
    }

    function part2(required array instructions) {
        /**
         * Part 2: Decode instructions from hex color codes.
         * Last digit of hex: 0=R, 1=D, 2=L, 3=U
         * First 5 digits: distance in hexadecimal
         */
        var directionMap = {
            "0": {dr: 0, dc: 1},   // R
            "1": {dr: 1, dc: 0},   // D
            "2": {dr: 0, dc: -1},  // L
            "3": {dr: -1, dc: 0}   // U
        };

        var vertices = [{x: precisionEvaluate(0), y: precisionEvaluate(0)}];
        var perimeter = precisionEvaluate(0);
        var r = precisionEvaluate(0);
        var c = precisionEvaluate(0);

        for (var instr in instructions) {
            var color = instr.color;
            // First 5 hex digits = distance
            var hexDistance = left(color, 5);
            var distance = inputBaseN(hexDistance, 16);
            // Last digit = direction
            var dirCode = right(color, 1);
            var dir = directionMap[dirCode];

            r = precisionEvaluate(r + precisionEvaluate(dir.dr * distance));
            c = precisionEvaluate(c + precisionEvaluate(dir.dc * distance));
            arrayAppend(vertices, {x: r, y: c});
            perimeter = precisionEvaluate(perimeter + distance);
        }

        return calculateArea(vertices, perimeter);
    }

}
