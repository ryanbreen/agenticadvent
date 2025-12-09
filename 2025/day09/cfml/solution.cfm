<cfscript>
// Read input file
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputText = fileRead(inputPath).trim();

// Parse input - each line is "x,y"
points = [];
lines = listToArray(inputText, chr(10));
for (line in lines) {
    coords = listToArray(line, ",");
    arrayAppend(points, {x: val(coords[1]), y: val(coords[2])});
}

function part1() {
    var maxArea = 0;
    var n = arrayLen(points);

    // Check all pairs of points as opposite corners
    for (var i = 1; i <= n; i++) {
        var x1 = points[i].x;
        var y1 = points[i].y;

        for (var j = i + 1; j <= n; j++) {
            var x2 = points[j].x;
            var y2 = points[j].y;

            // Rectangle area = width * height (inclusive of both corners)
            var width = abs(x2 - x1) + 1;
            var height = abs(y2 - y1) + 1;
            var area = width * height;

            if (area > maxArea) {
                maxArea = area;
            }
        }
    }

    return maxArea;
}

function part2() {
    var n = arrayLen(points);
    var horizontalEdges = [];
    var verticalEdges = [];

    // Build edges from consecutive points
    for (var i = 1; i <= n; i++) {
        var x1 = points[i].x;
        var y1 = points[i].y;
        var nextIdx = (i mod n) + 1;  // Wraps around to 1
        var x2 = points[nextIdx].x;
        var y2 = points[nextIdx].y;

        if (y1 == y2) {  // Horizontal edge
            arrayAppend(horizontalEdges, {
                y: y1,
                xMin: min(x1, x2),
                xMax: max(x1, x2)
            });
        } else {  // Vertical edge
            arrayAppend(verticalEdges, {
                x: x1,
                yMin: min(y1, y2),
                yMax: max(y1, y2)
            });
        }
    }

    // Sort vertical edges by x coordinate for efficient lookup
    arraySort(verticalEdges, function(a, b) {
        return a.x - b.x;
    });

    // Build maps for efficient edge lookup
    var vertByX = {};
    for (var edge in verticalEdges) {
        if (!structKeyExists(vertByX, edge.x)) {
            vertByX[edge.x] = [];
        }
        arrayAppend(vertByX[edge.x], {yMin: edge.yMin, yMax: edge.yMax});
    }

    var horizByY = {};
    for (var edge in horizontalEdges) {
        if (!structKeyExists(horizByY, edge.y)) {
            horizByY[edge.y] = [];
        }
        arrayAppend(horizByY[edge.y], {xMin: edge.xMin, xMax: edge.xMax});
    }

    // Find largest valid rectangle with red corners
    var maxArea = 0;

    for (var i = 1; i <= n; i++) {
        var px1 = points[i].x;
        var py1 = points[i].y;

        for (var j = i + 1; j <= n; j++) {
            var px2 = points[j].x;
            var py2 = points[j].y;

            // Check if rectangle is valid
            var minX = min(px1, px2);
            var maxX = max(px1, px2);
            var minY = min(py1, py2);
            var maxY = max(py1, py2);

            var valid = true;

            // Check if any vertical edge crosses through rectangle interior
            for (var vxKey in vertByX) {
                var vx = val(vxKey);
                if (minX < vx && vx < maxX) {
                    for (var edgeSeg in vertByX[vxKey]) {
                        if (!(edgeSeg.yMax <= minY || edgeSeg.yMin >= maxY)) {
                            valid = false;
                            break;
                        }
                    }
                    if (!valid) break;
                }
            }

            if (!valid) continue;

            // Check if any horizontal edge crosses through rectangle interior
            for (var hyKey in horizByY) {
                var hy = val(hyKey);
                if (minY < hy && hy < maxY) {
                    for (var edgeSeg in horizByY[hyKey]) {
                        if (!(edgeSeg.xMax <= minX || edgeSeg.xMin >= maxX)) {
                            valid = false;
                            break;
                        }
                    }
                    if (!valid) break;
                }
            }

            if (!valid) continue;

            // Check that rectangle center is inside the polygon
            var centerX = (minX + maxX) / 2;
            var centerY = (minY + maxY) / 2;

            // Ray casting to check if inside polygon
            var crossings = 0;
            var sortedXCoords = structKeyArray(vertByX);
            arraySort(sortedXCoords, "numeric");

            for (var vxStr in sortedXCoords) {
                var vxNum = val(vxStr);
                if (vxNum <= centerX) {
                    continue;
                }

                for (var edgeSeg in vertByX[vxStr]) {
                    if (edgeSeg.yMin < centerY && centerY < edgeSeg.yMax) {
                        crossings++;
                    } else if (centerY == edgeSeg.yMin || centerY == edgeSeg.yMax) {
                        crossings += 0.5;
                    }
                }
            }

            var isInside = (crossings mod 2 == 1);

            if (isInside) {
                var width = abs(px2 - px1) + 1;
                var height = abs(py2 - py1) + 1;
                var area = width * height;

                if (area > maxArea) {
                    maxArea = area;
                }
            }
        }
    }

    return maxArea;
}

// Run both parts
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
