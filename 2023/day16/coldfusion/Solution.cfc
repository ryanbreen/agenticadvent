component {

    function run() {
        // Get the directory containing this CFC file
        var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
        var inputPath = scriptDir & "../input.txt";
        var inputText = fileRead(inputPath);
        var grid = parseInput(inputText);

        writeOutput("Part 1: " & part1(grid) & chr(10));
        writeOutput("Part 2: " & part2(grid) & chr(10));
    }

    function parseInput(required string text) {
        // Split by newlines to get rows
        var lines = text.trim().split("\r?\n");
        var grid = [];

        for (var line in lines) {
            arrayAppend(grid, line);
        }

        return grid;
    }

    function countEnergized(required array grid, required numeric startRow, required numeric startCol, required numeric startDir) {
        /**
         * Count energized tiles starting from given position and direction.
         * Directions: 0=right, 1=down, 2=left, 3=up
         * Uses BFS with state tracking to avoid infinite loops.
         */
        var rows = arrayLen(grid);
        var cols = len(grid[1]);

        // Direction deltas: 0=right, 1=down, 2=left, 3=up
        var dr = [0, 1, 0, -1];
        var dc = [1, 0, -1, 0];

        // Track visited states as "row,col,dir"
        var visited = {};

        // BFS queue - array of structs with r, c, d
        var queue = [];
        arrayAppend(queue, {r: startRow, c: startCol, d: startDir});

        while (arrayLen(queue) > 0) {
            // Dequeue from front
            var current = queue[1];
            arrayDeleteAt(queue, 1);

            var r = current.r;
            var c = current.c;
            var d = current.d;

            // Bounds check (1-based indexing in CF)
            if (r < 1 || r > rows || c < 1 || c > cols) {
                continue;
            }

            // State key
            var stateKey = r & "," & c & "," & d;
            if (structKeyExists(visited, stateKey)) {
                continue;
            }
            visited[stateKey] = true;

            // Get current cell
            var cell = mid(grid[r], c, 1);

            // Determine next directions based on cell type
            var nextDirs = [];

            if (cell == ".") {
                nextDirs = [d];
            } else if (cell == "/") {
                // right(0)->up(3), down(1)->left(2), left(2)->down(1), up(3)->right(0)
                var slashMap = [3, 2, 1, 0]; // 0-indexed mapping
                nextDirs = [slashMap[d + 1]];
            } else if (cell == "\") {
                // right(0)->down(1), down(1)->right(0), left(2)->up(3), up(3)->left(2)
                var backslashMap = [1, 0, 3, 2]; // 0-indexed mapping
                nextDirs = [backslashMap[d + 1]];
            } else if (cell == "|") {
                // Vertical splitter
                if (d == 0 || d == 2) {
                    // Horizontal beam splits to up and down
                    nextDirs = [1, 3];
                } else {
                    // Vertical beam passes through
                    nextDirs = [d];
                }
            } else if (cell == "-") {
                // Horizontal splitter
                if (d == 1 || d == 3) {
                    // Vertical beam splits to left and right
                    nextDirs = [0, 2];
                } else {
                    // Horizontal beam passes through
                    nextDirs = [d];
                }
            }

            // Add next states to queue
            for (var nd in nextDirs) {
                var newR = r + dr[nd + 1];
                var newC = c + dc[nd + 1];
                arrayAppend(queue, {r: newR, c: newC, d: nd});
            }
        }

        // Count unique tiles (ignore direction in count)
        var tiles = {};
        for (var key in visited) {
            var parts = listToArray(key, ",");
            var tileKey = parts[1] & "," & parts[2];
            tiles[tileKey] = true;
        }

        return structCount(tiles);
    }

    function part1(required array grid) {
        // Beam starts at top-left (1,1) heading right (0)
        return countEnergized(grid, 1, 1, 0);
    }

    function part2(required array grid) {
        /**
         * Try all edge starting positions, find maximum energized tiles.
         */
        var rows = arrayLen(grid);
        var cols = len(grid[1]);
        var maxEnergized = 0;

        // Top row, heading down (1)
        for (var c = 1; c <= cols; c++) {
            var energy = countEnergized(grid, 1, c, 1);
            if (energy > maxEnergized) {
                maxEnergized = energy;
            }
        }

        // Bottom row, heading up (3)
        for (var c = 1; c <= cols; c++) {
            var energy = countEnergized(grid, rows, c, 3);
            if (energy > maxEnergized) {
                maxEnergized = energy;
            }
        }

        // Left column, heading right (0)
        for (var r = 1; r <= rows; r++) {
            var energy = countEnergized(grid, r, 1, 0);
            if (energy > maxEnergized) {
                maxEnergized = energy;
            }
        }

        // Right column, heading left (2)
        for (var r = 1; r <= rows; r++) {
            var energy = countEnergized(grid, r, cols, 2);
            if (energy > maxEnergized) {
                maxEnergized = energy;
            }
        }

        return maxEnergized;
    }

}
