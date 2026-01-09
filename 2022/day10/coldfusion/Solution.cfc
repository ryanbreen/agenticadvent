component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Parse instructions
        var instructions = parseInput(inputText);

        // Solve both parts
        var part1Result = part1(instructions);
        var part2Result = part2(instructions);

        systemOutput("Part 1: " & part1Result, true);
        systemOutput("Part 2:", true);
        systemOutput(part2Result, true);
    }

    /**
     * Parse the input into an array of instruction lines
     */
    function parseInput(required string input) {
        var lines = listToArray(arguments.input, chr(10));
        var instructions = [];

        for (var line in lines) {
            line = trim(line);
            if (len(line) > 0) {
                arrayAppend(instructions, line);
            }
        }

        return instructions;
    }

    /**
     * Simulate CPU and return array of {cycle, x} structs for each cycle
     */
    function simulateCPU(required array instructions) {
        var x = 1;
        var cycle = 0;
        var history = [];

        for (var line in arguments.instructions) {
            if (line == "noop") {
                cycle++;
                arrayAppend(history, {cycle: cycle, x: x});
            } else {
                // addx V - takes 2 cycles
                var parts = listToArray(line, " ");
                var v = val(parts[2]);

                // First cycle of addx
                cycle++;
                arrayAppend(history, {cycle: cycle, x: x});

                // Second cycle of addx
                cycle++;
                arrayAppend(history, {cycle: cycle, x: x});

                // After second cycle, update x
                x = x + v;
            }
        }

        return history;
    }

    /**
     * Part 1: Sum signal strengths at cycles 20, 60, 100, 140, 180, 220
     */
    function part1(required array instructions) {
        var targetCycles = {20: true, 60: true, 100: true, 140: true, 180: true, 220: true};
        var total = 0;

        var history = simulateCPU(arguments.instructions);

        for (var state in history) {
            if (structKeyExists(targetCycles, state.cycle)) {
                total = total + (state.cycle * state.x);
            }
        }

        return total;
    }

    /**
     * Part 2: Render CRT display
     * Sprite is 3 pixels wide centered at X position
     */
    function part2(required array instructions) {
        var screen = [];
        var row = [];

        var history = simulateCPU(arguments.instructions);

        for (var state in history) {
            // CRT position in current row (0-39)
            var pos = (state.cycle - 1) mod 40;

            // Sprite is 3 pixels wide centered at X
            // Draw '#' if sprite overlaps current position
            if (abs(pos - state.x) <= 1) {
                arrayAppend(row, chr(35));
            } else {
                arrayAppend(row, ".");
            }

            // End of row (40 pixels)
            if (state.cycle mod 40 == 0) {
                arrayAppend(screen, arrayToList(row, ""));
                row = [];
            }
        }

        return arrayToList(screen, chr(10));
    }

}
