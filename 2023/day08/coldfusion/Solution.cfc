component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Parse the input
        var parsed = parseInput(inputText);
        var instructions = parsed.instructions;
        var network = parsed.network;

        // Solve Part 1
        var part1Result = part1(instructions, network);
        print.line("Part 1: " & part1Result);

        // Solve Part 2
        var part2Result = part2(instructions, network);
        print.line("Part 2: " & part2Result);
    }

    function parseInput(text) {
        var lines = listToArray(text, chr(10));
        // Note: listToArray skips empty lines, so line 2 is already first network entry
        var instructions = lines[1];

        var network = {};
        for (var i = 2; i <= arrayLen(lines); i++) {
            var line = trim(lines[i]);
            if (len(line) == 0) continue;

            // Parse: AAA = (BBB, CCC)
            var parts = listToArray(line, "=");
            if (arrayLen(parts) < 2) continue;

            var node = trim(parts[1]);
            var rest = trim(parts[2]);

            // Remove parentheses
            rest = replace(rest, "(", "", "all");
            rest = replace(rest, ")", "", "all");

            var leftRight = listToArray(rest, ",");
            if (arrayLen(leftRight) < 2) continue;

            var leftNode = trim(leftRight[1]);
            var rightNode = trim(leftRight[2]);

            network[node] = {left: leftNode, right: rightNode};
        }

        return {instructions: instructions, network: network};
    }

    function part1(instructions, network) {
        // Navigate from AAA to ZZZ following L/R instructions
        var current = "AAA";
        var steps = 0;
        var instructionLen = len(instructions);

        while (current != "ZZZ") {
            var idx = (steps mod instructionLen) + 1;
            var instruction = mid(instructions, idx, 1);

            if (instruction == "L") {
                current = network[current].left;
            } else {
                current = network[current].right;
            }
            steps++;
        }

        return steps;
    }

    function part2(instructions, network) {
        // Find all starting nodes (ending in A)
        var startNodes = [];
        var allKeys = structKeyArray(network);
        for (var k = 1; k <= arrayLen(allKeys); k++) {
            var nodeName = allKeys[k];
            if (right(nodeName, 1) == "A") {
                arrayAppend(startNodes, nodeName);
            }
        }

        // For each starting node, find the cycle length to reach a Z node
        var cycleLengths = [];
        var instructionLen = len(instructions);

        for (var j = 1; j <= arrayLen(startNodes); j++) {
            var current = startNodes[j];
            var steps = 0;

            while (right(current, 1) != "Z") {
                var idx = (steps mod instructionLen) + 1;
                var instruction = mid(instructions, idx, 1);

                if (instruction == "L") {
                    current = network[current].left;
                } else {
                    current = network[current].right;
                }
                steps++;
            }
            arrayAppend(cycleLengths, steps);
        }

        // Find LCM of all cycle lengths
        var result = cycleLengths[1];
        for (var i = 2; i <= arrayLen(cycleLengths); i++) {
            result = lcm(result, cycleLengths[i]);
        }

        return result;
    }

    function gcd(a, b) {
        // Greatest common divisor using Euclidean algorithm
        while (b != 0) {
            var temp = b;
            b = a mod b;
            a = temp;
        }
        return a;
    }

    function lcm(a, b) {
        // Least common multiple
        return (a / gcd(a, b)) * b;
    }
}
