component {

    function run() {
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        var commands = parseInput(inputText);

        var part1Result = part1(commands);
        var part2Result = part2(commands);

        writeOutput("Part 1: " & part1Result & chr(10));
        writeOutput("Part 2: " & part2Result & chr(10));
    }

    function parseInput(inputText) {
        var lines = listToArray(inputText, chr(10));
        var commands = [];

        for (var line in lines) {
            var trimmedLine = trim(line);
            if (len(trimmedLine) > 0) {
                var parts = listToArray(trimmedLine, " ");
                arrayAppend(commands, {
                    "direction": parts[1],
                    "value": val(parts[2])
                });
            }
        }

        return commands;
    }

    function part1(commands) {
        var horizontal = 0;
        var depth = 0;

        for (var cmd in commands) {
            switch (cmd.direction) {
                case "forward":
                    horizontal += cmd.value;
                    break;
                case "down":
                    depth += cmd.value;
                    break;
                case "up":
                    depth -= cmd.value;
                    break;
            }
        }

        return horizontal * depth;
    }

    function part2(commands) {
        var horizontal = 0;
        var depth = 0;
        var aim = 0;

        for (var cmd in commands) {
            switch (cmd.direction) {
                case "forward":
                    horizontal += cmd.value;
                    depth += aim * cmd.value;
                    break;
                case "down":
                    aim += cmd.value;
                    break;
                case "up":
                    aim -= cmd.value;
                    break;
            }
        }

        return horizontal * depth;
    }
}
