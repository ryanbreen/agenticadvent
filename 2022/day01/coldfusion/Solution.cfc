component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Parse input into list of calorie totals per elf
        var elves = parseInput(inputText);

        // Solve both parts
        var part1Result = part1(elves);
        var part2Result = part2(elves);

        writeOutput("Part 1: " & part1Result & chr(10));
        writeOutput("Part 2: " & part2Result & chr(10));
    }

    function parseInput(inputText) {
        // Replace double newlines with a unique delimiter to split groups
        var groupDelimiter = "|||";
        var normalized = replace(inputText, chr(10) & chr(10), groupDelimiter, "all");
        var groups = listToArray(normalized, groupDelimiter);
        var elves = [];

        for (var group in groups) {
            var total = 0;
            // Split each group by single newlines to get individual calorie values
            var lines = listToArray(group, chr(10));
            for (var line in lines) {
                var trimmedLine = trim(line);
                if (len(trimmedLine) > 0) {
                    total += val(trimmedLine);
                }
            }
            arrayAppend(elves, total);
        }

        return elves;
    }

    function part1(elves) {
        // Find the Elf carrying the most Calories
        var maxCalories = 0;
        for (var calories in elves) {
            if (calories > maxCalories) {
                maxCalories = calories;
            }
        }
        return maxCalories;
    }

    function part2(elves) {
        // Find total calories carried by top three Elves
        // Sort in descending order
        arraySort(elves, "numeric", "desc");
        // Sum top 3
        return elves[1] + elves[2] + elves[3];
    }
}
