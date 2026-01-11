<cfscript>
    solution = new Solution();

    // Get the directory where this script is located
    scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
    inputPath = scriptDir & "../input.txt";
    parsed = solution.parseInput(inputPath);

    part1Result = solution.part1(parsed.numbers, parsed.boards);
    part2Result = solution.part2(parsed.numbers, parsed.boards);

    writeOutput("Part 1: " & part1Result & chr(10));
    writeOutput("Part 2: " & part2Result & chr(10));
</cfscript>
