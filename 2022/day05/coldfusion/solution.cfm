<cfscript>
    solution = new Solution();

    scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
    inputFile = scriptDir & "../input.txt";

    parsed = solution.parseInput(inputFile);

    writeOutput("Part 1: " & solution.part1(parsed.stacks, parsed.moves) & chr(10));
    writeOutput("Part 2: " & solution.part2(parsed.stacks, parsed.moves) & chr(10));
</cfscript>
