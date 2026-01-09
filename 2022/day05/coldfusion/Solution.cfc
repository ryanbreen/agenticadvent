component {

    /**
     * Parse input file into stacks and moves
     * Returns struct with 'stacks' array of arrays and 'moves' array of structs
     */
    public struct function parseInput(required string filepath) {
        var content = fileRead(arguments.filepath);

        // Split on blank line (double newline)
        var blankLinePos = find(chr(10) & chr(10), content);
        var stackPart = left(content, blankLinePos - 1);
        var movePart = mid(content, blankLinePos + 2, len(content));

        var stackLines = listToArray(stackPart, chr(10));
        var moveLines = listToArray(trim(movePart), chr(10));

        // Find number of stacks from the last line (the numbers line)
        var numLine = trim(stackLines[arrayLen(stackLines)]);
        var nums = listToArray(numLine, " ");
        var numStacks = arrayLen(nums);

        // Initialize stacks as arrays
        var stacks = [];
        for (var i = 1; i <= numStacks; i++) {
            arrayAppend(stacks, []);
        }

        // Parse stack lines (all except the last number line), from top to bottom
        for (var lineIdx = 1; lineIdx < arrayLen(stackLines); lineIdx++) {
            var line = stackLines[lineIdx];
            for (var i = 1; i <= numStacks; i++) {
                var pos = 2 + (i - 1) * 4; // Position of crate letter (1-indexed: 2, 6, 10, ...)
                if (pos <= len(line)) {
                    var ch = mid(line, pos, 1);
                    if (ch != " ") {
                        // Insert at beginning since we're reading top-down but want bottom at index 1
                        arrayPrepend(stacks[i], ch);
                    }
                }
            }
        }

        // Parse moves
        var moves = [];
        for (var moveLine in moveLines) {
            moveLine = trim(moveLine);
            if (len(moveLine) == 0) continue;

            // Parse "move N from X to Y"
            var match = reMatch("move (\d+) from (\d+) to (\d+)", moveLine);
            if (arrayLen(match) > 0) {
                var nums = reMatch("\d+", moveLine);
                arrayAppend(moves, {
                    count: val(nums[1]),
                    fromStack: val(nums[2]),
                    toStack: val(nums[3])
                });
            }
        }

        return {
            stacks: stacks,
            moves: moves
        };
    }

    /**
     * Deep copy an array of arrays
     */
    private array function copyStacks(required array stacks) {
        var copy = [];
        for (var stack in arguments.stacks) {
            arrayAppend(copy, duplicate(stack));
        }
        return copy;
    }

    /**
     * Get top crate from each stack concatenated
     */
    private string function getTops(required array stacks) {
        var result = "";
        for (var stack in arguments.stacks) {
            if (arrayLen(stack) > 0) {
                result &= stack[arrayLen(stack)];
            }
        }
        return result;
    }

    /**
     * Part 1: Move crates one at a time (reverses order)
     */
    public string function part1(required array stacks, required array moves) {
        var workStacks = copyStacks(arguments.stacks);

        for (var move in arguments.moves) {
            for (var i = 1; i <= move.count; i++) {
                if (arrayLen(workStacks[move.fromStack]) > 0) {
                    var crate = workStacks[move.fromStack][arrayLen(workStacks[move.fromStack])];
                    arrayDeleteAt(workStacks[move.fromStack], arrayLen(workStacks[move.fromStack]));
                    arrayAppend(workStacks[move.toStack], crate);
                }
            }
        }

        return getTops(workStacks);
    }

    /**
     * Part 2: Move multiple crates at once (preserves order)
     */
    public string function part2(required array stacks, required array moves) {
        var workStacks = copyStacks(arguments.stacks);

        for (var move in arguments.moves) {
            var fromLen = arrayLen(workStacks[move.fromStack]);
            var startIdx = fromLen - move.count + 1;

            // Get the crates to move (preserving order)
            var crates = [];
            for (var i = startIdx; i <= fromLen; i++) {
                arrayAppend(crates, workStacks[move.fromStack][i]);
            }

            // Remove from source (from end backwards)
            for (var i = 1; i <= move.count; i++) {
                arrayDeleteAt(workStacks[move.fromStack], arrayLen(workStacks[move.fromStack]));
            }

            // Add to destination (preserving order)
            for (var crate in crates) {
                arrayAppend(workStacks[move.toStack], crate);
            }
        }

        return getTops(workStacks);
    }

}
