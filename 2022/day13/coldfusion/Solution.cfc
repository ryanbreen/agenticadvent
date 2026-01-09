component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Solve both parts
        var part1Result = part1(inputText);
        var part2Result = part2(inputText);

        systemOutput("Part 1: " & part1Result, true);
        systemOutput("Part 2: " & part2Result, true);
    }

    /**
     * Compare two values recursively.
     * Returns: -1 if left < right (correct order)
     *           1 if left > right (wrong order)
     *           0 if equal (continue)
     */
    function comparePackets(required any left, required any right) {
        var leftIsArray = isArray(arguments.left);
        var rightIsArray = isArray(arguments.right);

        // Both integers
        if (!leftIsArray && !rightIsArray) {
            if (arguments.left < arguments.right) {
                return -1;
            } else if (arguments.left > arguments.right) {
                return 1;
            }
            return 0;
        }

        // Both arrays
        if (leftIsArray && rightIsArray) {
            var minLen = min(arrayLen(arguments.left), arrayLen(arguments.right));

            for (var i = 1; i <= minLen; i++) {
                var result = comparePackets(arguments.left[i], arguments.right[i]);
                if (result != 0) {
                    return result;
                }
            }

            // Check lengths
            if (arrayLen(arguments.left) < arrayLen(arguments.right)) {
                return -1;
            } else if (arrayLen(arguments.left) > arrayLen(arguments.right)) {
                return 1;
            }
            return 0;
        }

        // Mixed types - convert integer to array
        if (!leftIsArray) {
            return comparePackets([arguments.left], arguments.right);
        } else {
            return comparePackets(arguments.left, [arguments.right]);
        }
    }

    /**
     * Part 1: Sum indices of pairs in correct order
     */
    function part1(required string input) {
        // Parse all non-empty lines
        var lines = listToArray(arguments.input, chr(10));
        var packets = [];

        for (var line in lines) {
            line = trim(line);
            if (len(line) > 0) {
                arrayAppend(packets, deserializeJSON(line));
            }
        }

        // Process pairs
        var total = 0;
        var pairIndex = 1;

        for (var i = 1; i <= arrayLen(packets); i += 2) {
            var left = packets[i];
            var right = packets[i + 1];

            if (comparePackets(left, right) == -1) {
                total += pairIndex;
            }
            pairIndex++;
        }

        return total;
    }

    /**
     * Part 2: Sort all packets with divider packets, find decoder key
     */
    function part2(required string input) {
        // Get all non-empty lines as packets
        var lines = listToArray(arguments.input, chr(10));
        var packets = [];

        for (var line in lines) {
            line = trim(line);
            if (len(line) > 0) {
                arrayAppend(packets, deserializeJSON(line));
            }
        }

        // Add divider packets
        var divider1 = [[2]];
        var divider2 = [[6]];
        arrayAppend(packets, divider1);
        arrayAppend(packets, divider2);

        // Sort using comparison function (bubble sort for simplicity)
        var n = arrayLen(packets);
        for (var i = 1; i < n; i++) {
            for (var j = 1; j <= n - i; j++) {
                if (comparePackets(packets[j], packets[j + 1]) == 1) {
                    // Swap
                    var temp = packets[j];
                    packets[j] = packets[j + 1];
                    packets[j + 1] = temp;
                }
            }
        }

        // Find positions of dividers (1-indexed)
        var pos1 = 0;
        var pos2 = 0;

        for (var i = 1; i <= arrayLen(packets); i++) {
            // Check if this is divider1: [[2]]
            if (isDivider(packets[i], 2)) {
                pos1 = i;
            }
            // Check if this is divider2: [[6]]
            if (isDivider(packets[i], 6)) {
                pos2 = i;
            }
        }

        return pos1 * pos2;
    }

    /**
     * Check if a packet is a divider packet [[n]]
     */
    function isDivider(required any packet, required numeric value) {
        if (!isArray(arguments.packet)) return false;
        if (arrayLen(arguments.packet) != 1) return false;

        var inner = arguments.packet[1];
        if (!isArray(inner)) return false;
        if (arrayLen(inner) != 1) return false;

        // Check that inner[1] is a numeric value, not an array
        if (isArray(inner[1])) return false;

        return inner[1] == arguments.value;
    }

}
