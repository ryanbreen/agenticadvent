component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();
        var lines = listToArray(inputText, chr(10));

        // Parse numbers
        var numbers = [];
        for (var line in lines) {
            arrayAppend(numbers, val(trim(line)));
        }

        // Solve both parts
        var part1Result = part1(numbers);
        var part2Result = part2(numbers);

        writeOutput("Part 1: " & part1Result & chr(10));
        writeOutput("Part 2: " & part2Result & chr(10));
    }

    function mix(numbers, times) {
        var n = arrayLen(numbers);

        // Store [original_index, value] pairs
        var indexed = [];
        for (var i = 1; i <= n; i++) {
            arrayAppend(indexed, {origIdx: i, val: numbers[i]});
        }

        for (var t = 1; t <= times; t++) {
            for (var origIdx = 1; origIdx <= n; origIdx++) {
                // Find current position of this element
                var currPos = 0;
                for (var i = 1; i <= arrayLen(indexed); i++) {
                    if (indexed[i].origIdx == origIdx) {
                        currPos = i;
                        break;
                    }
                }

                // Get the value
                var val = indexed[currPos].val;

                // Remove from current position
                arrayDeleteAt(indexed, currPos);

                // Calculate new position (modulo n-1 because we removed the element)
                // CFML arrays are 1-indexed, so we need to adjust
                var newPos = ((currPos - 1 + val) % (n - 1));
                if (newPos < 0) {
                    newPos = newPos + (n - 1);
                }
                newPos = newPos + 1; // Convert back to 1-indexed

                // Handle edge case: if newPos is 0 after modulo, it should be at the end
                if (newPos == 0) {
                    newPos = n - 1;
                }

                // Insert at new position
                arrayInsertAt(indexed, newPos, {origIdx: origIdx, val: val});
            }
        }

        // Extract values
        var result = [];
        for (var item in indexed) {
            arrayAppend(result, item.val);
        }
        return result;
    }

    function groveCoordinates(mixed) {
        var n = arrayLen(mixed);

        // Find zero index (1-indexed in CFML)
        var zeroIdx = 0;
        for (var i = 1; i <= n; i++) {
            if (mixed[i] == 0) {
                zeroIdx = i;
                break;
            }
        }

        // Get values at 1000, 2000, 3000 positions after 0
        var sum = 0;
        var offsets = [1000, 2000, 3000];
        for (var offset in offsets) {
            // Calculate position (1-indexed, wrapping)
            var pos = ((zeroIdx - 1 + offset) % n) + 1;
            sum += mixed[pos];
        }

        return sum;
    }

    function part1(numbers) {
        var mixed = mix(numbers, 1);
        return groveCoordinates(mixed);
    }

    function part2(numbers) {
        var decryptionKey = 811589153;

        // Multiply all numbers by the decryption key
        var scaledNumbers = [];
        for (var num in numbers) {
            arrayAppend(scaledNumbers, num * decryptionKey);
        }

        var mixed = mix(scaledNumbers, 10);
        return groveCoordinates(mixed);
    }
}
