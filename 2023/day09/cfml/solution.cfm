<cfscript>
try {
    // Locate and validate input file
    currentDir = getDirectoryFromPath(getCurrentTemplatePath());
    inputPath = currentDir & "../input.txt";

    // Validate file exists before attempting to read
    if (!fileExists(inputPath)) {
        echo("Error: Input file not found at " & inputPath & chr(10));
        abort;
    }

    // Read and parse input file into lines
    inputText = fileRead(inputPath).trim();
    lines = listToArray(inputText, chr(10));

    /**
     * Parse input lines into arrays of integers
     * Each line contains space-separated integers
     *
     * @return {array} Array of arrays of integers (histories)
     */
    function parseInput() {
        var histories = [];
        for (var line in lines) {
            var nums = [];
            var parts = listToArray(line, " ");
            for (var part in parts) {
                if (len(trim(part)) > 0) {
                    arrayAppend(nums, val(part));
                }
            }
            if (arrayLen(nums) > 0) {
                arrayAppend(histories, nums);
            }
        }
        return histories;
    }

    /**
     * Compute differences between consecutive elements
     *
     * @param {array} seq - Array of numbers
     * @return {array} Array of differences (length = input length - 1)
     */
    function getDifferences(seq) {
        var result = [];
        var n = arrayLen(seq);
        for (var i = 1; i < n; i++) {
            arrayAppend(result, seq[i + 1] - seq[i]);
        }
        return result;
    }

    /**
     * Check if all elements in array are zero
     *
     * @param {array} seq - Array of numbers
     * @return {boolean} True if all elements are zero
     */
    function allZeros(seq) {
        for (var x in seq) {
            if (x != 0) {
                return false;
            }
        }
        return true;
    }

    /**
     * Extrapolate the next value in the sequence
     * Build difference pyramid until all zeros, then propagate back up
     *
     * @param {array} seq - Original sequence
     * @return {numeric} The extrapolated next value
     */
    function extrapolateNext(seq) {
        // Build list of sequences (difference pyramid)
        var sequences = [duplicate(seq)];
        var current = seq;

        // Keep computing differences until we hit all zeros
        while (!allZeros(current)) {
            current = getDifferences(current);
            arrayAppend(sequences, current);
        }

        // Propagate back up: add last value of lower level to last value of current level
        for (var i = arrayLen(sequences) - 1; i >= 1; i--) {
            var lastOfLower = sequences[i + 1][arrayLen(sequences[i + 1])];
            var lastOfCurrent = sequences[i][arrayLen(sequences[i])];
            arrayAppend(sequences[i], lastOfCurrent + lastOfLower);
        }

        // Return the new last value of the original sequence
        return sequences[1][arrayLen(sequences[1])];
    }

    /**
     * Extrapolate the previous value in the sequence
     * Build difference pyramid until all zeros, then propagate back up
     *
     * @param {array} seq - Original sequence
     * @return {numeric} The extrapolated previous value
     */
    function extrapolatePrev(seq) {
        // Build list of sequences (difference pyramid)
        var sequences = [duplicate(seq)];
        var current = seq;

        // Keep computing differences until we hit all zeros
        while (!allZeros(current)) {
            current = getDifferences(current);
            arrayAppend(sequences, current);
        }

        // Propagate back up: subtract first value of lower level from first value of current level
        for (var i = arrayLen(sequences) - 1; i >= 1; i--) {
            var firstOfLower = sequences[i + 1][1];
            var firstOfCurrent = sequences[i][1];
            arrayPrepend(sequences[i], firstOfCurrent - firstOfLower);
        }

        // Return the new first value of the original sequence
        return sequences[1][1];
    }

    /**
     * Part 1: Sum of all extrapolated next values
     *
     * @return {numeric} Sum of extrapolated next values
     */
    function part1() {
        var histories = parseInput();
        var total = 0;
        for (var h in histories) {
            total += extrapolateNext(h);
        }
        return total;
    }

    /**
     * Part 2: Sum of all extrapolated previous values
     *
     * @return {numeric} Sum of extrapolated previous values
     */
    function part2() {
        var histories = parseInput();
        var total = 0;
        for (var h in histories) {
            total += extrapolatePrev(h);
        }
        return total;
    }

    // Execute both parts and display results
    echo("Part 1: " & part1() & chr(10));
    echo("Part 2: " & part2() & chr(10));

} catch (any e) {
    // Graceful error handling with detailed error information
    echo("Error: " & e.message & chr(10));
    if (structKeyExists(e, "detail")) {
        echo("Detail: " & e.detail & chr(10));
    }
    abort;
}
</cfscript>
