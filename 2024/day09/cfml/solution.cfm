<cfscript>
/**
 * Advent of Code 2024 Day 9: Disk Fragmenter
 *
 * Compact a fragmented disk by moving file blocks to fill gaps.
 * Part 1: Move blocks one at a time from end to leftmost free space
 * Part 2: Move whole files to leftmost span that fits
 */

/**
 * Parse disk map into expanded block representation.
 * Returns array where each element is file ID or -1 for free space.
 */
function parseDiskMap(filename) {
    var diskMap = fileRead(filename).trim();
    var blocks = [];
    var fileId = 0;
    var isFile = true;

    for (var i = 1; i <= len(diskMap); i++) {
        var digit = mid(diskMap, i, 1);
        var length = val(digit);

        if (isFile) {
            for (var j = 1; j <= length; j++) {
                arrayAppend(blocks, fileId);
            }
            fileId++;
        } else {
            for (var j = 1; j <= length; j++) {
                arrayAppend(blocks, -1);  // -1 represents free space
            }
        }
        isFile = !isFile;
    }

    return blocks;
}

/**
 * Compact disk by moving blocks one at a time from end to leftmost free space.
 */
function compactBlocks(blocksParam) {
    var blocks = duplicate(blocksParam);
    var left = 1;
    var right = arrayLen(blocks);

    while (left < right) {
        // Find leftmost free space
        while (left < right && blocks[left] != -1) {
            left++;
        }
        // Find rightmost file block
        while (left < right && blocks[right] == -1) {
            right--;
        }

        if (left < right) {
            // Swap
            blocks[left] = blocks[right];
            blocks[right] = -1;
            left++;
            right--;
        }
    }

    return blocks;
}

/**
 * Calculate filesystem checksum: sum of position * file_id for each block.
 */
function calculateChecksum(blocks) {
    var checksum = 0;

    for (var pos = 1; pos <= arrayLen(blocks); pos++) {
        var fileId = blocks[pos];
        if (fileId != -1) {
            // Position is 0-indexed for checksum calculation
            checksum += (pos - 1) * fileId;
        }
    }

    return checksum;
}

/**
 * Compact by moving individual blocks, return checksum.
 */
function part1() {
    var blocks = parseDiskMap('../input.txt');
    var compacted = compactBlocks(blocks);
    return calculateChecksum(compacted);
}

/**
 * Compact by moving whole files (highest ID first), return checksum.
 */
function part2() {
    var blocks = parseDiskMap('../input.txt');

    // Find all files: file_id -> {start: pos, length: len}
    var files = {};
    var i = 1;

    while (i <= arrayLen(blocks)) {
        if (blocks[i] != -1) {
            var fileId = blocks[i];
            var start = i;
            while (i <= arrayLen(blocks) && blocks[i] == fileId) {
                i++;
            }
            files[fileId] = {
                start: start,
                length: i - start
            };
        } else {
            i++;
        }
    }

    // Find max file ID
    var maxFileId = 0;
    for (var key in files) {
        if (val(key) > maxFileId) {
            maxFileId = val(key);
        }
    }

    // Process files in decreasing order of file ID
    for (var fileId = maxFileId; fileId >= 0; fileId--) {
        if (!structKeyExists(files, fileId)) {
            continue;
        }

        var fileInfo = files[fileId];
        var start = fileInfo.start;
        var length = fileInfo.length;

        // Find leftmost span of free space that fits this file
        // Must be to the left of current position
        var freeStart = 0;
        var i = 1;

        while (i < start) {
            if (blocks[i] == -1) {
                // Count consecutive free blocks
                var spanStart = i;
                var spanLength = 0;
                while (i < start && blocks[i] == -1) {
                    spanLength++;
                    i++;
                }
                if (spanLength >= length) {
                    freeStart = spanStart;
                    break;
                }
            } else {
                i++;
            }
        }

        // Move file if we found a suitable span
        if (freeStart > 0) {
            // Clear old position
            for (var j = start; j < start + length; j++) {
                blocks[j] = -1;
            }
            // Write to new position
            for (var j = freeStart; j < freeStart + length; j++) {
                blocks[j] = fileId;
            }
            // Update file position
            files[fileId] = {
                start: freeStart,
                length: length
            };
        }
    }

    return calculateChecksum(blocks);
}

// Main execution
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
