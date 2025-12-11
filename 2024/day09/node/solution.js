import { readFileSync } from 'fs';

/**
 * Parse disk map into expanded block representation.
 * @param {string} filename - Path to input file
 * @returns {number[]} Array where each element is file ID or -1 for free space
 */
function parseDiskMap(filename) {
    const diskMap = readFileSync(filename, 'utf-8').trim();
    const blocks = [];
    let fileId = 0;
    let isFile = true;

    for (const digit of diskMap) {
        const length = parseInt(digit);
        if (isFile) {
            for (let i = 0; i < length; i++) {
                blocks.push(fileId);
            }
            fileId++;
        } else {
            for (let i = 0; i < length; i++) {
                blocks.push(-1);  // -1 represents free space
            }
        }
        isFile = !isFile;
    }

    return blocks;
}

/**
 * Compact disk by moving blocks one at a time from end to leftmost free space.
 * @param {number[]} blocks - Disk blocks
 * @returns {number[]} Compacted blocks
 */
function compactBlocks(blocks) {
    blocks = [...blocks];  // Copy
    let left = 0;
    let right = blocks.length - 1;

    while (left < right) {
        // Find leftmost free space
        while (left < right && blocks[left] !== -1) {
            left++;
        }
        // Find rightmost file block
        while (left < right && blocks[right] === -1) {
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
 * @param {number[]} blocks - Disk blocks
 * @returns {number} Checksum
 */
function calculateChecksum(blocks) {
    let checksum = 0;
    for (let pos = 0; pos < blocks.length; pos++) {
        if (blocks[pos] !== -1) {
            checksum += pos * blocks[pos];
        }
    }
    return checksum;
}

/**
 * Part 1: Compact by moving individual blocks, return checksum.
 */
function part1() {
    const blocks = parseDiskMap('../input.txt');
    const compacted = compactBlocks(blocks);
    return calculateChecksum(compacted);
}

/**
 * Part 2: Compact by moving whole files (highest ID first), return checksum.
 */
function part2() {
    const blocks = parseDiskMap('../input.txt');

    // Find all files: file_id -> {start, length}
    const files = new Map();
    let i = 0;
    while (i < blocks.length) {
        if (blocks[i] !== -1) {
            const fileId = blocks[i];
            const start = i;
            while (i < blocks.length && blocks[i] === fileId) {
                i++;
            }
            files.set(fileId, { start, length: i - start });
        } else {
            i++;
        }
    }

    // Process files in decreasing order of file ID
    const maxFileId = Math.max(...files.keys());

    for (let fileId = maxFileId; fileId >= 0; fileId--) {
        const file = files.get(fileId);
        if (!file) continue;

        const { start, length } = file;

        // Find leftmost span of free space that fits this file
        // Must be to the left of current position
        let freeStart = null;
        let j = 0;
        while (j < start) {
            if (blocks[j] === -1) {
                // Count consecutive free blocks
                const spanStart = j;
                let spanLength = 0;
                while (j < start && blocks[j] === -1) {
                    spanLength++;
                    j++;
                }
                if (spanLength >= length) {
                    freeStart = spanStart;
                    break;
                }
            } else {
                j++;
            }
        }

        // Move file if we found a suitable span
        if (freeStart !== null) {
            // Clear old position
            for (let k = start; k < start + length; k++) {
                blocks[k] = -1;
            }
            // Write to new position
            for (let k = freeStart; k < freeStart + length; k++) {
                blocks[k] = fileId;
            }
            // Update file position
            files.set(fileId, { start: freeStart, length });
        }
    }

    return calculateChecksum(blocks);
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
