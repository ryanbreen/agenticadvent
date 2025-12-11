#!/usr/bin/env php
<?php
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
function parseDiskMap($filename) {
    $diskMap = trim(file_get_contents($filename));

    $blocks = [];
    $fileId = 0;
    $isFile = true;

    for ($i = 0; $i < strlen($diskMap); $i++) {
        $length = intval($diskMap[$i]);
        if ($isFile) {
            for ($j = 0; $j < $length; $j++) {
                $blocks[] = $fileId;
            }
            $fileId++;
        } else {
            for ($j = 0; $j < $length; $j++) {
                $blocks[] = -1;  // -1 represents free space
            }
        }
        $isFile = !$isFile;
    }

    return $blocks;
}

/**
 * Compact disk by moving blocks one at a time from end to leftmost free space.
 */
function compactBlocks($blocks) {
    $blocks = $blocks;  // Copy array
    $left = 0;
    $right = count($blocks) - 1;

    while ($left < $right) {
        // Find leftmost free space
        while ($left < $right && $blocks[$left] != -1) {
            $left++;
        }
        // Find rightmost file block
        while ($left < $right && $blocks[$right] == -1) {
            $right--;
        }

        if ($left < $right) {
            // Swap
            $blocks[$left] = $blocks[$right];
            $blocks[$right] = -1;
            $left++;
            $right--;
        }
    }

    return $blocks;
}

/**
 * Calculate filesystem checksum: sum of position * file_id for each block.
 */
function calculateChecksum($blocks) {
    $checksum = 0;
    for ($pos = 0; $pos < count($blocks); $pos++) {
        if ($blocks[$pos] != -1) {
            $checksum += $pos * $blocks[$pos];
        }
    }
    return $checksum;
}

/**
 * Part 1: Compact by moving individual blocks, return checksum.
 */
function part1() {
    $blocks = parseDiskMap('../input.txt');
    $compacted = compactBlocks($blocks);
    return calculateChecksum($compacted);
}

/**
 * Part 2: Compact by moving whole files (highest ID first), return checksum.
 */
function part2() {
    $blocks = parseDiskMap('../input.txt');

    // Find all files: file_id -> [start_pos, length]
    $files = [];
    $i = 0;
    while ($i < count($blocks)) {
        if ($blocks[$i] != -1) {
            $fileId = $blocks[$i];
            $start = $i;
            while ($i < count($blocks) && $blocks[$i] == $fileId) {
                $i++;
            }
            $files[$fileId] = [$start, $i - $start];
        } else {
            $i++;
        }
    }

    // Process files in decreasing order of file ID
    $maxFileId = max(array_keys($files));

    for ($fileId = $maxFileId; $fileId >= 0; $fileId--) {
        list($start, $length) = $files[$fileId];

        // Find leftmost span of free space that fits this file
        // Must be to the left of current position
        $freeStart = null;
        $i = 0;
        while ($i < $start) {
            if ($blocks[$i] == -1) {
                // Count consecutive free blocks
                $spanStart = $i;
                $spanLength = 0;
                while ($i < $start && $blocks[$i] == -1) {
                    $spanLength++;
                    $i++;
                }
                if ($spanLength >= $length) {
                    $freeStart = $spanStart;
                    break;
                }
            } else {
                $i++;
            }
        }

        // Move file if we found a suitable span
        if ($freeStart !== null) {
            // Clear old position
            for ($j = $start; $j < $start + $length; $j++) {
                $blocks[$j] = -1;
            }
            // Write to new position
            for ($j = $freeStart; $j < $freeStart + $length; $j++) {
                $blocks[$j] = $fileId;
            }
            // Update file position
            $files[$fileId] = [$freeStart, $length];
        }
    }

    return calculateChecksum($blocks);
}

// Main execution
echo "Part 1: " . part1() . "\n";
echo "Part 2: " . part2() . "\n";
