import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Advent of Code 2024 Day 9: Disk Fragmenter
 *
 * Compact a fragmented disk by moving file blocks to fill gaps.
 * Part 1: Move blocks one at a time from end to leftmost free space
 * Part 2: Move whole files to leftmost span that fits
 */
public class Solution {

    /**
     * Parse disk map into expanded block representation.
     * Returns list where each element is file ID or -1 for free space.
     */
    private static List<Integer> parseDiskMap(String filename) throws IOException {
        String diskMap = Files.readString(Paths.get(filename)).strip();

        List<Integer> blocks = new ArrayList<>();
        int fileId = 0;
        boolean isFile = true;

        for (char c : diskMap.toCharArray()) {
            int length = c - '0';
            if (isFile) {
                for (int i = 0; i < length; i++) {
                    blocks.add(fileId);
                }
                fileId++;
            } else {
                for (int i = 0; i < length; i++) {
                    blocks.add(-1);  // -1 represents free space
                }
            }
            isFile = !isFile;
        }

        return blocks;
    }

    /**
     * Compact disk by moving blocks one at a time from end to leftmost free space.
     */
    private static List<Integer> compactBlocks(List<Integer> blocks) {
        // Create a copy to avoid modifying original
        List<Integer> result = new ArrayList<>(blocks);
        int left = 0;
        int right = result.size() - 1;

        while (left < right) {
            // Find leftmost free space
            while (left < right && result.get(left) != -1) {
                left++;
            }
            // Find rightmost file block
            while (left < right && result.get(right) == -1) {
                right--;
            }

            if (left < right) {
                // Swap
                result.set(left, result.get(right));
                result.set(right, -1);
                left++;
                right--;
            }
        }

        return result;
    }

    /**
     * Calculate filesystem checksum: sum of position * file_id for each block.
     */
    private static long calculateChecksum(List<Integer> blocks) {
        long checksum = 0;
        for (int pos = 0; pos < blocks.size(); pos++) {
            int fileId = blocks.get(pos);
            if (fileId != -1) {
                checksum += (long) pos * fileId;
            }
        }
        return checksum;
    }

    /**
     * Part 1: Compact by moving individual blocks, return checksum.
     */
    private static long part1() throws IOException {
        List<Integer> blocks = parseDiskMap("../input.txt");
        List<Integer> compacted = compactBlocks(blocks);
        return calculateChecksum(compacted);
    }

    /**
     * Part 2: Compact by moving whole files (highest ID first), return checksum.
     */
    private static long part2() throws IOException {
        List<Integer> blocks = parseDiskMap("../input.txt");

        // Find all files: file_id -> [start_pos, length]
        Map<Integer, int[]> files = new HashMap<>();
        int i = 0;
        while (i < blocks.size()) {
            if (blocks.get(i) != -1) {
                int fileId = blocks.get(i);
                int start = i;
                while (i < blocks.size() && blocks.get(i) == fileId) {
                    i++;
                }
                files.put(fileId, new int[]{start, i - start});
            } else {
                i++;
            }
        }

        // Process files in decreasing order of file ID
        int maxFileId = files.keySet().stream().max(Integer::compareTo).orElse(0);

        for (int fileId = maxFileId; fileId >= 0; fileId--) {
            int[] fileInfo = files.get(fileId);
            int start = fileInfo[0];
            int length = fileInfo[1];

            // Find leftmost span of free space that fits this file
            // Must be to the left of current position
            Integer freeStart = null;
            i = 0;
            while (i < start) {
                if (blocks.get(i) == -1) {
                    // Count consecutive free blocks
                    int spanStart = i;
                    int spanLength = 0;
                    while (i < start && blocks.get(i) == -1) {
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
            if (freeStart != null) {
                // Clear old position
                for (int j = start; j < start + length; j++) {
                    blocks.set(j, -1);
                }
                // Write to new position
                for (int j = freeStart; j < freeStart + length; j++) {
                    blocks.set(j, fileId);
                }
                // Update file position
                files.put(fileId, new int[]{freeStart, length});
            }
        }

        return calculateChecksum(blocks);
    }

    public static void main(String[] args) {
        try {
            System.out.println("Part 1: " + part1());
            System.out.println("Part 2: " + part2());
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
            System.exit(1);
        }
    }
}
