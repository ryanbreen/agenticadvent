import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Day 25: Code Chronicle - Lock and key matching
 */
public class Solution {

    static class ParseResult {
        List<int[]> locks;
        List<int[]> keys;

        ParseResult(List<int[]> locks, List<int[]> keys) {
            this.locks = locks;
            this.keys = keys;
        }
    }

    /**
     * Parse schematics into locks and keys.
     */
    static ParseResult parseInput(String text) {
        List<int[]> locks = new ArrayList<>();
        List<int[]> keys = new ArrayList<>();

        String[] schematics = text.trim().split("\n\n");

        for (String schematic : schematics) {
            String[] lines = schematic.trim().split("\n");

            // Lock: top row is all #, bottom is all .
            // Key: top row is all ., bottom is all #
            if (lines[0].equals("#####")) {
                // It's a lock - count # from top (excluding top row)
                int[] heights = new int[5];
                for (int col = 0; col < 5; col++) {
                    int height = 0;
                    for (int row = 1; row < 7; row++) {  // rows 1-6
                        if (lines[row].charAt(col) == '#') {
                            height++;
                        } else {
                            break;
                        }
                    }
                    heights[col] = height;
                }
                locks.add(heights);
            } else {
                // It's a key - count # from bottom (excluding bottom row)
                int[] heights = new int[5];
                for (int col = 0; col < 5; col++) {
                    int height = 0;
                    for (int row = 5; row >= 0; row--) {  // rows 5 down to 0
                        if (lines[row].charAt(col) == '#') {
                            height++;
                        } else {
                            break;
                        }
                    }
                    heights[col] = height;
                }
                keys.add(heights);
            }
        }

        return new ParseResult(locks, keys);
    }

    /**
     * Check if a key fits a lock (no column exceeds 5).
     */
    static boolean fits(int[] lock, int[] key) {
        for (int i = 0; i < 5; i++) {
            if (lock[i] + key[i] > 5) {
                return false;
            }
        }
        return true;
    }

    /**
     * Count unique lock/key pairs that fit together.
     */
    static int part1(List<int[]> locks, List<int[]> keys) {
        int count = 0;
        for (int[] lock : locks) {
            for (int[] key : keys) {
                if (fits(lock, key)) {
                    count++;
                }
            }
        }
        return count;
    }

    public static void main(String[] args) throws IOException {
        Path inputFile = Path.of("../input.txt");
        String text = Files.readString(inputFile);

        ParseResult result = parseInput(text);

        int answer1 = part1(result.locks, result.keys);
        System.out.println("Part 1: " + answer1);

        // Day 25 typically only has Part 1
        System.out.println("Part 2: Merry Christmas!");
    }
}
