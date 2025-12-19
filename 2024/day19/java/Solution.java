import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {
    private static String[] patterns;

    public static void main(String[] args) throws IOException {
        String content = Files.readString(Path.of("../input.txt")).trim();
        String[] parts = content.split("\n\n");

        // Parse patterns (comma-separated on first line)
        patterns = Arrays.stream(parts[0].split(","))
                         .map(String::trim)
                         .toArray(String[]::new);

        // Parse designs (one per line after blank line)
        String[] designs = parts[1].trim().split("\n");

        // Part 1: Count designs that can be formed
        long possibleCount = 0;
        for (String design : designs) {
            if (canForm(design)) {
                possibleCount++;
            }
        }
        System.out.println("Part 1: " + possibleCount);

        // Part 2: Sum the number of ways each design can be formed
        long totalWays = 0;
        for (String design : designs) {
            totalWays += countWays(design);
        }
        System.out.println("Part 2: " + totalWays);
    }

    /**
     * Check if design can be formed by concatenating patterns.
     * Uses dynamic programming with memoization.
     */
    private static boolean canForm(String design) {
        Map<Integer, Boolean> memo = new HashMap<>();
        return dpCanForm(design, 0, memo);
    }

    private static boolean dpCanForm(String design, int pos, Map<Integer, Boolean> memo) {
        if (pos == design.length()) {
            return true;
        }

        if (memo.containsKey(pos)) {
            return memo.get(pos);
        }

        for (String pattern : patterns) {
            int plen = pattern.length();
            if (pos + plen <= design.length() &&
                design.substring(pos, pos + plen).equals(pattern)) {
                if (dpCanForm(design, pos + plen, memo)) {
                    memo.put(pos, true);
                    return true;
                }
            }
        }

        memo.put(pos, false);
        return false;
    }

    /**
     * Count number of ways to form design from patterns.
     * Uses dynamic programming with memoization.
     */
    private static long countWays(String design) {
        Map<Integer, Long> memo = new HashMap<>();
        return dpCountWays(design, 0, memo);
    }

    private static long dpCountWays(String design, int pos, Map<Integer, Long> memo) {
        if (pos == design.length()) {
            return 1;
        }

        if (memo.containsKey(pos)) {
            return memo.get(pos);
        }

        long total = 0;
        for (String pattern : patterns) {
            int plen = pattern.length();
            if (pos + plen <= design.length() &&
                design.substring(pos, pos + plen).equals(pattern)) {
                total += dpCountWays(design, pos + plen, memo);
            }
        }

        memo.put(pos, total);
        return total;
    }
}
