import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Solution {

    private Map<String, Long> memo;
    private String pattern;
    private int[] groups;

    public Solution() {
        this.memo = new HashMap<>();
    }

    private long dp(int pos, int groupIdx, int currentRun) {
        // Create memoization key
        String key = pos + "," + groupIdx + "," + currentRun;
        if (memo.containsKey(key)) {
            return memo.get(key);
        }

        // Base case: reached end of pattern
        if (pos == pattern.length()) {
            // Valid if we've matched all groups and no partial run
            if (groupIdx == groups.length && currentRun == 0) {
                return 1;
            }
            // Or if we're on the last group and the run matches
            if (groupIdx == groups.length - 1 && groups[groupIdx] == currentRun) {
                return 1;
            }
            return 0;
        }

        long result = 0;
        char c = pattern.charAt(pos);

        // Option 1: Place operational spring (.)
        if (c == '.' || c == '?') {
            if (currentRun == 0) {
                // No active run, just move forward
                result += dp(pos + 1, groupIdx, 0);
            } else if (groupIdx < groups.length && groups[groupIdx] == currentRun) {
                // End current run if it matches expected group size
                result += dp(pos + 1, groupIdx + 1, 0);
            }
            // Otherwise invalid (run doesn't match group)
        }

        // Option 2: Place damaged spring (#)
        if (c == '#' || c == '?') {
            if (groupIdx < groups.length && currentRun < groups[groupIdx]) {
                // Can extend current run
                result += dp(pos + 1, groupIdx, currentRun + 1);
            }
            // Otherwise invalid (exceeds group size or no more groups)
        }

        memo.put(key, result);
        return result;
    }

    public long countArrangements(String pattern, int[] groups) {
        this.memo.clear();
        this.pattern = pattern;
        this.groups = groups;
        return dp(0, 0, 0);
    }

    private static int[] parseGroups(String groupStr) {
        String[] parts = groupStr.split(",");
        int[] groups = new int[parts.length];
        for (int i = 0; i < parts.length; i++) {
            groups[i] = Integer.parseInt(parts[i]);
        }
        return groups;
    }

    private static String unfoldPattern(String pattern, int times) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < times; i++) {
            if (i > 0) sb.append('?');
            sb.append(pattern);
        }
        return sb.toString();
    }

    private static int[] unfoldGroups(int[] groups, int times) {
        int[] unfolded = new int[groups.length * times];
        for (int i = 0; i < times; i++) {
            System.arraycopy(groups, 0, unfolded, i * groups.length, groups.length);
        }
        return unfolded;
    }

    public static void main(String[] args) throws IOException {
        List<String[]> records = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(new FileReader("../input.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;
                String[] parts = line.split("\\s+");
                records.add(parts);
            }
        }

        Solution solver = new Solution();

        // Part 1
        long part1 = 0;
        for (String[] record : records) {
            String pattern = record[0];
            int[] groups = parseGroups(record[1]);
            part1 += solver.countArrangements(pattern, groups);
        }
        System.out.println("Part 1: " + part1);

        // Part 2
        long part2 = 0;
        for (String[] record : records) {
            String pattern = unfoldPattern(record[0], 5);
            int[] groups = unfoldGroups(parseGroups(record[1]), 5);
            part2 += solver.countArrangements(pattern, groups);
        }
        System.out.println("Part 2: " + part2);
    }
}
