import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Solution {

    static class Range implements Comparable<Range> {
        long start;
        long end;

        Range(long start, long end) {
            this.start = start;
            this.end = end;
        }

        @Override
        public int compareTo(Range other) {
            return Long.compare(this.start, other.start);
        }
    }

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt")).strip();
        String[] lines = input.split("\n");

        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: " + part2(lines));
    }

    static int part1(String[] lines) {
        // Find the blank line separator
        int blankIdx = -1;
        for (int i = 0; i < lines.length; i++) {
            if (lines[i].isEmpty()) {
                blankIdx = i;
                break;
            }
        }

        // Parse ranges from the first section
        List<Range> ranges = new ArrayList<>();
        for (int i = 0; i < blankIdx; i++) {
            String[] parts = lines[i].split("-");
            long start = Long.parseLong(parts[0]);
            long end = Long.parseLong(parts[1]);
            ranges.add(new Range(start, end));
        }

        // Parse ingredient IDs from the second section
        List<Long> ingredientIds = new ArrayList<>();
        for (int i = blankIdx + 1; i < lines.length; i++) {
            if (!lines[i].isEmpty()) {
                ingredientIds.add(Long.parseLong(lines[i]));
            }
        }

        // Count how many ingredient IDs fall within any range
        int freshCount = 0;
        for (long ingredientId : ingredientIds) {
            for (Range range : ranges) {
                if (range.start <= ingredientId && ingredientId <= range.end) {
                    freshCount++;
                    break; // Found a match, no need to check other ranges
                }
            }
        }

        return freshCount;
    }

    static long part2(String[] lines) {
        // Find the blank line separator
        int blankIdx = -1;
        for (int i = 0; i < lines.length; i++) {
            if (lines[i].isEmpty()) {
                blankIdx = i;
                break;
            }
        }

        // Parse ranges from the first section
        List<Range> ranges = new ArrayList<>();
        for (int i = 0; i < blankIdx; i++) {
            String[] parts = lines[i].split("-");
            long start = Long.parseLong(parts[0]);
            long end = Long.parseLong(parts[1]);
            ranges.add(new Range(start, end));
        }

        // Sort ranges by start position
        Collections.sort(ranges);

        // Merge overlapping ranges
        List<Range> merged = new ArrayList<>();
        for (Range range : ranges) {
            if (!merged.isEmpty() && range.start <= merged.get(merged.size() - 1).end + 1) {
                // Overlapping or adjacent - merge with the last range
                Range last = merged.get(merged.size() - 1);
                last.end = Math.max(last.end, range.end);
            } else {
                // No overlap - add as new range
                merged.add(new Range(range.start, range.end));
            }
        }

        // Count total unique IDs covered by merged ranges
        long totalCount = 0;
        for (Range range : merged) {
            totalCount += (range.end - range.start + 1);
        }

        return totalCount;
    }
}
