import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;

public class Solution {

    record Pattern(List<String> lines) {
        int width() {
            return lines.isEmpty() ? 0 : lines.get(0).length();
        }

        int height() {
            return lines.size();
        }
    }

    public static List<Pattern> parseInput(String text) {
        return Arrays.stream(text.trim().split("\n\n"))
                .map(block -> new Pattern(Arrays.asList(block.split("\n"))))
                .toList();
    }

    private static int countDifferences(String s1, String s2) {
        int minLen = Math.min(s1.length(), s2.length());
        return (int) IntStream.range(0, minLen)
                .filter(i -> s1.charAt(i) != s2.charAt(i))
                .count();
    }

    private static int findVerticalReflection(Pattern pattern, int targetDiff) {
        if (pattern.lines().isEmpty()) {
            return 0;
        }

        for (int col = 1; col < pattern.width(); col++) {
            final int reflectionCol = col;
            int totalDiff = pattern.lines().stream()
                    .mapToInt(row -> {
                        String left = new StringBuilder(row.substring(0, reflectionCol)).reverse().toString();
                        String right = row.substring(reflectionCol);
                        int minLen = Math.min(left.length(), right.length());
                        return countDifferences(left.substring(0, minLen), right.substring(0, minLen));
                    })
                    .sum();

            if (totalDiff == targetDiff) {
                return col;
            }
        }

        return 0;
    }

    private static int findHorizontalReflection(Pattern pattern, int targetDiff) {
        if (pattern.lines().isEmpty()) {
            return 0;
        }

        for (int row = 1; row < pattern.height(); row++) {
            final int reflectionRow = row;
            int minLen = Math.min(reflectionRow, pattern.height() - reflectionRow);

            int totalDiff = IntStream.range(0, minLen)
                    .map(i -> countDifferences(
                            pattern.lines().get(reflectionRow - 1 - i),
                            pattern.lines().get(reflectionRow + i)))
                    .sum();

            if (totalDiff == targetDiff) {
                return row;
            }
        }

        return 0;
    }

    private static int summarizePattern(Pattern pattern, int targetDiff) {
        int vertical = findVerticalReflection(pattern, targetDiff);
        if (vertical > 0) {
            return vertical;
        }
        return findHorizontalReflection(pattern, targetDiff) * 100;
    }

    public static int part1(List<Pattern> patterns) {
        return patterns.stream()
                .mapToInt(p -> summarizePattern(p, 0))
                .sum();
    }

    public static int part2(List<Pattern> patterns) {
        return patterns.stream()
                .mapToInt(p -> summarizePattern(p, 1))
                .sum();
    }

    public static void main(String[] args) throws IOException {
        String text = Files.readString(Path.of("../input.txt"));
        List<Pattern> patterns = parseInput(text);

        System.out.println("Part 1: " + part1(patterns));
        System.out.println("Part 2: " + part2(patterns));
    }
}
