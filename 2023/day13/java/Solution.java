import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Solution {

    static class Pattern {
        List<String> lines;

        Pattern(List<String> lines) {
            this.lines = lines;
        }
    }

    public static List<Pattern> parseInput(String text) {
        List<Pattern> patterns = new ArrayList<>();
        String[] blocks = text.trim().split("\n\n");

        for (String block : blocks) {
            String[] lines = block.split("\n");
            List<String> lineList = new ArrayList<>();
            for (String line : lines) {
                lineList.add(line);
            }
            patterns.add(new Pattern(lineList));
        }

        return patterns;
    }

    public static int findVerticalReflection(Pattern pattern) {
        if (pattern.lines.isEmpty()) {
            return 0;
        }

        int width = pattern.lines.get(0).length();

        for (int col = 1; col < width; col++) {
            boolean isReflection = true;

            for (String row : pattern.lines) {
                // Get left side (reversed) and right side
                String left = new StringBuilder(row.substring(0, col)).reverse().toString();
                String right = row.substring(col);

                // Compare overlapping parts
                int minLen = Math.min(left.length(), right.length());
                if (!left.substring(0, minLen).equals(right.substring(0, minLen))) {
                    isReflection = false;
                    break;
                }
            }

            if (isReflection) {
                return col;
            }
        }

        return 0;
    }

    public static int findHorizontalReflection(Pattern pattern) {
        if (pattern.lines.isEmpty()) {
            return 0;
        }

        int height = pattern.lines.size();

        for (int row = 1; row < height; row++) {
            boolean isReflection = true;

            // Get top (reversed) and bottom
            List<String> top = new ArrayList<>();
            for (int i = row - 1; i >= 0; i--) {
                top.add(pattern.lines.get(i));
            }

            List<String> bottom = new ArrayList<>();
            for (int i = row; i < height; i++) {
                bottom.add(pattern.lines.get(i));
            }

            int minLen = Math.min(top.size(), bottom.size());
            for (int i = 0; i < minLen; i++) {
                if (!top.get(i).equals(bottom.get(i))) {
                    isReflection = false;
                    break;
                }
            }

            if (isReflection) {
                return row;
            }
        }

        return 0;
    }

    public static int summarizePattern(Pattern pattern) {
        int v = findVerticalReflection(pattern);
        if (v > 0) {
            return v;
        }
        int h = findHorizontalReflection(pattern);
        return h * 100;
    }

    public static int part1(List<Pattern> patterns) {
        int sum = 0;
        for (Pattern p : patterns) {
            sum += summarizePattern(p);
        }
        return sum;
    }

    public static int countDifferences(String s1, String s2) {
        int count = 0;
        int minLen = Math.min(s1.length(), s2.length());
        for (int i = 0; i < minLen; i++) {
            if (s1.charAt(i) != s2.charAt(i)) {
                count++;
            }
        }
        return count;
    }

    public static int findVerticalReflectionWithSmudge(Pattern pattern) {
        if (pattern.lines.isEmpty()) {
            return 0;
        }

        int width = pattern.lines.get(0).length();

        for (int col = 1; col < width; col++) {
            int totalDiff = 0;

            for (String row : pattern.lines) {
                String left = new StringBuilder(row.substring(0, col)).reverse().toString();
                String right = row.substring(col);
                int minLen = Math.min(left.length(), right.length());

                totalDiff += countDifferences(left.substring(0, minLen), right.substring(0, minLen));

                if (totalDiff > 1) {
                    break;
                }
            }

            if (totalDiff == 1) {
                return col;
            }
        }

        return 0;
    }

    public static int findHorizontalReflectionWithSmudge(Pattern pattern) {
        if (pattern.lines.isEmpty()) {
            return 0;
        }

        int height = pattern.lines.size();

        for (int row = 1; row < height; row++) {
            int totalDiff = 0;

            List<String> top = new ArrayList<>();
            for (int i = row - 1; i >= 0; i--) {
                top.add(pattern.lines.get(i));
            }

            List<String> bottom = new ArrayList<>();
            for (int i = row; i < height; i++) {
                bottom.add(pattern.lines.get(i));
            }

            int minLen = Math.min(top.size(), bottom.size());
            for (int i = 0; i < minLen; i++) {
                totalDiff += countDifferences(top.get(i), bottom.get(i));
                if (totalDiff > 1) {
                    break;
                }
            }

            if (totalDiff == 1) {
                return row;
            }
        }

        return 0;
    }

    public static int summarizePatternWithSmudge(Pattern pattern) {
        int v = findVerticalReflectionWithSmudge(pattern);
        if (v > 0) {
            return v;
        }
        int h = findHorizontalReflectionWithSmudge(pattern);
        return h * 100;
    }

    public static int part2(List<Pattern> patterns) {
        int sum = 0;
        for (Pattern p : patterns) {
            sum += summarizePatternWithSmudge(p);
        }
        return sum;
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get("../input.txt");
        String text = Files.readString(inputPath);
        List<Pattern> patterns = parseInput(text);

        System.out.println("Part 1: " + part1(patterns));
        System.out.println("Part 2: " + part2(patterns));
    }
}
