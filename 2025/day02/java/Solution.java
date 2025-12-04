import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class Solution {
    private static String inputText;

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of(args.length > 0 ? args[0] : "../input.txt");
        inputText = Files.readString(inputPath).trim();

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static boolean isInvalidIdPart1(long num) {
        String s = Long.toString(num);
        int length = s.length();

        // Must have even length to be repeated twice
        if (length % 2 != 0) {
            return false;
        }

        // Check if it starts with 0 (leading zeros not allowed)
        if (s.charAt(0) == '0') {
            return false;
        }

        // Split in half and check if both halves are identical
        int mid = length / 2;
        String firstHalf = s.substring(0, mid);
        String secondHalf = s.substring(mid);

        return firstHalf.equals(secondHalf);
    }

    private static boolean isInvalidIdPart2(long num) {
        String s = Long.toString(num);
        int length = s.length();

        // Check if it starts with 0 (leading zeros not allowed)
        if (s.charAt(0) == '0') {
            return false;
        }

        // Try all possible pattern lengths from 1 to length/2
        for (int patternLength = 1; patternLength <= length / 2; patternLength++) {
            if (length % patternLength == 0) {
                String pattern = s.substring(0, patternLength);
                int repetitions = length / patternLength;

                // Check if repeating the pattern gives us the original string
                StringBuilder sb = new StringBuilder();
                for (int i = 0; i < repetitions; i++) {
                    sb.append(pattern);
                }

                if (sb.toString().equals(s)) {
                    return true;
                }
            }
        }

        return false;
    }

    private static long part1() {
        long total = 0;

        String[] parts = inputText.split(",");
        for (String part : parts) {
            part = part.trim();
            if (part.contains("-")) {
                String[] rangeParts = part.split("-");
                if (rangeParts.length == 2) {
                    long start = Long.parseLong(rangeParts[0]);
                    long end = Long.parseLong(rangeParts[1]);

                    for (long num = start; num <= end; num++) {
                        if (isInvalidIdPart1(num)) {
                            total += num;
                        }
                    }
                }
            }
        }

        return total;
    }

    private static long part2() {
        long total = 0;

        String[] parts = inputText.split(",");
        for (String part : parts) {
            part = part.trim();
            if (part.contains("-")) {
                String[] rangeParts = part.split("-");
                if (rangeParts.length == 2) {
                    long start = Long.parseLong(rangeParts[0]);
                    long end = Long.parseLong(rangeParts[1]);

                    for (long num = start; num <= end; num++) {
                        if (isInvalidIdPart2(num)) {
                            total += num;
                        }
                    }
                }
            }
        }

        return total;
    }
}
