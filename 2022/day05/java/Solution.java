import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;

public class Solution {

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(System.getProperty("user.dir"), "..", "input.txt");
        String content = Files.readString(inputPath);

        String[] parts = content.split("\n\n");
        String[] stackLines = parts[0].split("\n");
        String[] moveLines = parts[1].trim().split("\n");

        // Find number of stacks from the last line (the numbers)
        int numStacks = stackLines[stackLines.length - 1].trim().split("\\s+").length;

        // Parse stacks (bottom-up, excluding the number line)
        List<List<Character>> stacks = new ArrayList<>();
        for (int i = 0; i < numStacks; i++) {
            stacks.add(new ArrayList<>());
        }

        for (int lineIdx = 0; lineIdx < stackLines.length - 1; lineIdx++) {
            String line = stackLines[lineIdx];
            for (int i = 0; i < numStacks; i++) {
                int pos = 1 + i * 4;  // Position of crate letter
                if (pos < line.length() && line.charAt(pos) != ' ') {
                    stacks.get(i).add(line.charAt(pos));
                }
            }
        }

        // Reverse so bottom is at index 0
        for (List<Character> stack : stacks) {
            Collections.reverse(stack);
        }

        // Parse moves
        List<int[]> moves = new ArrayList<>();
        Pattern pattern = Pattern.compile("move (\\d+) from (\\d+) to (\\d+)");
        for (String line : moveLines) {
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) {
                int count = Integer.parseInt(matcher.group(1));
                int fromStack = Integer.parseInt(matcher.group(2)) - 1;  // 0-indexed
                int toStack = Integer.parseInt(matcher.group(3)) - 1;
                moves.add(new int[]{count, fromStack, toStack});
            }
        }

        System.out.println("Part 1: " + part1(stacks, moves));
        System.out.println("Part 2: " + part2(stacks, moves));
    }

    private static List<List<Character>> deepCopy(List<List<Character>> stacks) {
        List<List<Character>> copy = new ArrayList<>();
        for (List<Character> stack : stacks) {
            copy.add(new ArrayList<>(stack));
        }
        return copy;
    }

    private static String part1(List<List<Character>> originalStacks, List<int[]> moves) {
        List<List<Character>> stacks = deepCopy(originalStacks);

        for (int[] move : moves) {
            int count = move[0];
            int fromStack = move[1];
            int toStack = move[2];

            for (int i = 0; i < count; i++) {
                List<Character> from = stacks.get(fromStack);
                char crate = from.remove(from.size() - 1);
                stacks.get(toStack).add(crate);
            }
        }

        StringBuilder result = new StringBuilder();
        for (List<Character> stack : stacks) {
            if (!stack.isEmpty()) {
                result.append(stack.get(stack.size() - 1));
            }
        }
        return result.toString();
    }

    private static String part2(List<List<Character>> originalStacks, List<int[]> moves) {
        List<List<Character>> stacks = deepCopy(originalStacks);

        for (int[] move : moves) {
            int count = move[0];
            int fromStack = move[1];
            int toStack = move[2];

            List<Character> from = stacks.get(fromStack);
            List<Character> to = stacks.get(toStack);

            // Move multiple crates at once (preserve order)
            List<Character> crates = new ArrayList<>(from.subList(from.size() - count, from.size()));
            for (int i = 0; i < count; i++) {
                from.remove(from.size() - 1);
            }
            to.addAll(crates);
        }

        StringBuilder result = new StringBuilder();
        for (List<Character> stack : stacks) {
            if (!stack.isEmpty()) {
                result.append(stack.get(stack.size() - 1));
            }
        }
        return result.toString();
    }
}
