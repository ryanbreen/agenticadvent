import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    /**
     * Parse a packet string into nested List/Integer structure.
     * Returns an Object which is either an Integer or a List<Object>.
     */
    static Object parse(String s) {
        int[] pos = {0};
        return parseValue(s, pos);
    }

    static Object parseValue(String s, int[] pos) {
        if (s.charAt(pos[0]) == '[') {
            return parseList(s, pos);
        } else {
            return parseNumber(s, pos);
        }
    }

    static List<Object> parseList(String s, int[] pos) {
        List<Object> list = new ArrayList<>();
        pos[0]++; // skip '['

        while (s.charAt(pos[0]) != ']') {
            if (s.charAt(pos[0]) == ',') {
                pos[0]++; // skip comma
            } else {
                list.add(parseValue(s, pos));
            }
        }
        pos[0]++; // skip ']'
        return list;
    }

    static Integer parseNumber(String s, int[] pos) {
        int start = pos[0];
        while (pos[0] < s.length() && Character.isDigit(s.charAt(pos[0]))) {
            pos[0]++;
        }
        return Integer.parseInt(s.substring(start, pos[0]));
    }

    /**
     * Compare two values recursively.
     * Returns: -1 if left < right (correct order)
     *           1 if left > right (wrong order)
     *           0 if equal (continue)
     */
    @SuppressWarnings("unchecked")
    static int compare(Object left, Object right) {
        // Both integers
        if (left instanceof Integer && right instanceof Integer) {
            int l = (Integer) left;
            int r = (Integer) right;
            if (l < r) return -1;
            if (l > r) return 1;
            return 0;
        }

        // Both lists
        if (left instanceof List && right instanceof List) {
            List<Object> leftList = (List<Object>) left;
            List<Object> rightList = (List<Object>) right;

            int minLen = Math.min(leftList.size(), rightList.size());
            for (int i = 0; i < minLen; i++) {
                int result = compare(leftList.get(i), rightList.get(i));
                if (result != 0) {
                    return result;
                }
            }

            // Check lengths
            if (leftList.size() < rightList.size()) return -1;
            if (leftList.size() > rightList.size()) return 1;
            return 0;
        }

        // Mixed types - convert integer to list
        if (left instanceof Integer) {
            List<Object> leftList = new ArrayList<>();
            leftList.add(left);
            return compare(leftList, right);
        } else {
            List<Object> rightList = new ArrayList<>();
            rightList.add(right);
            return compare(left, rightList);
        }
    }

    static int part1(String text) {
        String[] pairs = text.trim().split("\n\n");
        int total = 0;

        for (int i = 0; i < pairs.length; i++) {
            String[] lines = pairs[i].trim().split("\n");
            Object left = parse(lines[0]);
            Object right = parse(lines[1]);

            if (compare(left, right) == -1) {
                total += (i + 1); // 1-indexed
            }
        }

        return total;
    }

    static int part2(String text) {
        String[] lines = text.trim().split("\n");
        List<Object> packets = new ArrayList<>();

        for (String line : lines) {
            if (!line.isEmpty()) {
                packets.add(parse(line));
            }
        }

        // Add divider packets
        Object divider1 = parse("[[2]]");
        Object divider2 = parse("[[6]]");
        packets.add(divider1);
        packets.add(divider2);

        // Sort using comparison function
        packets.sort((a, b) -> compare(a, b));

        // Find positions of dividers (1-indexed)
        int pos1 = -1, pos2 = -1;
        for (int i = 0; i < packets.size(); i++) {
            if (compare(packets.get(i), divider1) == 0) {
                pos1 = i + 1;
            }
            if (compare(packets.get(i), divider2) == 0) {
                pos2 = i + 1;
            }
        }

        return pos1 * pos2;
    }

    public static void main(String[] args) throws IOException {
        String text = Files.readString(Path.of("../input.txt"));

        System.out.println("Part 1: " + part1(text));
        System.out.println("Part 2: " + part2(text));
    }
}
