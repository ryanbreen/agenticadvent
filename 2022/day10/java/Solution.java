import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(System.getProperty("user.dir"), "..", "input.txt");
        List<String> instructions = Files.readAllLines(inputPath);

        System.out.println("Part 1: " + part1(instructions));
        System.out.println("Part 2:");
        System.out.println(part2(instructions));
    }

    static int part1(List<String> instructions) {
        Set<Integer> targetCycles = new HashSet<>(Arrays.asList(20, 60, 100, 140, 180, 220));
        int total = 0;
        int x = 1;
        int cycle = 0;

        for (String line : instructions) {
            if (line.equals("noop")) {
                cycle++;
                if (targetCycles.contains(cycle)) {
                    total += cycle * x;
                }
            } else { // addx V
                int v = Integer.parseInt(line.split(" ")[1]);
                cycle++;
                if (targetCycles.contains(cycle)) {
                    total += cycle * x;
                }
                cycle++;
                if (targetCycles.contains(cycle)) {
                    total += cycle * x;
                }
                x += v;
            }
        }

        return total;
    }

    static String part2(List<String> instructions) {
        StringBuilder screen = new StringBuilder();
        StringBuilder row = new StringBuilder();
        int x = 1;
        int cycle = 0;

        for (String line : instructions) {
            if (line.equals("noop")) {
                cycle++;
                int pos = (cycle - 1) % 40;
                row.append(Math.abs(pos - x) <= 1 ? '#' : '.');
                if (cycle % 40 == 0) {
                    screen.append(row).append('\n');
                    row = new StringBuilder();
                }
            } else { // addx V
                int v = Integer.parseInt(line.split(" ")[1]);

                // First cycle
                cycle++;
                int pos = (cycle - 1) % 40;
                row.append(Math.abs(pos - x) <= 1 ? '#' : '.');
                if (cycle % 40 == 0) {
                    screen.append(row).append('\n');
                    row = new StringBuilder();
                }

                // Second cycle
                cycle++;
                pos = (cycle - 1) % 40;
                row.append(Math.abs(pos - x) <= 1 ? '#' : '.');
                if (cycle % 40 == 0) {
                    screen.append(row).append('\n');
                    row = new StringBuilder();
                }

                x += v;
            }
        }

        // Remove trailing newline if present
        if (screen.length() > 0 && screen.charAt(screen.length() - 1) == '\n') {
            screen.setLength(screen.length() - 1);
        }

        return screen.toString();
    }
}
