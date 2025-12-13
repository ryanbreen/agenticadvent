import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Solution {

    static class Machine {
        long ax, ay, bx, by, px, py;

        Machine(long ax, long ay, long bx, long by, long px, long py) {
            this.ax = ax;
            this.ay = ay;
            this.bx = bx;
            this.by = by;
            this.px = px;
            this.py = py;
        }
    }

    public static List<Machine> parseMachines(String text) {
        List<Machine> machines = new ArrayList<>();
        String[] blocks = text.split("\n\n");

        Pattern buttonAPattern = Pattern.compile("Button A: X\\+(\\d+), Y\\+(\\d+)");
        Pattern buttonBPattern = Pattern.compile("Button B: X\\+(\\d+), Y\\+(\\d+)");
        Pattern prizePattern = Pattern.compile("Prize: X=(\\d+), Y=(\\d+)");

        for (String block : blocks) {
            String[] lines = block.trim().split("\n");

            // Parse Button A
            Matcher aMatcher = buttonAPattern.matcher(lines[0]);
            if (!aMatcher.find()) continue;
            long ax = Long.parseLong(aMatcher.group(1));
            long ay = Long.parseLong(aMatcher.group(2));

            // Parse Button B
            Matcher bMatcher = buttonBPattern.matcher(lines[1]);
            if (!bMatcher.find()) continue;
            long bx = Long.parseLong(bMatcher.group(1));
            long by = Long.parseLong(bMatcher.group(2));

            // Parse Prize
            Matcher pMatcher = prizePattern.matcher(lines[2]);
            if (!pMatcher.find()) continue;
            long px = Long.parseLong(pMatcher.group(1));
            long py = Long.parseLong(pMatcher.group(2));

            machines.add(new Machine(ax, ay, bx, by, px, py));
        }

        return machines;
    }

    public static Long solveMachine(long ax, long ay, long bx, long by, long px, long py, Long maxPresses) {
        /*
         * Solve for button presses using Cramer's rule.
         *
         * System of equations:
         *   a*ax + b*bx = px
         *   a*ay + b*by = py
         *
         * Solution:
         *   det = ax*by - ay*bx
         *   a = (px*by - py*bx) / det
         *   b = (ax*py - ay*px) / det
         *
         * Returns token cost (3*a + b) or null if no valid solution.
         */
        long det = ax * by - ay * bx;

        if (det == 0) {
            return null; // No unique solution
        }

        // Calculate using integer arithmetic
        long aNum = px * by - py * bx;
        long bNum = ax * py - ay * px;

        // Check if solutions are integers
        if (aNum % det != 0 || bNum % det != 0) {
            return null;
        }

        long a = aNum / det;
        long b = bNum / det;

        // Check non-negative
        if (a < 0 || b < 0) {
            return null;
        }

        // Check max presses constraint (Part 1)
        if (maxPresses != null && (a > maxPresses || b > maxPresses)) {
            return null;
        }

        return 3 * a + b;
    }

    public static long part1(List<Machine> machines) {
        /*
         * Part 1: Max 100 presses per button.
         */
        long total = 0;

        for (Machine m : machines) {
            Long cost = solveMachine(m.ax, m.ay, m.bx, m.by, m.px, m.py, 100L);
            if (cost != null) {
                total += cost;
            }
        }

        return total;
    }

    public static long part2(List<Machine> machines) {
        /*
         * Part 2: Prize coordinates shifted by 10^13, no press limit.
         */
        long offset = 10_000_000_000_000L;
        long total = 0;

        for (Machine m : machines) {
            // Shift prize coordinates
            Long cost = solveMachine(m.ax, m.ay, m.bx, m.by, m.px + offset, m.py + offset, null);
            if (cost != null) {
                total += cost;
            }
        }

        return total;
    }

    public static void main(String[] args) throws IOException {
        String inputText = Files.readString(Path.of("../input.txt")).trim();
        List<Machine> machines = parseMachines(inputText);

        System.out.println("Part 1: " + part1(machines));
        System.out.println("Part 2: " + part2(machines));
    }
}
