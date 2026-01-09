import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {
    private static final int WIDTH = 7;

    // Rock shapes as list of (dx, dy) offsets from bottom-left
    private static final int[][][] ROCKS = {
        {{0, 0}, {1, 0}, {2, 0}, {3, 0}},           // Horizontal line
        {{1, 0}, {0, 1}, {1, 1}, {2, 1}, {1, 2}},   // Plus
        {{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}},   // L shape
        {{0, 0}, {0, 1}, {0, 2}, {0, 3}},           // Vertical line
        {{0, 0}, {1, 0}, {0, 1}, {1, 1}}            // Square
    };

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(args.length > 0 ? args[0] : "../input.txt");
        String jets = Files.readString(inputPath).trim();

        System.out.println("Part 1: " + simulate(jets, 2022));
        System.out.println("Part 2: " + simulate(jets, 1000000000000L));
    }

    private static long simulate(String jets, long numRocks) {
        Set<Long> occupied = new HashSet<>();
        long height = 0;
        int jetIdx = 0;

        // For cycle detection
        Map<String, long[]> states = new HashMap<>();
        List<Long> heights = new ArrayList<>();

        for (long rockNum = 0; rockNum < numRocks; rockNum++) {
            int rockType = (int)(rockNum % 5);
            int[][] rock = ROCKS[rockType];

            // Starting position: left edge at x=2, bottom at y=height+3
            int x = 2;
            long y = height + 3;

            while (true) {
                // Jet push
                char jet = jets.charAt(jetIdx);
                jetIdx = (jetIdx + 1) % jets.length();

                int dx = (jet == '>') ? 1 : -1;

                // Check if can move horizontally
                boolean canMove = true;
                for (int[] offset : rock) {
                    int nx = x + offset[0] + dx;
                    long ny = y + offset[1];
                    if (nx < 0 || nx >= WIDTH || occupied.contains(encode(nx, ny))) {
                        canMove = false;
                        break;
                    }
                }

                if (canMove) {
                    x += dx;
                }

                // Fall down
                boolean canFall = true;
                for (int[] offset : rock) {
                    int nx = x + offset[0];
                    long ny = y + offset[1] - 1;
                    if (ny < 0 || occupied.contains(encode(nx, ny))) {
                        canFall = false;
                        break;
                    }
                }

                if (canFall) {
                    y--;
                } else {
                    // Rock stops
                    for (int[] offset : rock) {
                        occupied.add(encode(x + offset[0], y + offset[1]));
                        height = Math.max(height, y + offset[1] + 1);
                    }
                    break;
                }
            }

            heights.add(height);

            // Cycle detection for Part 2
            if (numRocks > 10000) {
                // Create state key from surface profile
                int profileDepth = 30;
                StringBuilder profile = new StringBuilder();
                for (int col = 0; col < WIDTH; col++) {
                    boolean found = false;
                    for (int row = 0; row < profileDepth; row++) {
                        if (occupied.contains(encode(col, height - 1 - row))) {
                            profile.append(col).append(",").append(row).append(";");
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        profile.append(col).append(",").append(profileDepth).append(";");
                    }
                }

                String stateKey = rockType + "," + jetIdx + "," + profile.toString();

                if (states.containsKey(stateKey)) {
                    // Found cycle
                    long[] prev = states.get(stateKey);
                    long cycleStart = prev[0];
                    long cycleStartHeight = prev[1];
                    long cycleLen = rockNum - cycleStart;
                    long cycleHeight = height - cycleStartHeight;

                    // Calculate final height
                    long remaining = numRocks - rockNum - 1;
                    long fullCycles = remaining / cycleLen;
                    long leftover = remaining % cycleLen;

                    long finalHeight = height + fullCycles * cycleHeight;
                    if (leftover > 0) {
                        finalHeight += heights.get((int)(cycleStart + leftover)) - heights.get((int)cycleStart);
                    }

                    return finalHeight;
                }

                states.put(stateKey, new long[]{rockNum, height});
            }
        }

        return height;
    }

    private static long encode(int x, long y) {
        return ((long)x << 40) | y;
    }
}
