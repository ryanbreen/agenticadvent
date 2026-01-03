import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * Day 22: Sand Slabs - 3D falling bricks simulation.
 */
public class Solution {

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of(args.length > 0 ? args[0] : "../input.txt");
        List<int[]> bricks = parseInput(inputPath);

        System.out.println("Part 1: " + part1(bricks));
        System.out.println("Part 2: " + part2(bricks));
    }

    static List<int[]> parseInput(Path path) throws IOException {
        List<int[]> bricks = new ArrayList<>();
        for (String line : Files.readAllLines(path)) {
            String[] parts = line.split("~");
            String[] left = parts[0].split(",");
            String[] right = parts[1].split(",");

            int x1 = Integer.parseInt(left[0]);
            int y1 = Integer.parseInt(left[1]);
            int z1 = Integer.parseInt(left[2]);
            int x2 = Integer.parseInt(right[0]);
            int y2 = Integer.parseInt(right[1]);
            int z2 = Integer.parseInt(right[2]);

            // Ensure z1 <= z2 for consistent processing
            if (z1 > z2) {
                int tx = x1, ty = y1, tz = z1;
                x1 = x2; y1 = y2; z1 = z2;
                x2 = tx; y2 = ty; z2 = tz;
            }

            bricks.add(new int[]{x1, y1, z1, x2, y2, z2});
        }
        return bricks;
    }

    static class SettleResult {
        int[][] settled;
        Map<Integer, Set<Integer>> supports;      // brick i -> bricks it supports (above)
        Map<Integer, Set<Integer>> supporters;    // brick i -> bricks that support it (below)

        SettleResult(int n) {
            settled = new int[n][];
            supports = new HashMap<>();
            supporters = new HashMap<>();
            for (int i = 0; i < n; i++) {
                supports.put(i, new HashSet<>());
                supporters.put(i, new HashSet<>());
            }
        }
    }

    static SettleResult settleBricks(List<int[]> bricks) {
        int n = bricks.size();
        SettleResult result = new SettleResult(n);

        // Create indexed list and sort by minimum z coordinate
        Integer[] order = new Integer[n];
        for (int i = 0; i < n; i++) order[i] = i;
        Arrays.sort(order, (a, b) -> Integer.compare(bricks.get(a)[2], bricks.get(b)[2]));

        // Track occupied cells: (x, y, z) -> brick index
        Map<Long, Integer> occupied = new HashMap<>();

        for (int origIdx : order) {
            int[] brick = bricks.get(origIdx);
            int x1 = brick[0], y1 = brick[1], z1 = brick[2];
            int x2 = brick[3], y2 = brick[4], z2 = brick[5];

            // Find the lowest z where this brick can rest
            int drop = z1 - 1;  // Maximum drop (to z=1)

            // Get xy footprint of this brick
            for (int x = Math.min(x1, x2); x <= Math.max(x1, x2); x++) {
                for (int y = Math.min(y1, y2); y <= Math.max(y1, y2); y++) {
                    // Check each z level below the brick
                    for (int z = z1 - 1; z > 0; z--) {
                        if (occupied.containsKey(key(x, y, z))) {
                            drop = Math.min(drop, z1 - z - 1);
                            break;
                        }
                    }
                }
            }

            // Drop the brick
            int newZ1 = z1 - drop;
            int newZ2 = z2 - drop;
            result.settled[origIdx] = new int[]{x1, y1, newZ1, x2, y2, newZ2};

            // Mark cells as occupied and find supporters
            for (int x = Math.min(x1, x2); x <= Math.max(x1, x2); x++) {
                for (int y = Math.min(y1, y2); y <= Math.max(y1, y2); y++) {
                    // Check if there's a brick directly below
                    long belowKey = key(x, y, newZ1 - 1);
                    if (occupied.containsKey(belowKey)) {
                        int supporterIdx = occupied.get(belowKey);
                        result.supporters.get(origIdx).add(supporterIdx);
                        result.supports.get(supporterIdx).add(origIdx);
                    }

                    // Mark all cells of this brick as occupied
                    for (int z = newZ1; z <= newZ2; z++) {
                        occupied.put(key(x, y, z), origIdx);
                    }
                }
            }
        }

        return result;
    }

    static long key(int x, int y, int z) {
        // Pack x, y, z into a single long (assuming coords fit in ~20 bits each)
        return ((long)x << 40) | ((long)y << 20) | z;
    }

    static int part1(List<int[]> bricks) {
        SettleResult sr = settleBricks(bricks);

        int safeCount = 0;
        for (int i = 0; i < bricks.size(); i++) {
            // Brick i can be safely removed if every brick it supports
            // has at least one other supporter
            boolean canRemove = true;
            for (int supported : sr.supports.get(i)) {
                if (sr.supporters.get(supported).size() == 1) {
                    canRemove = false;
                    break;
                }
            }
            if (canRemove) {
                safeCount++;
            }
        }

        return safeCount;
    }

    static int part2(List<int[]> bricks) {
        SettleResult sr = settleBricks(bricks);

        int totalFalls = 0;

        for (int i = 0; i < bricks.size(); i++) {
            // Simulate removing brick i and count chain reaction
            // BFS to find all bricks that would fall
            Set<Integer> falling = new HashSet<>();
            falling.add(i);
            Deque<Integer> queue = new ArrayDeque<>();
            queue.add(i);

            while (!queue.isEmpty()) {
                int brick = queue.poll();

                // Check all bricks that this brick supports
                for (int supported : sr.supports.get(brick)) {
                    if (falling.contains(supported)) {
                        continue;
                    }

                    // This brick falls if all its supporters have fallen
                    if (falling.containsAll(sr.supporters.get(supported))) {
                        falling.add(supported);
                        queue.add(supported);
                    }
                }
            }

            // Don't count the initial brick we removed
            totalFalls += falling.size() - 1;
        }

        return totalFalls;
    }
}
