import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    // Direction checks: positions to check, and delta to move
    private static final int[][] NORTH_CHECK = {{-1, -1}, {-1, 0}, {-1, 1}};
    private static final int[] NORTH_DELTA = {-1, 0};

    private static final int[][] SOUTH_CHECK = {{1, -1}, {1, 0}, {1, 1}};
    private static final int[] SOUTH_DELTA = {1, 0};

    private static final int[][] WEST_CHECK = {{-1, -1}, {0, -1}, {1, -1}};
    private static final int[] WEST_DELTA = {0, -1};

    private static final int[][] EAST_CHECK = {{-1, 1}, {0, 1}, {1, 1}};
    private static final int[] EAST_DELTA = {0, 1};

    // All 8 neighbors
    private static final int[][] ALL_NEIGHBORS = {
        {-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}
    };

    // Encodes row, col as a single long for use as hash key
    private static long encode(int r, int c) {
        return ((long) r << 32) | (c & 0xFFFFFFFFL);
    }

    private static int decodeRow(long encoded) {
        return (int) (encoded >> 32);
    }

    private static int decodeCol(long encoded) {
        return (int) encoded;
    }

    private static Set<Long> parseInput(String text) {
        Set<Long> elves = new HashSet<>();
        String[] lines = text.trim().split("\n");
        for (int r = 0; r < lines.length; r++) {
            String line = lines[r];
            for (int c = 0; c < line.length(); c++) {
                if (line.charAt(c) == '#') {
                    elves.add(encode(r, c));
                }
            }
        }
        return elves;
    }

    private static int[][][] getDirectionChecks() {
        return new int[][][] {
            NORTH_CHECK, SOUTH_CHECK, WEST_CHECK, EAST_CHECK
        };
    }

    private static int[][] getDirectionDeltas() {
        return new int[][] {
            NORTH_DELTA, SOUTH_DELTA, WEST_DELTA, EAST_DELTA
        };
    }

    // Returns: {new elves set, whether any elf moved}
    private static Object[] simulateRound(Set<Long> elves, int startDir) {
        int[][][] dirChecks = getDirectionChecks();
        int[][] dirDeltas = getDirectionDeltas();

        // Phase 1: Each elf proposes a move
        Map<Long, Long> proposals = new HashMap<>(); // elf -> proposed position
        Map<Long, Integer> proposalCounts = new HashMap<>(); // position -> count

        for (long elf : elves) {
            int r = decodeRow(elf);
            int c = decodeCol(elf);

            // Check if any neighbors
            boolean hasNeighbor = false;
            for (int[] delta : ALL_NEIGHBORS) {
                if (elves.contains(encode(r + delta[0], c + delta[1]))) {
                    hasNeighbor = true;
                    break;
                }
            }

            if (!hasNeighbor) {
                continue; // Don't move
            }

            // Try each direction in order
            for (int i = 0; i < 4; i++) {
                int dirIdx = (startDir + i) % 4;
                int[][] checks = dirChecks[dirIdx];
                int[] delta = dirDeltas[dirIdx];

                boolean canMove = true;
                for (int[] check : checks) {
                    if (elves.contains(encode(r + check[0], c + check[1]))) {
                        canMove = false;
                        break;
                    }
                }

                if (canMove) {
                    long newPos = encode(r + delta[0], c + delta[1]);
                    proposals.put(elf, newPos);
                    proposalCounts.merge(newPos, 1, Integer::sum);
                    break;
                }
            }
        }

        // Phase 2: Execute moves (only if unique proposal)
        Set<Long> newElves = new HashSet<>();
        boolean moved = false;

        for (long elf : elves) {
            if (proposals.containsKey(elf)) {
                long newPos = proposals.get(elf);
                if (proposalCounts.get(newPos) == 1) {
                    newElves.add(newPos);
                    moved = true;
                } else {
                    newElves.add(elf);
                }
            } else {
                newElves.add(elf);
            }
        }

        return new Object[] {newElves, moved};
    }

    private static int boundingRectEmpty(Set<Long> elves) {
        int minR = Integer.MAX_VALUE, maxR = Integer.MIN_VALUE;
        int minC = Integer.MAX_VALUE, maxC = Integer.MIN_VALUE;

        for (long elf : elves) {
            int r = decodeRow(elf);
            int c = decodeCol(elf);
            minR = Math.min(minR, r);
            maxR = Math.max(maxR, r);
            minC = Math.min(minC, c);
            maxC = Math.max(maxC, c);
        }

        int area = (maxR - minR + 1) * (maxC - minC + 1);
        return area - elves.size();
    }

    @SuppressWarnings("unchecked")
    private static int part1(String text) {
        Set<Long> elves = parseInput(text);
        int startDir = 0;

        for (int round = 0; round < 10; round++) {
            Object[] result = simulateRound(elves, startDir);
            elves = (Set<Long>) result[0];
            startDir = (startDir + 1) % 4;
        }

        return boundingRectEmpty(elves);
    }

    @SuppressWarnings("unchecked")
    private static int part2(String text) {
        Set<Long> elves = parseInput(text);
        int startDir = 0;
        int roundNum = 0;

        while (true) {
            roundNum++;
            Object[] result = simulateRound(elves, startDir);
            elves = (Set<Long>) result[0];
            boolean moved = (boolean) result[1];

            if (!moved) {
                return roundNum;
            }

            startDir = (startDir + 1) % 4;
        }
    }

    public static void main(String[] args) throws IOException {
        // Get the directory where the .class file is located
        String classPath = Solution.class.getProtectionDomain()
            .getCodeSource().getLocation().getPath();
        // Handle URL encoding (spaces as %20, etc.)
        Path classDir = Paths.get(java.net.URLDecoder.decode(classPath, "UTF-8"));

        // If classPath is a directory, use it directly; otherwise get parent
        if (!Files.isDirectory(classDir)) {
            classDir = classDir.getParent();
        }

        Path inputFile = classDir.resolve("../input.txt").normalize();

        String text = Files.readString(inputFile);

        System.out.println("Part 1: " + part1(text));
        System.out.println("Part 2: " + part2(text));
    }
}
