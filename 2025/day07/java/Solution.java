import java.io.IOException;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Solution {
    private static String[] lines;

    public static void main(String[] args) throws IOException {
        // Read input file from ../input.txt
        String inputText = Files.readString(Paths.get("../input.txt")).trim();
        lines = inputText.split("\n");

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static long part1() {
        int rows = lines.length;
        int cols = rows > 0 ? lines[0].length() : 0;

        // Find starting position S
        Integer startCol = null;
        for (int col = 0; col < cols; col++) {
            if (lines[0].charAt(col) == 'S') {
                startCol = col;
                break;
            }
        }

        if (startCol == null) {
            return 0;
        }

        // Track active beam columns at each row
        // Use a set to handle beam merging
        Set<Integer> activeBeams = new HashSet<>();
        activeBeams.add(startCol);
        long splitCount = 0;

        // Process row by row starting from row 1 (below S)
        for (int row = 1; row < rows; row++) {
            Set<Integer> newBeams = new HashSet<>();

            for (int col : activeBeams) {
                if (col >= 0 && col < cols) {
                    char cell = lines[row].charAt(col);
                    if (cell == '^') {
                        // Beam hits splitter - count it and emit left/right
                        splitCount++;
                        // Left beam goes to col-1, right beam goes to col+1
                        if (col - 1 >= 0) {
                            newBeams.add(col - 1);
                        }
                        if (col + 1 < cols) {
                            newBeams.add(col + 1);
                        }
                    } else if (cell == '.') {
                        // Beam continues straight down
                        newBeams.add(col);
                    } else {
                        // If cell is something else (like S), beam continues
                        newBeams.add(col);
                    }
                }
            }

            activeBeams = newBeams;

            // If no more beams, stop
            if (activeBeams.isEmpty()) {
                break;
            }
        }

        return splitCount;
    }

    private static BigInteger part2() {
        int rows = lines.length;
        int cols = rows > 0 ? lines[0].length() : 0;

        // Find starting position S
        Integer startCol = null;
        for (int col = 0; col < cols; col++) {
            if (lines[0].charAt(col) == 'S') {
                startCol = col;
                break;
            }
        }

        if (startCol == null) {
            return BigInteger.ZERO;
        }

        // Track number of timelines at each column position
        // Use a map: col -> count of timelines at that position
        Map<Integer, BigInteger> timelines = new HashMap<>();
        timelines.put(startCol, BigInteger.ONE);

        // Process row by row starting from row 1 (below S)
        for (int row = 1; row < rows; row++) {
            Map<Integer, BigInteger> newTimelines = new HashMap<>();

            for (Map.Entry<Integer, BigInteger> entry : timelines.entrySet()) {
                int col = entry.getKey();
                BigInteger count = entry.getValue();

                if (col >= 0 && col < cols) {
                    char cell = lines[row].charAt(col);
                    if (cell == '^') {
                        // Each timeline splits into 2 (left and right)
                        if (col - 1 >= 0) {
                            newTimelines.merge(col - 1, count, BigInteger::add);
                        }
                        if (col + 1 < cols) {
                            newTimelines.merge(col + 1, count, BigInteger::add);
                        }
                    } else if (cell == '.') {
                        // Timelines continue straight down
                        newTimelines.merge(col, count, BigInteger::add);
                    } else {
                        // Other characters - timelines continue
                        newTimelines.merge(col, count, BigInteger::add);
                    }
                }
            }

            timelines = newTimelines;

            // If no more timelines, stop
            if (timelines.isEmpty()) {
                break;
            }
        }

        // Total number of timelines
        BigInteger total = BigInteger.ZERO;
        for (BigInteger count : timelines.values()) {
            total = total.add(count);
        }

        return total;
    }
}
