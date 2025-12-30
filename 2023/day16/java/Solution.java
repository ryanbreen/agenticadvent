import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {
    // Direction encoding: 0=right, 1=down, 2=left, 3=up
    private static final int[] DR = {0, 1, 0, -1};
    private static final int[] DC = {1, 0, -1, 0};

    // Direction mappings for mirrors
    // '/' mirror: right->up, down->left, left->down, up->right
    private static final int[] SLASH_MAP = {3, 2, 1, 0};
    // '\' mirror: right->down, down->right, left->up, up->left
    private static final int[] BACKSLASH_MAP = {1, 0, 3, 2};

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of(args.length > 0 ? args[0] : "../input.txt");
        List<String> grid = Files.readAllLines(inputPath);

        System.out.println("Part 1: " + part1(grid));
        System.out.println("Part 2: " + part2(grid));
    }

    private static int part1(List<String> grid) {
        return countEnergized(grid, 0, 0, 0);
    }

    private static int part2(List<String> grid) {
        int rows = grid.size();
        int cols = grid.get(0).length();
        int maxEnergized = 0;

        // Top row, heading down
        for (int c = 0; c < cols; c++) {
            maxEnergized = Math.max(maxEnergized, countEnergized(grid, 0, c, 1));
        }

        // Bottom row, heading up
        for (int c = 0; c < cols; c++) {
            maxEnergized = Math.max(maxEnergized, countEnergized(grid, rows - 1, c, 3));
        }

        // Left column, heading right
        for (int r = 0; r < rows; r++) {
            maxEnergized = Math.max(maxEnergized, countEnergized(grid, r, 0, 0));
        }

        // Right column, heading left
        for (int r = 0; r < rows; r++) {
            maxEnergized = Math.max(maxEnergized, countEnergized(grid, r, cols - 1, 2));
        }

        return maxEnergized;
    }

    private static int countEnergized(List<String> grid, int startRow, int startCol, int startDir) {
        int rows = grid.size();
        int cols = grid.get(0).length();

        // Track visited states as (row, col, direction)
        Set<Long> visited = new HashSet<>();
        Set<Long> energizedTiles = new HashSet<>();

        // BFS queue: encoded as (row << 20) | (col << 4) | direction
        Deque<int[]> queue = new ArrayDeque<>();
        queue.add(new int[]{startRow, startCol, startDir});

        while (!queue.isEmpty()) {
            int[] state = queue.poll();
            int r = state[0];
            int c = state[1];
            int d = state[2];

            // Check bounds
            if (r < 0 || r >= rows || c < 0 || c >= cols) {
                continue;
            }

            // Create state key for visited set
            long stateKey = ((long) r << 20) | ((long) c << 4) | d;
            if (visited.contains(stateKey)) {
                continue;
            }
            visited.add(stateKey);

            // Track energized tile (just row, col)
            long tileKey = ((long) r << 16) | c;
            energizedTiles.add(tileKey);

            char cell = grid.get(r).charAt(c);
            int[] nextDirs = getNextDirections(cell, d);

            for (int nd : nextDirs) {
                queue.add(new int[]{r + DR[nd], c + DC[nd], nd});
            }
        }

        return energizedTiles.size();
    }

    private static int[] getNextDirections(char cell, int direction) {
        return switch (cell) {
            case '.' -> new int[]{direction};
            case '/' -> new int[]{SLASH_MAP[direction]};
            case '\\' -> new int[]{BACKSLASH_MAP[direction]};
            case '|' -> (direction == 0 || direction == 2)
                ? new int[]{1, 3}  // Split vertically
                : new int[]{direction};  // Pass through
            case '-' -> (direction == 1 || direction == 3)
                ? new int[]{0, 2}  // Split horizontally
                : new int[]{direction};  // Pass through
            default -> new int[]{direction};
        };
    }
}
