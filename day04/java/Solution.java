import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Solution {
    private static final int[][] DIRECTIONS = {
        {-1, -1}, {-1, 0}, {-1, 1},
        {0, -1},           {0, 1},
        {1, -1},  {1, 0},  {1, 1}
    };

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Paths.get("../input.txt")).strip();
        String[] lines = input.split("\n");

        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: " + part2(lines));
    }

    private static int part1(String[] lines) {
        int rows = lines.length;
        int cols = rows > 0 ? lines[0].length() : 0;
        int accessibleCount = 0;

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (lines[r].charAt(c) == '@') {
                    // Count adjacent rolls
                    int adjacentRolls = 0;
                    for (int[] dir : DIRECTIONS) {
                        int nr = r + dir[0];
                        int nc = c + dir[1];
                        // Check bounds
                        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                            if (lines[nr].charAt(nc) == '@') {
                                adjacentRolls++;
                            }
                        }
                    }

                    // Accessible if fewer than 4 adjacent rolls
                    if (adjacentRolls < 4) {
                        accessibleCount++;
                    }
                }
            }
        }

        return accessibleCount;
    }

    private static int part2(String[] lines) {
        int rows = lines.length;
        int cols = rows > 0 ? lines[0].length() : 0;

        // Create a mutable copy of the grid
        char[][] grid = new char[rows][cols];
        for (int r = 0; r < rows; r++) {
            grid[r] = lines[r].toCharArray();
        }

        int totalRemoved = 0;

        while (true) {
            // Find all rolls that can be removed in this iteration
            List<int[]> removable = new ArrayList<>();

            for (int r = 0; r < rows; r++) {
                for (int c = 0; c < cols; c++) {
                    if (grid[r][c] == '@') {
                        // Count adjacent rolls
                        int adjacentRolls = 0;
                        for (int[] dir : DIRECTIONS) {
                            int nr = r + dir[0];
                            int nc = c + dir[1];
                            // Check bounds
                            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                                if (grid[nr][nc] == '@') {
                                    adjacentRolls++;
                                }
                            }
                        }

                        // Can be removed if fewer than 4 adjacent rolls
                        if (adjacentRolls < 4) {
                            removable.add(new int[]{r, c});
                        }
                    }
                }
            }

            // If no rolls can be removed, we're done
            if (removable.isEmpty()) {
                break;
            }

            // Remove all accessible rolls
            for (int[] pos : removable) {
                grid[pos[0]][pos[1]] = '.';
            }

            totalRemoved += removable.size();
        }

        return totalRemoved;
    }
}
