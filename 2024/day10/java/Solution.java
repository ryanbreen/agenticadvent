import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;

public class Solution {
    record Point(int r, int c) {}

    // Directions: up, down, left, right
    private static final int[][] DIRS = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

    private final int[][] grid;
    private final int rows;
    private final int cols;

    public Solution(String input) {
        // Parse input into grid
        String[] lines = input.split("\n");
        this.rows = lines.length;
        this.cols = lines[0].length();
        this.grid = new int[rows][cols];

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                grid[r][c] = lines[r].charAt(c) - '0';
            }
        }
    }

    public static void main(String[] args) throws IOException {
        // Read input file
        String input = Files.readString(Paths.get("../input.txt")).trim();
        Solution solution = new Solution(input);

        System.out.println("Part 1: " + solution.part1());
        System.out.println("Part 2: " + solution.part2());
    }

    private List<Point> findTrailheads() {
        List<Point> trailheads = new ArrayList<>();
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid[r][c] == 0) {
                    trailheads.add(new Point(r, c));
                }
            }
        }
        return trailheads;
    }

    private int countReachableNines(Point start) {
        Set<Point> visited = new HashSet<>();
        Queue<Point> queue = new LinkedList<>();
        Set<Point> nines = new HashSet<>();

        visited.add(start);
        queue.offer(start);

        while (!queue.isEmpty()) {
            Point pos = queue.poll();
            int currentHeight = grid[pos.r][pos.c];

            if (currentHeight == 9) {
                nines.add(pos);
                continue;
            }

            // Try all four directions
            for (int[] dir : DIRS) {
                int nr = pos.r + dir[0];
                int nc = pos.c + dir[1];

                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                    Point next = new Point(nr, nc);
                    if (!visited.contains(next)) {
                        if (grid[nr][nc] == currentHeight + 1) {
                            visited.add(next);
                            queue.offer(next);
                        }
                    }
                }
            }
        }

        return nines.size();
    }

    private int part1() {
        List<Point> trailheads = findTrailheads();
        int totalScore = 0;
        for (Point trailhead : trailheads) {
            totalScore += countReachableNines(trailhead);
        }
        return totalScore;
    }

    private int dfs(int r, int c) {
        int currentHeight = grid[r][c];
        if (currentHeight == 9) {
            return 1;
        }

        int total = 0;
        for (int[] dir : DIRS) {
            int nr = r + dir[0];
            int nc = c + dir[1];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                if (grid[nr][nc] == currentHeight + 1) {
                    total += dfs(nr, nc);
                }
            }
        }

        return total;
    }

    private int part2() {
        List<Point> trailheads = findTrailheads();
        int totalRating = 0;
        for (Point trailhead : trailheads) {
            totalRating += dfs(trailhead.r, trailhead.c);
        }
        return totalRating;
    }
}
