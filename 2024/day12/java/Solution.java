import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Solution {
    private static final int[][] DIRECTIONS = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

    private static char[][] grid;
    private static int rows;
    private static int cols;

    public static void main(String[] args) throws IOException {
        // Read input file
        String input = Files.readString(Paths.get("../input.txt")).strip();
        String[] lines = input.split("\n");

        rows = lines.length;
        cols = lines[0].length();
        grid = new char[rows][cols];

        for (int i = 0; i < rows; i++) {
            grid[i] = lines[i].toCharArray();
        }

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static List<Set<Coord>> findRegions() {
        Set<Coord> visited = new HashSet<>();
        List<Set<Coord>> regions = new ArrayList<>();

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                Coord start = new Coord(r, c);
                if (visited.contains(start)) {
                    continue;
                }

                // BFS to find all cells in this region
                char plant = grid[r][c];
                Set<Coord> region = new HashSet<>();
                Deque<Coord> queue = new ArrayDeque<>();
                queue.add(start);

                while (!queue.isEmpty()) {
                    Coord current = queue.poll();

                    visited.add(current);
                    region.add(current);

                    // Add neighbors (check before adding to queue)
                    for (int[] dir : DIRECTIONS) {
                        int nr = current.r + dir[0];
                        int nc = current.c + dir[1];
                        Coord neighbor = new Coord(nr, nc);

                        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols &&
                            !visited.contains(neighbor) &&
                            grid[nr][nc] == plant) {
                            queue.add(neighbor);
                            visited.add(neighbor);  // Mark as visited when adding
                        }
                    }
                }

                regions.add(region);
            }
        }

        return regions;
    }

    private static int calculatePerimeter(Set<Coord> region) {
        int perimeter = 0;
        for (Coord coord : region) {
            // Check all 4 neighbors
            for (int[] dir : DIRECTIONS) {
                Coord neighbor = new Coord(coord.r + dir[0], coord.c + dir[1]);
                if (!region.contains(neighbor)) {
                    perimeter++;
                }
            }
        }
        return perimeter;
    }

    private static int part1() {
        List<Set<Coord>> regions = findRegions();
        return regions.stream()
            .mapToInt(region -> region.size() * calculatePerimeter(region))
            .sum();
    }

    private static int countSides(Set<Coord> region) {
        int corners = 0;

        for (Coord coord : region) {
            int r = coord.r;
            int c = coord.c;

            // Check all 4 corners of this cell
            boolean up = region.contains(new Coord(r - 1, c));
            boolean down = region.contains(new Coord(r + 1, c));
            boolean left = region.contains(new Coord(r, c - 1));
            boolean right = region.contains(new Coord(r, c + 1));
            boolean upLeft = region.contains(new Coord(r - 1, c - 1));
            boolean upRight = region.contains(new Coord(r - 1, c + 1));
            boolean downLeft = region.contains(new Coord(r + 1, c - 1));
            boolean downRight = region.contains(new Coord(r + 1, c + 1));

            // Top-left corner
            if (!up && !left) {  // convex
                corners++;
            } else if (up && left && !upLeft) {  // concave
                corners++;
            }

            // Top-right corner
            if (!up && !right) {  // convex
                corners++;
            } else if (up && right && !upRight) {  // concave
                corners++;
            }

            // Bottom-left corner
            if (!down && !left) {  // convex
                corners++;
            } else if (down && left && !downLeft) {  // concave
                corners++;
            }

            // Bottom-right corner
            if (!down && !right) {  // convex
                corners++;
            } else if (down && right && !downRight) {  // concave
                corners++;
            }
        }

        return corners;
    }

    private static int part2() {
        List<Set<Coord>> regions = findRegions();
        return regions.stream()
            .mapToInt(region -> region.size() * countSides(region))
            .sum();
    }

    // Coordinate record - automatically provides equals/hashCode
    private record Coord(int r, int c) {}
}
