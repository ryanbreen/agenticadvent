import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {
    record Point(int r, int c) {}

    private char[][] grid;
    private int rows, cols;
    private Point start, end;

    public static void main(String[] args) throws IOException {
        var solution = new Solution();
        solution.run();
    }

    private void run() throws IOException {
        parseInput();
        var distances = tracePath();

        System.out.println("Part 1: " + countCheats(distances, 2, 100));
        System.out.println("Part 2: " + countCheats(distances, 20, 100));
    }

    private void parseInput() throws IOException {
        var lines = Files.readAllLines(Path.of("../input.txt"));

        rows = lines.size();
        cols = lines.getFirst().length();
        grid = new char[rows][cols];

        for (int r = 0; r < rows; r++) {
            var line = lines.get(r);
            for (int c = 0; c < cols; c++) {
                char ch = line.charAt(c);
                grid[r][c] = ch;
                if (ch == 'S') {
                    start = new Point(r, c);
                } else if (ch == 'E') {
                    end = new Point(r, c);
                }
            }
        }
    }

    private Map<Point, Integer> tracePath() {
        var distances = new HashMap<Point, Integer>();
        var queue = new ArrayDeque<Point>();

        distances.put(start, 0);
        queue.add(start);

        int[][] dirs = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

        while (!queue.isEmpty()) {
            var pos = queue.poll();

            if (pos.equals(end)) {
                break;
            }

            int currentDist = distances.get(pos);

            for (var dir : dirs) {
                int nr = pos.r() + dir[0];
                int nc = pos.c() + dir[1];

                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#') {
                    var next = new Point(nr, nc);
                    if (!distances.containsKey(next)) {
                        distances.put(next, currentDist + 1);
                        queue.add(next);
                    }
                }
            }
        }

        return distances;
    }

    private long countCheats(Map<Point, Integer> distances, int maxCheatTime, int minSavings) {
        var positions = new ArrayList<>(distances.entrySet());
        long count = 0;

        for (var entry1 : positions) {
            var p1 = entry1.getKey();
            int d1 = entry1.getValue();

            for (var entry2 : positions) {
                var p2 = entry2.getKey();
                int d2 = entry2.getValue();

                int cheatCost = Math.abs(p2.r() - p1.r()) + Math.abs(p2.c() - p1.c());

                if (cheatCost <= maxCheatTime) {
                    int savings = d2 - d1 - cheatCost;
                    if (savings >= minSavings) {
                        count++;
                    }
                }
            }
        }

        return count;
    }
}
