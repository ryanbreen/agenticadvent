import java.io.*;
import java.util.*;

public class Solution {
    private char[][] grid;
    private int rows, cols;
    private int[] start, end;

    public static void main(String[] args) throws IOException {
        Solution solution = new Solution();
        solution.run();
    }

    private void run() throws IOException {
        parseInput();

        Map<Long, Integer> dist = tracePath();

        System.out.println("Part 1: " + countCheats(dist, 2, 100));
        System.out.println("Part 2: " + countCheats(dist, 20, 100));
    }

    private void parseInput() throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader("../input.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (!line.isEmpty()) {
                    lines.add(line);
                }
            }
        }

        rows = lines.size();
        cols = lines.get(0).length();
        grid = new char[rows][cols];

        for (int r = 0; r < rows; r++) {
            String line = lines.get(r);
            for (int c = 0; c < cols; c++) {
                char ch = line.charAt(c);
                grid[r][c] = ch;
                if (ch == 'S') {
                    start = new int[]{r, c};
                } else if (ch == 'E') {
                    end = new int[]{r, c};
                }
            }
        }
    }

    private long key(int r, int c) {
        return ((long) r << 16) | (c & 0xFFFF);
    }

    private Map<Long, Integer> tracePath() {
        Map<Long, Integer> dist = new HashMap<>();
        Queue<int[]> queue = new LinkedList<>();

        dist.put(key(start[0], start[1]), 0);
        queue.add(start);

        int[][] dirs = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

        while (!queue.isEmpty()) {
            int[] pos = queue.poll();
            int r = pos[0], c = pos[1];

            if (r == end[0] && c == end[1]) {
                break;
            }

            int currentDist = dist.get(key(r, c));

            for (int[] dir : dirs) {
                int nr = r + dir[0];
                int nc = c + dir[1];

                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#') {
                    long nkey = key(nr, nc);
                    if (!dist.containsKey(nkey)) {
                        dist.put(nkey, currentDist + 1);
                        queue.add(new int[]{nr, nc});
                    }
                }
            }
        }

        return dist;
    }

    private long countCheats(Map<Long, Integer> dist, int maxCheatTime, int minSavings) {
        long count = 0;

        List<long[]> positions = new ArrayList<>();
        for (Map.Entry<Long, Integer> entry : dist.entrySet()) {
            long k = entry.getKey();
            int r = (int) (k >> 16);
            int c = (int) (k & 0xFFFF);
            int d = entry.getValue();
            positions.add(new long[]{r, c, d});
        }

        for (long[] pos1 : positions) {
            int r1 = (int) pos1[0];
            int c1 = (int) pos1[1];
            int d1 = (int) pos1[2];

            for (long[] pos2 : positions) {
                int r2 = (int) pos2[0];
                int c2 = (int) pos2[1];
                int d2 = (int) pos2[2];

                int cheatCost = Math.abs(r2 - r1) + Math.abs(c2 - c1);

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
