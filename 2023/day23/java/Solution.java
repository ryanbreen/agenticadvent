import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * Day 23: A Long Walk - Longest path through hiking trails.
 */
public class Solution {

    private static final int[] DR = {-1, 1, 0, 0};
    private static final int[] DC = {0, 0, -1, 1};

    private static final Map<Character, int[]> SLOPE_DIRS = Map.of(
        '^', new int[]{-1, 0},
        'v', new int[]{1, 0},
        '<', new int[]{0, -1},
        '>', new int[]{0, 1}
    );

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(args.length > 0 ? args[0] : "../input.txt");
        List<String> grid = Files.readAllLines(inputPath);

        System.out.println("Part 1: " + solve(grid, true));
        System.out.println("Part 2: " + solve(grid, false));
    }

    /**
     * Solve for either part.
     */
    private static int solve(List<String> grid, boolean respectSlopes) {
        int rows = grid.size();
        int cols = grid.get(0).length();

        int startCol = grid.get(0).indexOf('.');
        int endCol = grid.get(rows - 1).indexOf('.');

        int[] start = {0, startCol};
        int[] end = {rows - 1, endCol};

        Set<Long> junctions = findJunctions(grid, start, end);
        Map<Long, Map<Long, Integer>> graph = buildGraph(grid, junctions, respectSlopes);

        return longestPathDfs(graph, encode(start[0], start[1]), encode(end[0], end[1]));
    }

    /**
     * Encode row, col into a single long for efficient storage.
     */
    private static long encode(int r, int c) {
        return ((long) r << 16) | (c & 0xFFFF);
    }

    /**
     * Decode row from encoded value.
     */
    private static int decodeRow(long encoded) {
        return (int) (encoded >> 16);
    }

    /**
     * Decode column from encoded value.
     */
    private static int decodeCol(long encoded) {
        return (int) (encoded & 0xFFFF);
    }

    /**
     * Find all junction points (start, end, and intersections with 3+ neighbors).
     */
    private static Set<Long> findJunctions(List<String> grid, int[] start, int[] end) {
        int rows = grid.size();
        int cols = grid.get(0).length();
        Set<Long> junctions = new HashSet<>();

        // Add start and end
        junctions.add(encode(start[0], start[1]));
        junctions.add(encode(end[0], end[1]));

        // Find intersections
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid.get(r).charAt(c) == '#') continue;

                int neighbors = 0;
                for (int d = 0; d < 4; d++) {
                    int nr = r + DR[d];
                    int nc = c + DC[d];
                    if (nr >= 0 && nr < rows && nc >= 0 && nc < cols
                            && grid.get(nr).charAt(nc) != '#') {
                        neighbors++;
                    }
                }

                if (neighbors >= 3) {
                    junctions.add(encode(r, c));
                }
            }
        }

        return junctions;
    }

    /**
     * Build a compressed graph of junctions with edge weights (distances).
     */
    private static Map<Long, Map<Long, Integer>> buildGraph(
            List<String> grid, Set<Long> junctions, boolean respectSlopes) {
        int rows = grid.size();
        int cols = grid.get(0).length();

        Map<Long, Map<Long, Integer>> graph = new HashMap<>();
        for (long junction : junctions) {
            graph.put(junction, new HashMap<>());
        }

        for (long startJunction : junctions) {
            int startR = decodeRow(startJunction);
            int startC = decodeCol(startJunction);

            // DFS/BFS from each junction to find reachable junctions
            Deque<int[]> stack = new ArrayDeque<>();
            Set<Long> visited = new HashSet<>();

            stack.push(new int[]{startR, startC, 0});
            visited.add(startJunction);

            while (!stack.isEmpty()) {
                int[] current = stack.pop();
                int r = current[0];
                int c = current[1];
                int dist = current[2];

                long encoded = encode(r, c);
                if (dist > 0 && junctions.contains(encoded)) {
                    // Found another junction
                    graph.get(startJunction).put(encoded, dist);
                    continue;
                }

                // Explore neighbors
                for (int d = 0; d < 4; d++) {
                    int dr = DR[d];
                    int dc = DC[d];
                    int nr = r + dr;
                    int nc = c + dc;

                    if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) continue;
                    if (grid.get(nr).charAt(nc) == '#') continue;

                    long neighborEncoded = encode(nr, nc);
                    if (visited.contains(neighborEncoded)) continue;

                    // Check slope constraints for Part 1
                    if (respectSlopes) {
                        char cell = grid.get(r).charAt(c);
                        if (SLOPE_DIRS.containsKey(cell)) {
                            int[] reqDir = SLOPE_DIRS.get(cell);
                            if (dr != reqDir[0] || dc != reqDir[1]) {
                                continue;
                            }
                        }
                    }

                    visited.add(neighborEncoded);
                    stack.push(new int[]{nr, nc, dist + 1});
                }
            }
        }

        return graph;
    }

    /**
     * Find longest path using DFS with backtracking.
     */
    private static int longestPathDfs(Map<Long, Map<Long, Integer>> graph, long start, long end) {
        Set<Long> visited = new HashSet<>();
        return dfs(graph, start, end, visited);
    }

    private static int dfs(Map<Long, Map<Long, Integer>> graph, long node, long end, Set<Long> visited) {
        if (node == end) {
            return 0;
        }

        visited.add(node);
        int maxDist = Integer.MIN_VALUE;

        Map<Long, Integer> neighbors = graph.get(node);
        if (neighbors != null) {
            for (Map.Entry<Long, Integer> entry : neighbors.entrySet()) {
                long neighbor = entry.getKey();
                int dist = entry.getValue();

                if (!visited.contains(neighbor)) {
                    int result = dfs(graph, neighbor, end, visited);
                    if (result != Integer.MIN_VALUE) {
                        maxDist = Math.max(maxDist, dist + result);
                    }
                }
            }
        }

        visited.remove(node);
        return maxDist;
    }
}
