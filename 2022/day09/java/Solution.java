import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    private static final Map<Character, int[]> DIRECTIONS = Map.of(
        'U', new int[]{0, 1},
        'D', new int[]{0, -1},
        'L', new int[]{-1, 0},
        'R', new int[]{1, 0}
    );

    private static int sign(int x) {
        return Integer.compare(x, 0);
    }

    private static int[] moveTail(int[] head, int[] tail) {
        int dx = head[0] - tail[0];
        int dy = head[1] - tail[1];

        // If adjacent or overlapping, don't move
        if (Math.abs(dx) <= 1 && Math.abs(dy) <= 1) {
            return tail;
        }

        // Move toward head
        return new int[]{tail[0] + sign(dx), tail[1] + sign(dy)};
    }

    private static int simulateRope(List<String> moves, int ropeLength) {
        int[][] knots = new int[ropeLength][2];
        Set<String> visited = new HashSet<>();
        visited.add(knots[ropeLength - 1][0] + "," + knots[ropeLength - 1][1]);

        for (String line : moves) {
            String[] parts = line.split(" ");
            char direction = parts[0].charAt(0);
            int count = Integer.parseInt(parts[1]);
            int[] delta = DIRECTIONS.get(direction);

            for (int step = 0; step < count; step++) {
                // Move head
                knots[0][0] += delta[0];
                knots[0][1] += delta[1];

                // Move each subsequent knot
                for (int i = 1; i < ropeLength; i++) {
                    int[] newPos = moveTail(knots[i - 1], knots[i]);
                    knots[i][0] = newPos[0];
                    knots[i][1] = newPos[1];
                }

                // Record tail position
                visited.add(knots[ropeLength - 1][0] + "," + knots[ropeLength - 1][1]);
            }
        }

        return visited.size();
    }

    private static int part1(List<String> moves) {
        return simulateRope(moves, 2);
    }

    private static int part2(List<String> moves) {
        return simulateRope(moves, 10);
    }

    public static void main(String[] args) throws IOException {
        Path scriptDir = Paths.get(System.getProperty("user.dir"));
        Path inputFile = scriptDir.resolve("../input.txt").normalize();

        List<String> moves = Files.readAllLines(inputFile);

        System.out.println("Part 1: " + part1(moves));
        System.out.println("Part 2: " + part2(moves));
    }
}
