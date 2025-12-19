import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {
    private static final int SIZE = 71;
    private static final int INITIAL_BYTES = 1024;
    private static final int[] DX = {0, 0, 1, -1};
    private static final int[] DY = {1, -1, 0, 0};

    record Point(int x, int y) {}

    public static void main(String[] args) throws IOException {
        List<Point> positions = parseInput("../input.txt");

        System.out.println("Part 1: " + part1(positions));
        System.out.println("Part 2: " + part2(positions));
    }

    private static List<Point> parseInput(String filename) throws IOException {
        return Files.lines(Path.of(filename))
                .map(String::trim)
                .filter(line -> !line.isEmpty())
                .map(line -> {
                    String[] parts = line.split(",");
                    return new Point(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]));
                })
                .toList();
    }

    private static int bfs(Set<Point> corrupted) {
        Point start = new Point(0, 0);
        Point goal = new Point(SIZE - 1, SIZE - 1);

        if (corrupted.contains(start) || corrupted.contains(goal)) {
            return -1;
        }

        record State(Point pos, int steps) {}

        Queue<State> queue = new ArrayDeque<>();
        queue.offer(new State(start, 0));
        Set<Point> visited = new HashSet<>();
        visited.add(start);

        while (!queue.isEmpty()) {
            State current = queue.poll();

            if (current.pos().equals(goal)) {
                return current.steps();
            }

            int x = current.pos().x();
            int y = current.pos().y();

            for (int i = 0; i < 4; i++) {
                int nx = x + DX[i];
                int ny = y + DY[i];

                if (nx >= 0 && nx < SIZE && ny >= 0 && ny < SIZE) {
                    Point newPos = new Point(nx, ny);
                    if (!visited.contains(newPos) && !corrupted.contains(newPos)) {
                        visited.add(newPos);
                        queue.offer(new State(newPos, current.steps() + 1));
                    }
                }
            }
        }

        return -1;
    }

    private static Set<Point> buildCorruptedSet(List<Point> positions, int count) {
        Set<Point> corrupted = new HashSet<>();
        for (int i = 0; i < count && i < positions.size(); i++) {
            corrupted.add(positions.get(i));
        }
        return corrupted;
    }

    private static int part1(List<Point> positions) {
        Set<Point> corrupted = buildCorruptedSet(positions, INITIAL_BYTES);
        return bfs(corrupted);
    }

    private static String part2(List<Point> positions) {
        int left = 0;
        int right = positions.size();

        while (left < right) {
            int mid = (left + right) / 2;
            Set<Point> corrupted = buildCorruptedSet(positions, mid + 1);
            if (bfs(corrupted) == -1) {
                right = mid;
            } else {
                left = mid + 1;
            }
        }

        Point blockingPos = positions.get(left);
        return blockingPos.x() + "," + blockingPos.y();
    }
}
