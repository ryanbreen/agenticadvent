import java.io.*;
import java.util.*;

public class Solution {
    static class Point {
        final int r;
        final int c;

        Point(int r, int c) {
            this.r = r;
            this.c = c;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Point point = (Point) o;
            return r == point.r && c == point.c;
        }

        @Override
        public int hashCode() {
            return Objects.hash(r, c);
        }
    }

    static class Input {
        final int rows;
        final int cols;
        final Map<Character, List<Point>> antennas;

        Input(int rows, int cols, Map<Character, List<Point>> antennas) {
            this.rows = rows;
            this.cols = cols;
            this.antennas = antennas;
        }
    }

    private static boolean inBounds(int r, int c, int rows, int cols) {
        return r >= 0 && r < rows && c >= 0 && c < cols;
    }

    static Input parseInput(String filename) throws IOException {
        List<String> grid = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                grid.add(line);
            }
        }

        int rows = grid.size();
        int cols = rows > 0 ? grid.get(0).length() : 0;

        // Group antenna positions by frequency
        Map<Character, List<Point>> antennas = new HashMap<>();
        for (int r = 0; r < rows; r++) {
            String row = grid.get(r);
            for (int c = 0; c < row.length(); c++) {
                char ch = row.charAt(c);
                if (ch != '.') {
                    antennas.computeIfAbsent(ch, k -> new ArrayList<>()).add(new Point(r, c));
                }
            }
        }

        return new Input(rows, cols, antennas);
    }

    static int part1() throws IOException {
        Input input = parseInput("../input.txt");
        Set<Point> antinodes = new HashSet<>();

        for (Map.Entry<Character, List<Point>> entry : input.antennas.entrySet()) {
            List<Point> positions = entry.getValue();

            // For each pair of antennas with same frequency
            for (int i = 0; i < positions.size(); i++) {
                for (int j = i + 1; j < positions.size(); j++) {
                    Point p1 = positions.get(i);
                    Point p2 = positions.get(j);

                    // Calculate the two antinodes
                    // Antinode beyond antenna 1 (away from antenna 2)
                    int ar1 = 2 * p1.r - p2.r;
                    int ac1 = 2 * p1.c - p2.c;
                    // Antinode beyond antenna 2 (away from antenna 1)
                    int ar2 = 2 * p2.r - p1.r;
                    int ac2 = 2 * p2.c - p1.c;

                    // Add if within bounds
                    if (inBounds(ar1, ac1, input.rows, input.cols)) {
                        antinodes.add(new Point(ar1, ac1));
                    }
                    if (inBounds(ar2, ac2, input.rows, input.cols)) {
                        antinodes.add(new Point(ar2, ac2));
                    }
                }
            }
        }

        return antinodes.size();
    }

    static int part2() throws IOException {
        Input input = parseInput("../input.txt");
        Set<Point> antinodes = new HashSet<>();

        for (Map.Entry<Character, List<Point>> entry : input.antennas.entrySet()) {
            List<Point> positions = entry.getValue();

            // For each pair of antennas with same frequency
            for (int i = 0; i < positions.size(); i++) {
                for (int j = i + 1; j < positions.size(); j++) {
                    Point p1 = positions.get(i);
                    Point p2 = positions.get(j);

                    int dr = p2.r - p1.r;
                    int dc = p2.c - p1.c;

                    // Extend in both directions along the line
                    // Direction 1: from antenna 1 towards and beyond antenna 2
                    int r = p1.r;
                    int c = p1.c;
                    while (inBounds(r, c, input.rows, input.cols)) {
                        antinodes.add(new Point(r, c));
                        r += dr;
                        c += dc;
                    }

                    // Direction 2: from antenna 1 away from antenna 2
                    r = p1.r - dr;
                    c = p1.c - dc;
                    while (inBounds(r, c, input.rows, input.cols)) {
                        antinodes.add(new Point(r, c));
                        r -= dr;
                        c -= dc;
                    }
                }
            }
        }

        return antinodes.size();
    }

    public static void main(String[] args) {
        try {
            System.out.println("Part 1: " + part1());
            System.out.println("Part 2: " + part2());
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
