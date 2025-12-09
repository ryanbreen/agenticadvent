import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Solution {
    private static class Point {
        int x, y;

        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }
    }

    private static class HorizontalEdge {
        int y, xMin, xMax;

        HorizontalEdge(int y, int xMin, int xMax) {
            this.y = y;
            this.xMin = xMin;
            this.xMax = xMax;
        }
    }

    private static class VerticalEdge implements Comparable<VerticalEdge> {
        int x, yMin, yMax;

        VerticalEdge(int x, int yMin, int yMax) {
            this.x = x;
            this.yMin = yMin;
            this.yMax = yMax;
        }

        @Override
        public int compareTo(VerticalEdge other) {
            return Integer.compare(this.x, other.x);
        }
    }

    private static List<Point> points;

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Paths.get("../input.txt")).trim();

        points = new ArrayList<>();
        for (String line : input.split("\n")) {
            String[] parts = line.split(",");
            int x = Integer.parseInt(parts[0]);
            int y = Integer.parseInt(parts[1]);
            points.add(new Point(x, y));
        }

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static long part1() {
        long maxArea = 0;
        int n = points.size();

        // Check all pairs of points as opposite corners
        for (int i = 0; i < n; i++) {
            Point p1 = points.get(i);
            for (int j = i + 1; j < n; j++) {
                Point p2 = points.get(j);

                // Rectangle area = width * height (inclusive of both corners)
                long width = Math.abs(p2.x - p1.x) + 1;
                long height = Math.abs(p2.y - p1.y) + 1;
                long area = width * height;
                maxArea = Math.max(maxArea, area);
            }
        }

        return maxArea;
    }

    private static long part2() {
        int n = points.size();
        List<HorizontalEdge> horizontalEdges = new ArrayList<>();
        List<VerticalEdge> verticalEdges = new ArrayList<>();

        // Build edges connecting consecutive points
        for (int i = 0; i < n; i++) {
            Point p1 = points.get(i);
            Point p2 = points.get((i + 1) % n);

            if (p1.y == p2.y) {  // Horizontal edge
                horizontalEdges.add(new HorizontalEdge(p1.y, Math.min(p1.x, p2.x), Math.max(p1.x, p2.x)));
            } else {  // Vertical edge
                verticalEdges.add(new VerticalEdge(p1.x, Math.min(p1.y, p2.y), Math.max(p1.y, p2.y)));
            }
        }

        // Sort vertical edges by x coordinate for efficient lookup
        Collections.sort(verticalEdges);

        // Build maps for efficient edge lookup
        Map<Integer, List<VerticalEdge>> vertByX = new HashMap<>();
        for (VerticalEdge edge : verticalEdges) {
            vertByX.computeIfAbsent(edge.x, k -> new ArrayList<>()).add(edge);
        }

        Map<Integer, List<HorizontalEdge>> horizByY = new HashMap<>();
        for (HorizontalEdge edge : horizontalEdges) {
            horizByY.computeIfAbsent(edge.y, k -> new ArrayList<>()).add(edge);
        }

        // Find largest valid rectangle with red corners
        long maxArea = 0;

        for (int i = 0; i < n; i++) {
            Point p1 = points.get(i);
            for (int j = i + 1; j < n; j++) {
                Point p2 = points.get(j);

                if (rectangleValid(p1.x, p1.y, p2.x, p2.y, vertByX, horizByY)) {
                    long width = Math.abs(p2.x - p1.x) + 1;
                    long height = Math.abs(p2.y - p1.y) + 1;
                    long area = width * height;
                    maxArea = Math.max(maxArea, area);
                }
            }
        }

        return maxArea;
    }

    private static boolean rectangleValid(int x1, int y1, int x2, int y2,
                                         Map<Integer, List<VerticalEdge>> vertByX,
                                         Map<Integer, List<HorizontalEdge>> horizByY) {
        int minX = Math.min(x1, x2);
        int maxX = Math.max(x1, x2);
        int minY = Math.min(y1, y2);
        int maxY = Math.max(y1, y2);

        // Check if any vertical edge crosses through the rectangle interior
        for (Map.Entry<Integer, List<VerticalEdge>> entry : vertByX.entrySet()) {
            int vx = entry.getKey();
            if (minX < vx && vx < maxX) {  // Vertical edge x is inside rectangle's x range
                for (VerticalEdge edge : entry.getValue()) {
                    // Check if this edge segment overlaps with rectangle's y range
                    if (!(edge.yMax <= minY || edge.yMin >= maxY)) {
                        return false;
                    }
                }
            }
        }

        // Check if any horizontal edge crosses through the rectangle interior
        for (Map.Entry<Integer, List<HorizontalEdge>> entry : horizByY.entrySet()) {
            int hy = entry.getKey();
            if (minY < hy && hy < maxY) {  // Horizontal edge y is inside rectangle's y range
                for (HorizontalEdge edge : entry.getValue()) {
                    // Check if this edge segment overlaps with rectangle's x range
                    if (!(edge.xMax <= minX || edge.xMin >= maxX)) {
                        return false;
                    }
                }
            }
        }

        // Finally, check that we're inside the polygon (not outside)
        // Check center point using ray casting
        double centerX = (minX + maxX) / 2.0;
        double centerY = (minY + maxY) / 2.0;
        return isInsidePolygon(centerX, centerY, vertByX);
    }

    private static boolean isInsidePolygon(double x, double y, Map<Integer, List<VerticalEdge>> vertByX) {
        double crossings = 0;

        // Cast ray to the right
        List<Integer> sortedX = new ArrayList<>(vertByX.keySet());
        Collections.sort(sortedX);

        for (int vx : sortedX) {
            if (vx <= x) {
                continue;
            }

            for (VerticalEdge edge : vertByX.get(vx)) {
                if (edge.yMin < y && y < edge.yMax) {
                    // Ray crosses edge (strict inequality)
                    crossings += 1;
                } else if (y == edge.yMin || y == edge.yMax) {
                    // On corner - count as 0.5 crossing
                    crossings += 0.5;
                }
            }
        }

        return crossings % 2 == 1;
    }
}
