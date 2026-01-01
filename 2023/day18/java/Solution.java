import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * Day 18: Lavaduct Lagoon
 * Polygon area calculation using Shoelace formula and Pick's theorem.
 */
public class Solution {

    record Instruction(char direction, int distance, String color) {}

    public static void main(String[] args) throws IOException {
        List<Instruction> instructions = parseInput(Path.of("../input.txt"));
        System.out.println("Part 1: " + part1(instructions));
        System.out.println("Part 2: " + part2(instructions));
    }

    static List<Instruction> parseInput(Path path) throws IOException {
        List<Instruction> instructions = new ArrayList<>();
        for (String line : Files.readAllLines(path)) {
            String[] parts = line.split(" ");
            char direction = parts[0].charAt(0);
            int distance = Integer.parseInt(parts[1]);
            String color = parts[2].substring(2, parts[2].length() - 1); // Remove (# and )
            instructions.add(new Instruction(direction, distance, color));
        }
        return instructions;
    }

    /**
     * Calculate total area using Shoelace formula and Pick's theorem.
     *
     * Shoelace gives us twice the signed area of the polygon.
     * Pick's theorem: A = i + b/2 - 1, where i = interior points, b = boundary points
     * We want: Total = i + b = A + b/2 + 1
     */
    static long calculateArea(List<long[]> vertices, long perimeter) {
        int n = vertices.size();
        long area = 0;
        for (int i = 0; i < n; i++) {
            int j = (i + 1) % n;
            area += vertices.get(i)[0] * vertices.get(j)[1];
            area -= vertices.get(j)[0] * vertices.get(i)[1];
        }
        area = Math.abs(area) / 2;

        // Total points = interior + boundary
        // From Pick's theorem: interior = area - boundary/2 + 1
        // Total = interior + boundary = area + boundary/2 + 1
        return area + perimeter / 2 + 1;
    }

    static long part1(List<Instruction> instructions) {
        Map<Character, int[]> directionMap = Map.of(
            'R', new int[]{0, 1},
            'D', new int[]{1, 0},
            'L', new int[]{0, -1},
            'U', new int[]{-1, 0}
        );

        List<long[]> vertices = new ArrayList<>();
        vertices.add(new long[]{0, 0});
        long perimeter = 0;
        long r = 0, c = 0;

        for (Instruction instr : instructions) {
            int[] delta = directionMap.get(instr.direction());
            r += (long) delta[0] * instr.distance();
            c += (long) delta[1] * instr.distance();
            vertices.add(new long[]{r, c});
            perimeter += instr.distance();
        }

        return calculateArea(vertices, perimeter);
    }

    static long part2(List<Instruction> instructions) {
        // Last digit of hex: 0=R, 1=D, 2=L, 3=U
        // First 5 digits: distance in hex
        Map<Character, int[]> directionMap = Map.of(
            '0', new int[]{0, 1},   // R
            '1', new int[]{1, 0},   // D
            '2', new int[]{0, -1},  // L
            '3', new int[]{-1, 0}   // U
        );

        List<long[]> vertices = new ArrayList<>();
        vertices.add(new long[]{0, 0});
        long perimeter = 0;
        long r = 0, c = 0;

        for (Instruction instr : instructions) {
            String color = instr.color();
            long distance = Long.parseLong(color.substring(0, 5), 16);
            char direction = color.charAt(5);
            int[] delta = directionMap.get(direction);
            r += delta[0] * distance;
            c += delta[1] * distance;
            vertices.add(new long[]{r, c});
            perimeter += distance;
        }

        return calculateArea(vertices, perimeter);
    }
}
