import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    public static void main(String[] args) throws IOException {
        String inputFile = args.length > 0 ? args[0] : "../input.txt";
        List<String> lines = Files.readAllLines(Paths.get(inputFile));

        // Remove trailing empty lines
        while (!lines.isEmpty() && lines.get(lines.size() - 1).isEmpty()) {
            lines.remove(lines.size() - 1);
        }

        System.out.println("Part 1: " + solve(lines, 2));
        System.out.println("Part 2: " + solve(lines, 1_000_000));
    }

    private static long solve(List<String> lines, int expansionFactor) {
        List<int[]> galaxies = parseGalaxies(lines);
        Set<Integer> emptyRows = findEmptyRows(lines);
        Set<Integer> emptyCols = findEmptyCols(lines);

        return calculateDistances(galaxies, emptyRows, emptyCols, expansionFactor);
    }

    private static List<int[]> parseGalaxies(List<String> lines) {
        List<int[]> galaxies = new ArrayList<>();
        for (int r = 0; r < lines.size(); r++) {
            String line = lines.get(r);
            for (int c = 0; c < line.length(); c++) {
                if (line.charAt(c) == '#') {
                    galaxies.add(new int[]{r, c});
                }
            }
        }
        return galaxies;
    }

    private static Set<Integer> findEmptyRows(List<String> lines) {
        Set<Integer> emptyRows = new HashSet<>();
        for (int r = 0; r < lines.size(); r++) {
            if (!lines.get(r).contains("#")) {
                emptyRows.add(r);
            }
        }
        return emptyRows;
    }

    private static Set<Integer> findEmptyCols(List<String> lines) {
        Set<Integer> emptyCols = new HashSet<>();
        if (lines.isEmpty()) return emptyCols;

        int cols = lines.get(0).length();
        for (int c = 0; c < cols; c++) {
            boolean hasGalaxy = false;
            for (String line : lines) {
                if (c < line.length() && line.charAt(c) == '#') {
                    hasGalaxy = true;
                    break;
                }
            }
            if (!hasGalaxy) {
                emptyCols.add(c);
            }
        }
        return emptyCols;
    }

    private static long calculateDistances(List<int[]> galaxies, Set<Integer> emptyRows,
                                           Set<Integer> emptyCols, int expansionFactor) {
        long total = 0;

        for (int i = 0; i < galaxies.size(); i++) {
            for (int j = i + 1; j < galaxies.size(); j++) {
                int[] g1 = galaxies.get(i);
                int[] g2 = galaxies.get(j);

                int r1 = g1[0], c1 = g1[1];
                int r2 = g2[0], c2 = g2[1];

                // Calculate row distance with expansion
                int minR = Math.min(r1, r2);
                int maxR = Math.max(r1, r2);
                long rowDist = maxR - minR;
                for (int r = minR; r < maxR; r++) {
                    if (emptyRows.contains(r)) {
                        rowDist += expansionFactor - 1;
                    }
                }

                // Calculate column distance with expansion
                int minC = Math.min(c1, c2);
                int maxC = Math.max(c1, c2);
                long colDist = maxC - minC;
                for (int c = minC; c < maxC; c++) {
                    if (emptyCols.contains(c)) {
                        colDist += expansionFactor - 1;
                    }
                }

                total += rowDist + colDist;
            }
        }

        return total;
    }
}
