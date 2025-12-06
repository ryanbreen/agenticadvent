import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Solution {
    private static List<String> grid;
    private static int rows;
    private static int cols;

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Paths.get("../input.txt")).strip();
        grid = Arrays.asList(input.split("\n"));
        rows = grid.size();
        cols = grid.get(0).length();

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static int part1() {
        int sum = 0;

        for (int row = 0; row < rows; row++) {
            String line = grid.get(row);
            int col = 0;

            while (col < cols) {
                if (Character.isDigit(line.charAt(col))) {
                    // Found start of a number
                    int startCol = col;
                    StringBuilder numStr = new StringBuilder();

                    // Extract the full number
                    while (col < cols && Character.isDigit(line.charAt(col))) {
                        numStr.append(line.charAt(col));
                        col++;
                    }

                    int endCol = col - 1;
                    int number = Integer.parseInt(numStr.toString());

                    // Check if this number is adjacent to any symbol
                    if (isAdjacentToSymbol(row, startCol, endCol)) {
                        sum += number;
                    }
                } else {
                    col++;
                }
            }
        }

        return sum;
    }

    private static boolean isAdjacentToSymbol(int row, int startCol, int endCol) {
        // Check all cells adjacent to the number (including diagonals)
        for (int r = row - 1; r <= row + 1; r++) {
            for (int c = startCol - 1; c <= endCol + 1; c++) {
                if (r >= 0 && r < rows && c >= 0 && c < cols) {
                    char ch = grid.get(r).charAt(c);
                    if (!Character.isDigit(ch) && ch != '.') {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private static int part2() {
        // Map from gear position to list of adjacent numbers
        Map<String, List<Integer>> gearNumbers = new HashMap<>();

        for (int row = 0; row < rows; row++) {
            String line = grid.get(row);
            int col = 0;

            while (col < cols) {
                if (Character.isDigit(line.charAt(col))) {
                    // Found start of a number
                    int startCol = col;
                    StringBuilder numStr = new StringBuilder();

                    // Extract the full number
                    while (col < cols && Character.isDigit(line.charAt(col))) {
                        numStr.append(line.charAt(col));
                        col++;
                    }

                    int endCol = col - 1;
                    int number = Integer.parseInt(numStr.toString());

                    // Find all adjacent gears (*) for this number
                    Set<String> adjacentGears = findAdjacentGears(row, startCol, endCol);
                    for (String gearPos : adjacentGears) {
                        gearNumbers.computeIfAbsent(gearPos, k -> new ArrayList<>()).add(number);
                    }
                } else {
                    col++;
                }
            }
        }

        // Sum up gear ratios for gears with exactly 2 adjacent numbers
        int sum = 0;
        for (List<Integer> numbers : gearNumbers.values()) {
            if (numbers.size() == 2) {
                sum += numbers.get(0) * numbers.get(1);
            }
        }

        return sum;
    }

    private static Set<String> findAdjacentGears(int row, int startCol, int endCol) {
        Set<String> gears = new HashSet<>();

        // Check all cells adjacent to the number
        for (int r = row - 1; r <= row + 1; r++) {
            for (int c = startCol - 1; c <= endCol + 1; c++) {
                if (r >= 0 && r < rows && c >= 0 && c < cols) {
                    char ch = grid.get(r).charAt(c);
                    if (ch == '*') {
                        gears.add(r + "," + c);
                    }
                }
            }
        }

        return gears;
    }
}
