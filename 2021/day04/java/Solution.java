import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    public static void main(String[] args) throws IOException {
        String content = Files.readString(Path.of("../input.txt")).trim();
        String[] sections = content.split("\n\n");

        // Parse numbers
        int[] numbers = Arrays.stream(sections[0].split(","))
                              .mapToInt(Integer::parseInt)
                              .toArray();

        // Parse boards
        List<int[][]> boards = new ArrayList<>();
        for (int i = 1; i < sections.length; i++) {
            int[][] board = new int[5][5];
            String[] lines = sections[i].trim().split("\n");
            for (int row = 0; row < 5; row++) {
                String[] nums = lines[row].trim().split("\\s+");
                for (int col = 0; col < 5; col++) {
                    board[row][col] = Integer.parseInt(nums[col]);
                }
            }
            boards.add(board);
        }

        System.out.println("Part 1: " + part1(numbers, boards));
        System.out.println("Part 2: " + part2(numbers, boards));
    }

    private static int part1(int[] numbers, List<int[][]> boards) {
        boolean[][][] marked = new boolean[boards.size()][5][5];

        for (int number : numbers) {
            for (int i = 0; i < boards.size(); i++) {
                markNumber(boards.get(i), marked[i], number);
                if (checkWinner(marked[i])) {
                    return calculateScore(boards.get(i), marked[i], number);
                }
            }
        }
        return -1;
    }

    private static int part2(int[] numbers, List<int[][]> boards) {
        boolean[][][] marked = new boolean[boards.size()][5][5];
        boolean[] won = new boolean[boards.size()];
        int lastScore = -1;

        for (int number : numbers) {
            for (int i = 0; i < boards.size(); i++) {
                if (won[i]) continue;

                markNumber(boards.get(i), marked[i], number);
                if (checkWinner(marked[i])) {
                    won[i] = true;
                    lastScore = calculateScore(boards.get(i), marked[i], number);
                }
            }
        }
        return lastScore;
    }

    private static void markNumber(int[][] board, boolean[][] marked, int number) {
        for (int row = 0; row < 5; row++) {
            for (int col = 0; col < 5; col++) {
                if (board[row][col] == number) {
                    marked[row][col] = true;
                }
            }
        }
    }

    private static boolean checkWinner(boolean[][] marked) {
        // Check rows
        for (int row = 0; row < 5; row++) {
            boolean allMarked = true;
            for (int col = 0; col < 5; col++) {
                if (!marked[row][col]) {
                    allMarked = false;
                    break;
                }
            }
            if (allMarked) return true;
        }

        // Check columns
        for (int col = 0; col < 5; col++) {
            boolean allMarked = true;
            for (int row = 0; row < 5; row++) {
                if (!marked[row][col]) {
                    allMarked = false;
                    break;
                }
            }
            if (allMarked) return true;
        }

        return false;
    }

    private static int calculateScore(int[][] board, boolean[][] marked, int lastNumber) {
        int unmarkedSum = 0;
        for (int row = 0; row < 5; row++) {
            for (int col = 0; col < 5; col++) {
                if (!marked[row][col]) {
                    unmarkedSum += board[row][col];
                }
            }
        }
        return unmarkedSum * lastNumber;
    }
}
