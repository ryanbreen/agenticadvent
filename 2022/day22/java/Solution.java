import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    private static String[] grid;
    private static List<Object> instructions;
    private static int height;
    private static int width;

    // Directions: 0=right, 1=down, 2=left, 3=up
    private static final int[] DR = {0, 1, 0, -1};
    private static final int[] DC = {1, 0, -1, 0};

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(args.length > 0 ? args[0] : "../input.txt");
        String text = Files.readString(inputPath);
        parseInput(text);

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static void parseInput(String text) {
        String[] parts = text.split("\n\n");
        String[] gridLines = parts[0].split("\n");
        String path = parts[1].trim();

        // Find dimensions
        height = gridLines.length;
        width = 0;
        for (String line : gridLines) {
            width = Math.max(width, line.length());
        }

        // Create grid (pad lines to consistent width)
        grid = new String[height];
        for (int i = 0; i < height; i++) {
            StringBuilder sb = new StringBuilder(gridLines[i]);
            while (sb.length() < width) {
                sb.append(' ');
            }
            grid[i] = sb.toString();
        }

        // Parse path into moves and turns
        instructions = new ArrayList<>();
        int i = 0;
        while (i < path.length()) {
            if (Character.isDigit(path.charAt(i))) {
                int j = i;
                while (j < path.length() && Character.isDigit(path.charAt(j))) {
                    j++;
                }
                instructions.add(Integer.parseInt(path.substring(i, j)));
                i = j;
            } else {
                instructions.add(path.charAt(i));
                i++;
            }
        }
    }

    private static long part1() {
        // Find starting position (leftmost open tile on top row)
        int row = 0;
        int col = grid[0].indexOf('.');
        int facing = 0; // Start facing right

        for (Object instr : instructions) {
            if (instr instanceof Integer) {
                int steps = (Integer) instr;
                for (int s = 0; s < steps; s++) {
                    int dr = DR[facing];
                    int dc = DC[facing];
                    int nr = row + dr;
                    int nc = col + dc;

                    // Wrap around if needed
                    if (facing == 0) { // Right
                        if (nc >= width || grid[nr].charAt(nc) == ' ') {
                            nc = 0;
                            while (grid[nr].charAt(nc) == ' ') {
                                nc++;
                            }
                        }
                    } else if (facing == 2) { // Left
                        if (nc < 0 || grid[nr].charAt(nc) == ' ') {
                            nc = width - 1;
                            while (grid[nr].charAt(nc) == ' ') {
                                nc--;
                            }
                        }
                    } else if (facing == 1) { // Down
                        if (nr >= height || grid[nr].charAt(nc) == ' ') {
                            nr = 0;
                            while (grid[nr].charAt(nc) == ' ') {
                                nr++;
                            }
                        }
                    } else if (facing == 3) { // Up
                        if (nr < 0 || grid[nr].charAt(nc) == ' ') {
                            nr = height - 1;
                            while (grid[nr].charAt(nc) == ' ') {
                                nr--;
                            }
                        }
                    }

                    // Check if we hit a wall
                    if (grid[nr].charAt(nc) == '#') {
                        break;
                    }

                    row = nr;
                    col = nc;
                }
            } else {
                char turn = (Character) instr;
                if (turn == 'R') {
                    facing = (facing + 1) % 4;
                } else {
                    facing = (facing + 3) % 4;
                }
            }
        }

        return 1000L * (row + 1) + 4L * (col + 1) + facing;
    }

    private static int[] getCubeFaceAndLocal(int row, int col, int faceSize) {
        int faceRow = row / faceSize;
        int faceCol = col / faceSize;
        int localR = row % faceSize;
        int localC = col % faceSize;

        int face;
        if (faceRow == 0 && faceCol == 1) {
            face = 1;
        } else if (faceRow == 0 && faceCol == 2) {
            face = 2;
        } else if (faceRow == 1 && faceCol == 1) {
            face = 3;
        } else if (faceRow == 2 && faceCol == 0) {
            face = 4;
        } else if (faceRow == 2 && faceCol == 1) {
            face = 5;
        } else if (faceRow == 3 && faceCol == 0) {
            face = 6;
        } else {
            face = -1;
        }

        return new int[]{face, localR, localC};
    }

    private static int[] wrapCube(int row, int col, int facing, int faceSize) {
        int S = faceSize;
        int[] faceInfo = getCubeFaceAndLocal(row, col, S);
        int face = faceInfo[0];
        int lr = faceInfo[1];
        int lc = faceInfo[2];

        if (face == 1) {
            if (facing == 3) { // Up: goes to face 6, from left, facing right
                return new int[]{3*S + lc, 0, 0};
            } else if (facing == 2) { // Left: goes to face 4, from left, facing right (inverted)
                return new int[]{3*S - 1 - lr, 0, 0};
            }
        } else if (face == 2) {
            if (facing == 0) { // Right: goes to face 5, from right, facing left (inverted)
                return new int[]{3*S - 1 - lr, 2*S - 1, 2};
            } else if (facing == 1) { // Down: goes to face 3, from right, facing left
                return new int[]{S + lc, 2*S - 1, 2};
            } else if (facing == 3) { // Up: goes to face 6, from bottom, facing up
                return new int[]{4*S - 1, lc, 3};
            }
        } else if (face == 3) {
            if (facing == 0) { // Right: goes to face 2, from bottom, facing up
                return new int[]{S - 1, 2*S + lr, 3};
            } else if (facing == 2) { // Left: goes to face 4, from top, facing down
                return new int[]{2*S, lr, 1};
            }
        } else if (face == 4) {
            if (facing == 3) { // Up: goes to face 3, from left, facing right
                return new int[]{S + lc, S, 0};
            } else if (facing == 2) { // Left: goes to face 1, from left, facing right (inverted)
                return new int[]{S - 1 - lr, S, 0};
            }
        } else if (face == 5) {
            if (facing == 0) { // Right: goes to face 2, from right, facing left (inverted)
                return new int[]{S - 1 - lr, 3*S - 1, 2};
            } else if (facing == 1) { // Down: goes to face 6, from right, facing left
                return new int[]{3*S + lc, S - 1, 2};
            }
        } else if (face == 6) {
            if (facing == 0) { // Right: goes to face 5, from bottom, facing up
                return new int[]{3*S - 1, S + lr, 3};
            } else if (facing == 1) { // Down: goes to face 2, from top, facing down
                return new int[]{0, 2*S + lc, 1};
            } else if (facing == 2) { // Left: goes to face 1, from top, facing down
                return new int[]{0, S + lr, 1};
            }
        }

        return new int[]{row, col, facing};
    }

    private static long part2() {
        // Determine face size
        int faceSize = height > 50 ? 50 : 4;

        // Find starting position
        int row = 0;
        int col = grid[0].indexOf('.');
        int facing = 0;

        for (Object instr : instructions) {
            if (instr instanceof Integer) {
                int steps = (Integer) instr;
                for (int s = 0; s < steps; s++) {
                    int dr = DR[facing];
                    int dc = DC[facing];
                    int nr = row + dr;
                    int nc = col + dc;
                    int nf = facing;

                    // Check if we need to wrap
                    boolean needWrap = false;
                    if (nr < 0 || nr >= height || nc < 0 || nc >= width) {
                        needWrap = true;
                    } else if (grid[nr].charAt(nc) == ' ') {
                        needWrap = true;
                    }

                    if (needWrap) {
                        int[] wrapped = wrapCube(row, col, facing, faceSize);
                        nr = wrapped[0];
                        nc = wrapped[1];
                        nf = wrapped[2];
                    }

                    // Check for wall
                    if (grid[nr].charAt(nc) == '#') {
                        break;
                    }

                    row = nr;
                    col = nc;
                    facing = nf;
                }
            } else {
                char turn = (Character) instr;
                if (turn == 'R') {
                    facing = (facing + 1) % 4;
                } else {
                    facing = (facing + 3) % 4;
                }
            }
        }

        return 1000L * (row + 1) + 4L * (col + 1) + facing;
    }
}
