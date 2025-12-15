import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Solution {

    record Pos(int r, int c) {}

    record ParseResult(char[][] grid, String moves) {}

    private static final int GPS_ROW_MULTIPLIER = 100;

    static ParseResult parseInput(String text) {
        String[] parts = text.split("\n\n");
        String[] gridLines = parts[0].split("\n");
        char[][] grid = new char[gridLines.length][];
        for (int i = 0; i < gridLines.length; i++) {
            grid[i] = gridLines[i].toCharArray();
        }
        String moves = parts[1].replaceAll("\n", "");
        return new ParseResult(grid, moves);
    }

    static Optional<Pos> findRobot(char[][] grid) {
        for (int r = 0; r < grid.length; r++) {
            for (int c = 0; c < grid[r].length; c++) {
                if (grid[r][c] == '@') {
                    return Optional.of(new Pos(r, c));
                }
            }
        }
        return Optional.empty();
    }

    static Pos moveRobot(char[][] grid, Pos robotPos, char direction) {
        int dr = switch(direction) {
            case '^' -> -1;
            case 'v' -> 1;
            default -> 0;
        };
        int dc = switch(direction) {
            case '<' -> -1;
            case '>' -> 1;
            default -> 0;
        };

        int r = robotPos.r;
        int c = robotPos.c;
        int nr = r + dr;
        int nc = c + dc;

        if (grid[nr][nc] == '#') {
            return robotPos;
        }

        if (grid[nr][nc] == '.') {
            grid[r][c] = '.';
            grid[nr][nc] = '@';
            return new Pos(nr, nc);
        }

        if (grid[nr][nc] == 'O') {
            int checkR = nr;
            int checkC = nc;
            while (grid[checkR][checkC] == 'O') {
                checkR += dr;
                checkC += dc;
            }

            if (grid[checkR][checkC] == '#') {
                return robotPos;
            }

            grid[checkR][checkC] = 'O';
            grid[r][c] = '.';
            grid[nr][nc] = '@';
            return new Pos(nr, nc);
        }

        return robotPos;
    }

    static int calculateGPS(char[][] grid, char boxChar) {
        int total = 0;
        for (int r = 0; r < grid.length; r++) {
            for (int c = 0; c < grid[r].length; c++) {
                if (grid[r][c] == boxChar) {
                    total += GPS_ROW_MULTIPLIER * r + c;
                }
            }
        }
        return total;
    }

    static int part1(String input) {
        ParseResult parsed = parseInput(input);
        char[][] grid = parsed.grid();
        String moves = parsed.moves();
        Pos robotPos = findRobot(grid).orElseThrow();

        for (char move : moves.toCharArray()) {
            robotPos = moveRobot(grid, robotPos, move);
        }

        return calculateGPS(grid, 'O');
    }

    static char[][] scaleGrid(char[][] grid) {
        int height = grid.length;
        int width = grid[0].length;
        char[][] newGrid = new char[height][width * 2];

        for (int r = 0; r < height; r++) {
            for (int c = 0; c < width; c++) {
                char cell = grid[r][c];
                int nc = c * 2;
                if (cell == '#') {
                    newGrid[r][nc] = '#';
                    newGrid[r][nc + 1] = '#';
                } else if (cell == 'O') {
                    newGrid[r][nc] = '[';
                    newGrid[r][nc + 1] = ']';
                } else if (cell == '.') {
                    newGrid[r][nc] = '.';
                    newGrid[r][nc + 1] = '.';
                } else if (cell == '@') {
                    newGrid[r][nc] = '@';
                    newGrid[r][nc + 1] = '.';
                }
            }
        }
        return newGrid;
    }

    static boolean canMoveBoxVertical(char[][] grid, int boxLeftC, int r, int dr) {
        int nr = r + dr;
        int leftC = boxLeftC;
        int rightC = boxLeftC + 1;

        char leftTarget = grid[nr][leftC];
        char rightTarget = grid[nr][rightC];

        if (leftTarget == '#' || rightTarget == '#') {
            return false;
        }

        Set<Pos> boxesToCheck = new HashSet<>();

        if (leftTarget == '[') {
            boxesToCheck.add(new Pos(nr, leftC));
        } else if (leftTarget == ']') {
            boxesToCheck.add(new Pos(nr, leftC - 1));
        }

        if (rightTarget == '[') {
            boxesToCheck.add(new Pos(nr, rightC));
        } else if (rightTarget == ']') {
            boxesToCheck.add(new Pos(nr, rightC - 1));
        }

        for (Pos box : boxesToCheck) {
            if (!canMoveBoxVertical(grid, box.c, box.r, dr)) {
                return false;
            }
        }

        return true;
    }

    static void collectBoxesVertical(char[][] grid, int boxLeftC, int r, int dr, Set<Pos> collected) {
        Pos current = new Pos(r, boxLeftC);
        collected.add(current);

        int nr = r + dr;
        int leftC = boxLeftC;
        int rightC = boxLeftC + 1;

        char leftTarget = grid[nr][leftC];
        char rightTarget = grid[nr][rightC];

        Set<Pos> boxesToCheck = new HashSet<>();

        if (leftTarget == '[') {
            boxesToCheck.add(new Pos(nr, leftC));
        } else if (leftTarget == ']') {
            boxesToCheck.add(new Pos(nr, leftC - 1));
        }

        if (rightTarget == '[') {
            boxesToCheck.add(new Pos(nr, rightC));
        } else if (rightTarget == ']') {
            boxesToCheck.add(new Pos(nr, rightC - 1));
        }

        for (Pos box : boxesToCheck) {
            if (!collected.contains(box)) {
                collectBoxesVertical(grid, box.c, box.r, dr, collected);
            }
        }
    }

    static Pos moveRobotWide(char[][] grid, Pos robotPos, char direction) {
        int dr = switch(direction) {
            case '^' -> -1;
            case 'v' -> 1;
            default -> 0;
        };
        int dc = switch(direction) {
            case '<' -> -1;
            case '>' -> 1;
            default -> 0;
        };

        int r = robotPos.r;
        int c = robotPos.c;
        int nr = r + dr;
        int nc = c + dc;

        char target = grid[nr][nc];

        if (target == '#') {
            return robotPos;
        }

        if (target == '.') {
            grid[r][c] = '.';
            grid[nr][nc] = '@';
            return new Pos(nr, nc);
        }

        if (target == '[' || target == ']') {
            if (dc != 0) { // Horizontal movement
                int checkC = nc;
                while (grid[r][checkC] == '[' || grid[r][checkC] == ']') {
                    checkC += dc;
                }

                if (grid[r][checkC] == '#') {
                    return robotPos;
                }

                // Shift all boxes
                if (dc > 0) { // Moving right
                    for (int col = checkC; col > nc; col--) {
                        grid[r][col] = grid[r][col - 1];
                    }
                } else { // Moving left
                    for (int col = checkC; col < nc; col++) {
                        grid[r][col] = grid[r][col + 1];
                    }
                }

                grid[r][c] = '.';
                grid[nr][nc] = '@';
                return new Pos(nr, nc);

            } else { // Vertical movement
                int boxLeftC = (target == '[') ? nc : nc - 1;

                if (!canMoveBoxVertical(grid, boxLeftC, nr, dr)) {
                    return robotPos;
                }

                Set<Pos> boxesToMove = new HashSet<>();
                collectBoxesVertical(grid, boxLeftC, nr, dr, boxesToMove);

                // Sort boxes by row
                List<Pos> sortedBoxes = new ArrayList<>(boxesToMove);
                sortedBoxes.sort((a, b) -> dr > 0 ? Integer.compare(b.r, a.r) : Integer.compare(a.r, b.r));

                // Move all boxes
                for (Pos box : sortedBoxes) {
                    grid[box.r][box.c] = '.';
                    grid[box.r][box.c + 1] = '.';
                    grid[box.r + dr][box.c] = '[';
                    grid[box.r + dr][box.c + 1] = ']';
                }

                // Move robot
                grid[r][c] = '.';
                grid[nr][nc] = '@';
                return new Pos(nr, nc);
            }
        }

        return robotPos;
    }

    static int part2(String input) {
        ParseResult parsed = parseInput(input);
        char[][] grid = scaleGrid(parsed.grid());
        String moves = parsed.moves();
        Pos robotPos = findRobot(grid).orElseThrow();

        for (char move : moves.toCharArray()) {
            robotPos = moveRobotWide(grid, robotPos, move);
        }

        return calculateGPS(grid, '[');
    }

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt")).strip();

        System.out.println("Part 1: " + part1(input));
        System.out.println("Part 2: " + part2(input));
    }
}
