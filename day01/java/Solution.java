import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Solution {
    private static List<String> lines;

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of(args.length > 0 ? args[0] : "../input.txt");
        lines = Files.readAllLines(inputPath);

        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static int part1() {
        int position = 50;
        int zeroCount = 0;

        for (String line : lines) {
            if (line.isEmpty()) continue;

            char direction = line.charAt(0);
            int distance = Integer.parseInt(line.substring(1));

            if (direction == 'L') {
                position = Math.floorMod(position - distance, 100);
            } else {
                position = Math.floorMod(position + distance, 100);
            }

            if (position == 0) {
                zeroCount++;
            }
        }

        return zeroCount;
    }

    private static long part2() {
        int position = 50;
        long zeroCount = 0;

        for (String line : lines) {
            if (line.isEmpty()) continue;

            char direction = line.charAt(0);
            int distance = Integer.parseInt(line.substring(1));

            if (direction == 'L') {
                // Moving left (toward lower numbers)
                if (position > 0 && distance >= position) {
                    zeroCount += 1 + (distance - position) / 100;
                } else if (position == 0 && distance >= 100) {
                    zeroCount += distance / 100;
                }
            } else {
                // Moving right (toward higher numbers)
                if (position > 0) {
                    int stepsToZero = 100 - position;
                    if (distance >= stepsToZero) {
                        zeroCount += 1 + (distance - stepsToZero) / 100;
                    }
                } else {
                    if (distance >= 100) {
                        zeroCount += distance / 100;
                    }
                }
            }

            // Update position
            if (direction == 'L') {
                position = Math.floorMod(position - distance, 100);
            } else {
                position = Math.floorMod(position + distance, 100);
            }
        }

        return zeroCount;
    }
}
