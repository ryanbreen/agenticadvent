import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class Solution {
    public static void main(String[] args) throws IOException {
        byte[] data = Files.readAllBytes(Path.of(args.length > 0 ? args[0] : "../input.txt"));

        int position = 50;
        int part1 = 0;
        long part2 = 0;

        int i = 0;
        int len = data.length;

        while (i < len) {
            // Skip whitespace/newlines
            while (i < len && (data[i] == '\n' || data[i] == '\r' || data[i] == ' ')) i++;
            if (i >= len) break;

            // Read direction
            int dir = (data[i] == 'L') ? -1 : 1;
            i++;

            // Parse distance manually
            int distance = 0;
            while (i < len && data[i] >= '0' && data[i] <= '9') {
                distance = distance * 10 + (data[i] - '0');
                i++;
            }

            // Part 2: count zero crossings
            if (dir == -1) {
                // Moving left: hit 0 at position, position+100, position+200, ...
                if (position > 0 && distance >= position) {
                    part2 += 1 + (distance - position) / 100;
                } else if (position == 0 && distance >= 100) {
                    part2 += distance / 100;
                }
            } else {
                // Moving right: hit 0 at (100-position), (100-position)+100, ...
                int stepsToZero = 100 - position;
                if (position > 0 && distance >= stepsToZero) {
                    part2 += 1 + (distance - stepsToZero) / 100;
                } else if (position == 0 && distance >= 100) {
                    part2 += distance / 100;
                }
            }

            // Update position with proper modulo
            position += dir * distance;
            position = ((position % 100) + 100) % 100;

            // Part 1: count ending at zero
            if (position == 0) part1++;
        }

        System.out.println("Part 1: " + part1);
        System.out.println("Part 2: " + part2);
    }
}
