import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Solution {

    public static void main(String[] args) throws IOException {
        String content = Files.readString(Path.of("../input.txt")).strip();
        List<Integer> elves = parseInput(content);

        System.out.println("Part 1: " + part1(elves));
        System.out.println("Part 2: " + part2(elves));
    }

    private static List<Integer> parseInput(String content) {
        List<Integer> elves = new ArrayList<>();

        for (String group : content.split("\n\n")) {
            int total = 0;
            for (String line : group.split("\n")) {
                if (!line.isEmpty()) {
                    total += Integer.parseInt(line);
                }
            }
            elves.add(total);
        }

        return elves;
    }

    private static int part1(List<Integer> elves) {
        return Collections.max(elves);
    }

    private static int part2(List<Integer> elves) {
        List<Integer> sorted = new ArrayList<>(elves);
        sorted.sort(Collections.reverseOrder());
        return sorted.get(0) + sorted.get(1) + sorted.get(2);
    }
}
