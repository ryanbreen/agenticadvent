import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Solution {

    record Command(String direction, int value) {}

    public static void main(String[] args) throws IOException {
        List<Command> commands = parseInput();
        System.out.println("Part 1: " + part1(commands));
        System.out.println("Part 2: " + part2(commands));
    }

    private static List<Command> parseInput() throws IOException {
        List<Command> commands = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader("../input.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty()) {
                    String[] parts = line.split(" ");
                    commands.add(new Command(parts[0], Integer.parseInt(parts[1])));
                }
            }
        }
        return commands;
    }

    private static long part1(List<Command> commands) {
        long horizontal = 0;
        long depth = 0;

        for (Command cmd : commands) {
            switch (cmd.direction()) {
                case "forward" -> horizontal += cmd.value();
                case "down" -> depth += cmd.value();
                case "up" -> depth -= cmd.value();
            }
        }

        return horizontal * depth;
    }

    private static long part2(List<Command> commands) {
        long horizontal = 0;
        long depth = 0;
        long aim = 0;

        for (Command cmd : commands) {
            switch (cmd.direction()) {
                case "forward" -> {
                    horizontal += cmd.value();
                    depth += aim * cmd.value();
                }
                case "down" -> aim += cmd.value();
                case "up" -> aim -= cmd.value();
            }
        }

        return horizontal * depth;
    }
}
