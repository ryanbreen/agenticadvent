import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Solution {
    private final String instructions;
    private final Map<String, String[]> network;

    public Solution(String input) {
        String[] parts = input.split("\n\n");
        this.instructions = parts[0].trim();
        this.network = new HashMap<>();

        for (String line : parts[1].split("\n")) {
            if (line.isBlank()) continue;
            // Parse: AAA = (BBB, CCC)
            String[] nodeParts = line.split(" = ");
            String node = nodeParts[0];
            String connections = nodeParts[1].substring(1, nodeParts[1].length() - 1);
            String[] leftRight = connections.split(", ");
            network.put(node, leftRight);
        }
    }

    public long part1() {
        String current = "AAA";
        long steps = 0;
        int instructionLen = instructions.length();

        while (!current.equals("ZZZ")) {
            char instruction = instructions.charAt((int) (steps % instructionLen));
            current = (instruction == 'L') ? network.get(current)[0] : network.get(current)[1];
            steps++;
        }

        return steps;
    }

    public long part2() {
        // Find all starting nodes (ending in A)
        List<String> startNodes = network.keySet().stream()
                .filter(node -> node.endsWith("A"))
                .collect(Collectors.toList());

        int instructionLen = instructions.length();

        // For each starting node, find steps to reach a Z node
        long[] cycleLengths = new long[startNodes.size()];
        for (int i = 0; i < startNodes.size(); i++) {
            String current = startNodes.get(i);
            long steps = 0;
            while (!current.endsWith("Z")) {
                char instruction = instructions.charAt((int) (steps % instructionLen));
                current = (instruction == 'L') ? network.get(current)[0] : network.get(current)[1];
                steps++;
            }
            cycleLengths[i] = steps;
        }

        // Find LCM of all cycle lengths
        long result = cycleLengths[0];
        for (int i = 1; i < cycleLengths.length; i++) {
            result = lcm(result, cycleLengths[i]);
        }

        return result;
    }

    private static long gcd(long a, long b) {
        while (b != 0) {
            long temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

    private static long lcm(long a, long b) {
        return a * b / gcd(a, b);
    }

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt"));
        Solution solution = new Solution(input);

        System.out.println("Part 1: " + solution.part1());
        System.out.println("Part 2: " + solution.part2());
    }
}
