import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

public class Solution {
    private record Node(String left, String right) {}

    private final String instructions;
    private final Map<String, Node> network;

    public Solution(String input) {
        String[] parts = input.split("\n\n");
        this.instructions = parts[0].trim();
        this.network = new HashMap<>();

        for (String line : parts[1].split("\n")) {
            if (line.isBlank()) continue;
            // Parse: AAA = (BBB, CCC)
            String[] nodeParts = line.split(" = ");
            String name = nodeParts[0];
            String connections = nodeParts[1].substring(1, nodeParts[1].length() - 1);
            String[] leftRight = connections.split(", ");
            network.put(name, new Node(leftRight[0], leftRight[1]));
        }
    }

    private long navigate(String start, Predicate<String> isEnd) {
        String current = start;
        long steps = 0;
        int instructionLen = instructions.length();

        while (!isEnd.test(current)) {
            char instruction = instructions.charAt((int) (steps % instructionLen));
            Node node = network.get(current);
            current = (instruction == 'L') ? node.left() : node.right();
            steps++;
        }

        return steps;
    }

    public long part1() {
        return navigate("AAA", node -> node.equals("ZZZ"));
    }

    public long part2() {
        return network.keySet().stream()
                .filter(node -> node.endsWith("A"))
                .mapToLong(start -> navigate(start, node -> node.endsWith("Z")))
                .reduce(1L, Solution::lcm);
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
        return a / gcd(a, b) * b;
    }

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt"));
        Solution solution = new Solution(input);

        System.out.println("Part 1: " + solution.part1());
        System.out.println("Part 2: " + solution.part2());
    }
}
