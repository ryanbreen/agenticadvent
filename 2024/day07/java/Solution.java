import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

class Equation {
    long target;
    List<Long> numbers;

    Equation(long target, List<Long> numbers) {
        this.target = target;
        this.numbers = numbers;
    }
}

public class Solution {

    static List<Equation> parseInput(String text) {
        List<Equation> equations = new ArrayList<>();
        for (String line : text.trim().split("\n")) {
            String[] parts = line.split(": ");
            long target = Long.parseLong(parts[0]);
            String[] numStrs = parts[1].split(" ");
            List<Long> numbers = new ArrayList<>();
            for (String numStr : numStrs) {
                numbers.add(Long.parseLong(numStr));
            }
            equations.add(new Equation(target, numbers));
        }
        return equations;
    }

    static long evaluate(List<Long> nums, int[] ops) {
        long result = nums.get(0);
        for (int i = 0; i < ops.length; i++) {
            if (ops[i] == 0) {  // Add
                result += nums.get(i + 1);
            } else if (ops[i] == 1) {  // Multiply
                result *= nums.get(i + 1);
            } else if (ops[i] == 2) {  // Concatenate
                result = Long.parseLong(String.valueOf(result) + String.valueOf(nums.get(i + 1)));
            }
        }
        return result;
    }

    static boolean canMakeTarget(long target, List<Long> nums, int numOperators) {
        int nOps = nums.size() - 1;
        int totalCombinations = (int) Math.pow(numOperators, nOps);

        for (int i = 0; i < totalCombinations; i++) {
            int[] ops = new int[nOps];
            int val = i;
            for (int j = 0; j < nOps; j++) {
                ops[j] = val % numOperators;
                val /= numOperators;
            }

            if (evaluate(nums, ops) == target) {
                return true;
            }
        }
        return false;
    }

    static long part1(List<Equation> equations) {
        long total = 0;
        for (Equation eq : equations) {
            if (canMakeTarget(eq.target, eq.numbers, 2)) {  // 2 operators: + and *
                total += eq.target;
            }
        }
        return total;
    }

    static long part2(List<Equation> equations) {
        long total = 0;
        for (Equation eq : equations) {
            if (canMakeTarget(eq.target, eq.numbers, 3)) {  // 3 operators: +, *, ||
                total += eq.target;
            }
        }
        return total;
    }

    public static void main(String[] args) throws IOException {
        String text = new String(Files.readAllBytes(Paths.get("../input.txt")));
        List<Equation> equations = parseInput(text);

        System.out.println("Part 1: " + part1(equations));
        System.out.println("Part 2: " + part2(equations));
    }
}
