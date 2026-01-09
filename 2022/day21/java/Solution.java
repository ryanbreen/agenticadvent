import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    private Map<String, Object> monkeys = new HashMap<>();
    private Map<String, Long> evalMemo = new HashMap<>();
    private Map<String, Boolean> humnMemo = new HashMap<>();

    public static void main(String[] args) throws IOException {
        Solution solution = new Solution();
        String inputPath = Paths.get(System.getProperty("user.dir"), "..", "input.txt")
                                .normalize().toString();
        String text = Files.readString(Path.of(inputPath));

        System.out.println("Part 1: " + solution.part1(text));
        System.out.println("Part 2: " + solution.part2(text));
    }

    private void parseInput(String text) {
        monkeys.clear();
        evalMemo.clear();
        humnMemo.clear();

        for (String line : text.trim().split("\n")) {
            String[] parts = line.split(": ");
            String name = parts[0];
            String job = parts[1];
            String[] jobParts = job.split(" ");

            if (jobParts.length == 1) {
                monkeys.put(name, Long.parseLong(jobParts[0]));
            } else {
                monkeys.put(name, new String[]{jobParts[0], jobParts[1], jobParts[2]});
            }
        }
    }

    private long evaluate(String name) {
        if (evalMemo.containsKey(name)) {
            return evalMemo.get(name);
        }

        Object job = monkeys.get(name);
        if (job instanceof Long) {
            return (Long) job;
        }

        String[] parts = (String[]) job;
        String left = parts[0];
        String op = parts[1];
        String right = parts[2];

        long leftVal = evaluate(left);
        long rightVal = evaluate(right);

        long result;
        switch (op) {
            case "+": result = leftVal + rightVal; break;
            case "-": result = leftVal - rightVal; break;
            case "*": result = leftVal * rightVal; break;
            case "/": result = leftVal / rightVal; break;
            default: throw new IllegalArgumentException("Unknown operator: " + op);
        }

        evalMemo.put(name, result);
        return result;
    }

    private boolean containsHumn(String name) {
        if (humnMemo.containsKey(name)) {
            return humnMemo.get(name);
        }

        if (name.equals("humn")) {
            return true;
        }

        Object job = monkeys.get(name);
        if (job instanceof Long) {
            humnMemo.put(name, false);
            return false;
        }

        String[] parts = (String[]) job;
        boolean result = containsHumn(parts[0]) || containsHumn(parts[2]);
        humnMemo.put(name, result);
        return result;
    }

    private long solveForHumn(String name, long target) {
        if (name.equals("humn")) {
            return target;
        }

        Object job = monkeys.get(name);
        if (job instanceof Long) {
            throw new IllegalStateException("Cannot solve for humn through a literal number");
        }

        String[] parts = (String[]) job;
        String left = parts[0];
        String op = parts[1];
        String right = parts[2];

        boolean leftHasHumn = containsHumn(left);

        if (leftHasHumn) {
            long rightVal = evaluate(right);
            long newTarget;
            switch (op) {
                case "+": newTarget = target - rightVal; break;
                case "-": newTarget = target + rightVal; break;
                case "*": newTarget = target / rightVal; break;
                case "/": newTarget = target * rightVal; break;
                default: throw new IllegalArgumentException("Unknown operator: " + op);
            }
            return solveForHumn(left, newTarget);
        } else {
            long leftVal = evaluate(left);
            long newTarget;
            switch (op) {
                case "+": newTarget = target - leftVal; break;
                case "-": newTarget = leftVal - target; break;
                case "*": newTarget = target / leftVal; break;
                case "/": newTarget = leftVal / target; break;
                default: throw new IllegalArgumentException("Unknown operator: " + op);
            }
            return solveForHumn(right, newTarget);
        }
    }

    public long part1(String text) {
        parseInput(text);
        return evaluate("root");
    }

    public long part2(String text) {
        parseInput(text);

        String[] rootJob = (String[]) monkeys.get("root");
        String left = rootJob[0];
        String right = rootJob[2];

        boolean leftHasHumn = containsHumn(left);

        if (leftHasHumn) {
            long target = evaluate(right);
            return solveForHumn(left, target);
        } else {
            long target = evaluate(left);
            return solveForHumn(right, target);
        }
    }
}
