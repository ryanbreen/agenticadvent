import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;

/**
 * Day 19: Aplenty - Workflow processing and range analysis.
 */
public class solution {

    // Represents a rule in a workflow
    record Rule(Character attr, Character op, Integer value, String destination) {
        boolean isDefault() {
            return attr == null;
        }

        boolean matches(Map<Character, Integer> part) {
            if (isDefault()) return true;
            int partValue = part.get(attr);
            return op == '<' ? partValue < value : partValue > value;
        }
    }

    // Represents a range [lo, hi] inclusive
    record Range(long lo, long hi) {
        long size() {
            return Math.max(0, hi - lo + 1);
        }
    }

    private final Map<String, List<Rule>> workflows = new HashMap<>();
    private final List<Map<Character, Integer>> parts = new ArrayList<>();

    public static void main(String[] args) throws IOException {
        solution sol = new solution();
        sol.parseInput(Paths.get(args.length > 0 ? args[0] : "../input.txt"));

        System.out.println("Part 1: " + sol.part1());
        System.out.println("Part 2: " + sol.part2());
    }

    void parseInput(Path path) throws IOException {
        String content = Files.readString(path);
        String[] sections = content.strip().split("\n\n");

        // Parse workflows
        Pattern rulePattern = Pattern.compile("([xmas])([<>])(\\d+)");
        for (String line : sections[0].split("\n")) {
            int braceIdx = line.indexOf('{');
            String name = line.substring(0, braceIdx);
            String rulesStr = line.substring(braceIdx + 1, line.length() - 1);

            List<Rule> rules = new ArrayList<>();
            for (String ruleStr : rulesStr.split(",")) {
                if (ruleStr.contains(":")) {
                    String[] parts = ruleStr.split(":");
                    Matcher m = rulePattern.matcher(parts[0]);
                    m.find();
                    rules.add(new Rule(
                        m.group(1).charAt(0),
                        m.group(2).charAt(0),
                        Integer.parseInt(m.group(3)),
                        parts[1]
                    ));
                } else {
                    rules.add(new Rule(null, null, null, ruleStr));
                }
            }
            workflows.put(name, rules);
        }

        // Parse parts
        Pattern partPattern = Pattern.compile("([xmas])=(\\d+)");
        for (String line : sections[1].split("\n")) {
            Map<Character, Integer> part = new HashMap<>();
            Matcher m = partPattern.matcher(line);
            while (m.find()) {
                part.put(m.group(1).charAt(0), Integer.parseInt(m.group(2)));
            }
            parts.add(part);
        }
    }

    boolean processPart(Map<Character, Integer> part) {
        String current = "in";

        while (!current.equals("A") && !current.equals("R")) {
            for (Rule rule : workflows.get(current)) {
                if (rule.matches(part)) {
                    current = rule.destination();
                    break;
                }
            }
        }

        return current.equals("A");
    }

    long part1() {
        long total = 0;
        for (var part : parts) {
            if (processPart(part)) {
                total += part.get('x') + part.get('m') + part.get('a') + part.get('s');
            }
        }
        return total;
    }

    long countAccepted(String workflow, Map<Character, Range> ranges) {
        if (workflow.equals("R")) {
            return 0;
        }
        if (workflow.equals("A")) {
            long result = 1;
            for (Range r : ranges.values()) {
                result *= r.size();
            }
            return result;
        }

        long total = 0;
        Map<Character, Range> currentRanges = new HashMap<>(ranges);

        for (Rule rule : workflows.get(workflow)) {
            if (rule.isDefault()) {
                total += countAccepted(rule.destination(), currentRanges);
            } else {
                char attr = rule.attr();
                char op = rule.op();
                int value = rule.value();
                Range r = currentRanges.get(attr);
                long lo = r.lo();
                long hi = r.hi();

                if (op == '<') {
                    // Split: [lo, value-1] goes to destination, [value, hi] continues
                    if (lo < value) {
                        Map<Character, Range> newRanges = new HashMap<>(currentRanges);
                        newRanges.put(attr, new Range(lo, Math.min(hi, value - 1)));
                        total += countAccepted(rule.destination(), newRanges);
                    }
                    if (hi >= value) {
                        currentRanges.put(attr, new Range(Math.max(lo, value), hi));
                    } else {
                        break;
                    }
                } else { // op == '>'
                    // Split: [value+1, hi] goes to destination, [lo, value] continues
                    if (hi > value) {
                        Map<Character, Range> newRanges = new HashMap<>(currentRanges);
                        newRanges.put(attr, new Range(Math.max(lo, value + 1), hi));
                        total += countAccepted(rule.destination(), newRanges);
                    }
                    if (lo <= value) {
                        currentRanges.put(attr, new Range(lo, Math.min(hi, value)));
                    } else {
                        break;
                    }
                }
            }
        }

        return total;
    }

    long part2() {
        Map<Character, Range> initialRanges = new HashMap<>();
        initialRanges.put('x', new Range(1, 4000));
        initialRanges.put('m', new Range(1, 4000));
        initialRanges.put('a', new Range(1, 4000));
        initialRanges.put('s', new Range(1, 4000));
        return countAccepted("in", initialRanges);
    }
}
