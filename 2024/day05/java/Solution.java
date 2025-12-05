import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Solution {
    private static Map<Integer, Set<Integer>> rules;
    private static List<List<Integer>> updates;

    public static void main(String[] args) throws IOException {
        parseInput();
        System.out.println("Part 1: " + part1());
        System.out.println("Part 2: " + part2());
    }

    private static void parseInput() throws IOException {
        String input = Files.readString(Path.of("../input.txt")).strip();
        String[] sections = input.split("\n\n");
        String[] rulesSection = sections[0].split("\n");
        String[] updatesSection = sections[1].split("\n");

        // Parse rules: X|Y means X must come before Y
        // Store as: rules[X] = set of pages that must come AFTER X
        rules = new HashMap<>();
        for (String rule : rulesSection) {
            String[] parts = rule.split("\\|");
            int before = Integer.parseInt(parts[0]);
            int after = Integer.parseInt(parts[1]);
            rules.computeIfAbsent(before, k -> new HashSet<>()).add(after);
        }

        // Parse updates
        updates = new ArrayList<>();
        for (String line : updatesSection) {
            List<Integer> update = new ArrayList<>();
            for (String num : line.split(",")) {
                update.add(Integer.parseInt(num));
            }
            updates.add(update);
        }
    }

    private static boolean isValidOrder(List<Integer> update) {
        Map<Integer, Integer> pagePositions = new HashMap<>();
        for (int i = 0; i < update.size(); i++) {
            pagePositions.put(update.get(i), i);
        }

        for (int i = 0; i < update.size(); i++) {
            int page = update.get(i);
            // Check all pages that must come after this page
            Set<Integer> mustBeAfter = rules.getOrDefault(page, Collections.emptySet());
            for (int afterPage : mustBeAfter) {
                if (pagePositions.containsKey(afterPage)) {
                    if (pagePositions.get(afterPage) < i) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    private static int part1() {
        int total = 0;
        for (List<Integer> update : updates) {
            if (isValidOrder(update)) {
                int middleIdx = update.size() / 2;
                total += update.get(middleIdx);
            }
        }
        return total;
    }

    private static List<Integer> fixOrder(List<Integer> update) {
        List<Integer> result = new ArrayList<>(update);
        result.sort((a, b) -> {
            // If a must come before b, return -1
            if (rules.getOrDefault(a, Collections.emptySet()).contains(b)) {
                return -1;
            }
            // If b must come before a, return 1
            if (rules.getOrDefault(b, Collections.emptySet()).contains(a)) {
                return 1;
            }
            return 0;
        });
        return result;
    }

    private static int part2() {
        int total = 0;
        for (List<Integer> update : updates) {
            if (!isValidOrder(update)) {
                List<Integer> fixed = fixOrder(update);
                int middleIdx = fixed.size() / 2;
                total += fixed.get(middleIdx);
            }
        }
        return total;
    }
}
