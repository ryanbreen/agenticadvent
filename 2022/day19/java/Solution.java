import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;

public class Solution {

    private static int[] parseBlueprint(String line) {
        Pattern pattern = Pattern.compile(
            "Blueprint (\\d+): Each ore robot costs (\\d+) ore\\. " +
            "Each clay robot costs (\\d+) ore\\. " +
            "Each obsidian robot costs (\\d+) ore and (\\d+) clay\\. " +
            "Each geode robot costs (\\d+) ore and (\\d+) obsidian\\."
        );
        Matcher m = pattern.matcher(line);
        if (m.find()) {
            int[] result = new int[7];
            for (int i = 0; i < 7; i++) {
                result[i] = Integer.parseInt(m.group(i + 1));
            }
            return result;
        }
        return null;
    }

    private static List<int[]> parseInput(String text) {
        List<int[]> blueprints = new ArrayList<>();
        for (String line : text.split("\n")) {
            int[] bp = parseBlueprint(line);
            if (bp != null) {
                blueprints.add(bp);
            }
        }
        return blueprints;
    }

    private static int maxGeodes(int[] bp, int timeLimit) {
        int bpId = bp[0];
        int oreOre = bp[1];
        int clayOre = bp[2];
        int obsOre = bp[3];
        int obsClay = bp[4];
        int geoOre = bp[5];
        int geoObs = bp[6];

        // Max robots needed per type
        int maxOre = Math.max(Math.max(oreOre, clayOre), Math.max(obsOre, geoOre));
        int maxClay = obsClay;
        int maxObs = geoObs;

        int[] best = {0};
        Map<String, Integer> seen = new HashMap<>();

        dfs(0, 0, 0, 0, 0, 1, 0, 0, 0, timeLimit,
            oreOre, clayOre, obsOre, obsClay, geoOre, geoObs,
            maxOre, maxClay, maxObs, best, seen);

        return best[0];
    }

    private static void dfs(int time, int ore, int clay, int obs, int geodes,
                            int oreR, int clayR, int obsR, int geoR,
                            int timeLimit, int oreOre, int clayOre,
                            int obsOre, int obsClay, int geoOre, int geoObs,
                            int maxOre, int maxClay, int maxObs,
                            int[] best, Map<String, Integer> seen) {

        int remaining = timeLimit - time;

        // Pruning: upper bound on possible geodes
        int upperBound = geodes + geoR * remaining + (remaining * (remaining - 1)) / 2;
        if (upperBound <= best[0]) return;

        if (time == timeLimit) {
            best[0] = Math.max(best[0], geodes);
            return;
        }

        // Cap resources
        int cappedOre = Math.min(ore, remaining * maxOre);
        int cappedClay = Math.min(clay, remaining * maxClay);
        int cappedObs = Math.min(obs, remaining * maxObs);

        // State deduplication
        String key = time + "," + cappedOre + "," + cappedClay + "," + cappedObs + "," +
                     oreR + "," + clayR + "," + obsR + "," + geoR;
        Integer prevGeodes = seen.get(key);
        if (prevGeodes != null && prevGeodes >= geodes) return;
        seen.put(key, geodes);

        // Collect resources
        int newOre = cappedOre + oreR;
        int newClay = cappedClay + clayR;
        int newObs = cappedObs + obsR;
        int newGeodes = geodes + geoR;

        // Try building geode robot (always do if possible)
        if (cappedOre >= geoOre && cappedObs >= geoObs) {
            dfs(time + 1, newOre - geoOre, newClay, newObs - geoObs, newGeodes,
                oreR, clayR, obsR, geoR + 1, timeLimit,
                oreOre, clayOre, obsOre, obsClay, geoOre, geoObs,
                maxOre, maxClay, maxObs, best, seen);
            return; // If we can build geode, always do
        }

        // Try building obsidian robot
        if (cappedOre >= obsOre && cappedClay >= obsClay && obsR < maxObs) {
            dfs(time + 1, newOre - obsOre, newClay - obsClay, newObs, newGeodes,
                oreR, clayR, obsR + 1, geoR, timeLimit,
                oreOre, clayOre, obsOre, obsClay, geoOre, geoObs,
                maxOre, maxClay, maxObs, best, seen);
        }

        // Try building clay robot
        if (cappedOre >= clayOre && clayR < maxClay) {
            dfs(time + 1, newOre - clayOre, newClay, newObs, newGeodes,
                oreR, clayR + 1, obsR, geoR, timeLimit,
                oreOre, clayOre, obsOre, obsClay, geoOre, geoObs,
                maxOre, maxClay, maxObs, best, seen);
        }

        // Try building ore robot
        if (cappedOre >= oreOre && oreR < maxOre) {
            dfs(time + 1, newOre - oreOre, newClay, newObs, newGeodes,
                oreR + 1, clayR, obsR, geoR, timeLimit,
                oreOre, clayOre, obsOre, obsClay, geoOre, geoObs,
                maxOre, maxClay, maxObs, best, seen);
        }

        // Do nothing (wait)
        dfs(time + 1, newOre, newClay, newObs, newGeodes,
            oreR, clayR, obsR, geoR, timeLimit,
            oreOre, clayOre, obsOre, obsClay, geoOre, geoObs,
            maxOre, maxClay, maxObs, best, seen);
    }

    private static int part1(List<int[]> blueprints) {
        int total = 0;
        for (int[] bp : blueprints) {
            int geodes = maxGeodes(bp, 24);
            total += bp[0] * geodes;
        }
        return total;
    }

    private static long part2(List<int[]> blueprints) {
        long result = 1;
        int count = Math.min(3, blueprints.size());
        for (int i = 0; i < count; i++) {
            int geodes = maxGeodes(blueprints.get(i), 32);
            result *= geodes;
        }
        return result;
    }

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt")).trim();
        List<int[]> blueprints = parseInput(input);

        System.out.println("Part 1: " + part1(blueprints));
        System.out.println("Part 2: " + part2(blueprints));
    }
}
