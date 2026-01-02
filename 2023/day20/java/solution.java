import java.io.*;
import java.nio.file.*;
import java.util.*;

public class solution {

    enum ModuleType { BROADCASTER, FLIP_FLOP, CONJUNCTION }

    static class Module {
        ModuleType type;
        List<String> destinations;
        boolean state; // For flip-flops
        Map<String, Boolean> memory; // For conjunctions

        Module(ModuleType type, List<String> destinations) {
            this.type = type;
            this.destinations = destinations;
            this.state = false;
            this.memory = new HashMap<>();
        }
    }

    static class Pulse {
        String source;
        String destination;
        boolean high; // true = high pulse, false = low pulse

        Pulse(String source, String destination, boolean high) {
            this.source = source;
            this.destination = destination;
            this.high = high;
        }
    }

    public static Map<String, Module> parseInput(String filename) throws IOException {
        Map<String, Module> modules = new HashMap<>();
        List<String> lines = Files.readAllLines(Paths.get(filename));

        for (String line : lines) {
            String[] parts = line.split(" -> ");
            String namePart = parts[0];
            String[] destParts = parts[1].split(", ");
            List<String> destinations = Arrays.asList(destParts);

            if (namePart.equals("broadcaster")) {
                modules.put("broadcaster", new Module(ModuleType.BROADCASTER, destinations));
            } else if (namePart.startsWith("%")) {
                String name = namePart.substring(1);
                modules.put(name, new Module(ModuleType.FLIP_FLOP, destinations));
            } else if (namePart.startsWith("&")) {
                String name = namePart.substring(1);
                modules.put(name, new Module(ModuleType.CONJUNCTION, destinations));
            }
        }

        // Initialize conjunction memory for all inputs
        for (Map.Entry<String, Module> entry : modules.entrySet()) {
            String name = entry.getKey();
            Module module = entry.getValue();
            for (String dest : module.destinations) {
                if (modules.containsKey(dest) && modules.get(dest).type == ModuleType.CONJUNCTION) {
                    modules.get(dest).memory.put(name, false);
                }
            }
        }

        return modules;
    }

    static void resetModules(Map<String, Module> modules) {
        for (Module module : modules.values()) {
            if (module.type == ModuleType.FLIP_FLOP) {
                module.state = false;
            } else if (module.type == ModuleType.CONJUNCTION) {
                for (String key : module.memory.keySet()) {
                    module.memory.put(key, false);
                }
            }
        }
    }

    static long[] simulateButtonPress(Map<String, Module> modules, Set<String> watchNodes) {
        long lowCount = 0;
        long highCount = 0;
        Set<String> highSenders = new HashSet<>();

        Queue<Pulse> queue = new LinkedList<>();
        queue.add(new Pulse("button", "broadcaster", false));

        while (!queue.isEmpty()) {
            Pulse pulse = queue.poll();

            if (pulse.high) {
                highCount++;
            } else {
                lowCount++;
            }

            // Track if watched nodes send high pulses
            if (watchNodes != null && watchNodes.contains(pulse.source) && pulse.high) {
                highSenders.add(pulse.source);
            }

            if (!modules.containsKey(pulse.destination)) {
                continue;
            }

            Module module = modules.get(pulse.destination);

            switch (module.type) {
                case BROADCASTER:
                    for (String nextDest : module.destinations) {
                        queue.add(new Pulse(pulse.destination, nextDest, pulse.high));
                    }
                    break;

                case FLIP_FLOP:
                    if (!pulse.high) { // Only react to low pulses
                        module.state = !module.state;
                        for (String nextDest : module.destinations) {
                            queue.add(new Pulse(pulse.destination, nextDest, module.state));
                        }
                    }
                    break;

                case CONJUNCTION:
                    module.memory.put(pulse.source, pulse.high);
                    // Send low if all inputs are high, otherwise send high
                    boolean allHigh = module.memory.values().stream().allMatch(v -> v);
                    boolean output = !allHigh;
                    for (String nextDest : module.destinations) {
                        queue.add(new Pulse(pulse.destination, nextDest, output));
                    }
                    break;
            }
        }

        // Encode high senders count in a separate way - we'll use a different approach for part 2
        return new long[] { lowCount, highCount };
    }

    static Set<String> simulateAndGetHighSenders(Map<String, Module> modules, Set<String> watchNodes) {
        Set<String> highSenders = new HashSet<>();

        Queue<Pulse> queue = new LinkedList<>();
        queue.add(new Pulse("button", "broadcaster", false));

        while (!queue.isEmpty()) {
            Pulse pulse = queue.poll();

            // Track if watched nodes send high pulses
            if (watchNodes != null && watchNodes.contains(pulse.source) && pulse.high) {
                highSenders.add(pulse.source);
            }

            if (!modules.containsKey(pulse.destination)) {
                continue;
            }

            Module module = modules.get(pulse.destination);

            switch (module.type) {
                case BROADCASTER:
                    for (String nextDest : module.destinations) {
                        queue.add(new Pulse(pulse.destination, nextDest, pulse.high));
                    }
                    break;

                case FLIP_FLOP:
                    if (!pulse.high) {
                        module.state = !module.state;
                        for (String nextDest : module.destinations) {
                            queue.add(new Pulse(pulse.destination, nextDest, module.state));
                        }
                    }
                    break;

                case CONJUNCTION:
                    module.memory.put(pulse.source, pulse.high);
                    boolean allHigh = module.memory.values().stream().allMatch(v -> v);
                    boolean output = !allHigh;
                    for (String nextDest : module.destinations) {
                        queue.add(new Pulse(pulse.destination, nextDest, output));
                    }
                    break;
            }
        }

        return highSenders;
    }

    static long part1(Map<String, Module> modules) {
        resetModules(modules);

        long totalLow = 0;
        long totalHigh = 0;

        for (int i = 0; i < 1000; i++) {
            long[] counts = simulateButtonPress(modules, null);
            totalLow += counts[0];
            totalHigh += counts[1];
        }

        return totalLow * totalHigh;
    }

    static long gcd(long a, long b) {
        while (b != 0) {
            long t = b;
            b = a % b;
            a = t;
        }
        return a;
    }

    static long lcm(long a, long b) {
        return a / gcd(a, b) * b;
    }

    static long part2(Map<String, Module> modules) {
        resetModules(modules);

        // Find the module that feeds into rx
        String rxInput = null;
        for (Map.Entry<String, Module> entry : modules.entrySet()) {
            if (entry.getValue().destinations.contains("rx")) {
                rxInput = entry.getKey();
                break;
            }
        }

        if (rxInput == null) {
            return 0;
        }

        // Find all modules that feed into rxInput
        Set<String> watchNodes = new HashSet<>(modules.get(rxInput).memory.keySet());
        Map<String, Long> cycleLengths = new HashMap<>();

        long buttonPress = 0;
        while (cycleLengths.size() < watchNodes.size()) {
            buttonPress++;
            Set<String> highSenders = simulateAndGetHighSenders(modules, watchNodes);

            for (String node : highSenders) {
                if (!cycleLengths.containsKey(node)) {
                    cycleLengths.put(node, buttonPress);
                }
            }
        }

        // LCM of all cycle lengths
        long result = 1;
        for (long length : cycleLengths.values()) {
            result = lcm(result, length);
        }

        return result;
    }

    public static void main(String[] args) throws IOException {
        String inputPath = Paths.get(System.getProperty("user.dir"), "..", "input.txt").normalize().toString();

        Map<String, Module> modules = parseInput(inputPath);
        System.out.println("Part 1: " + part1(modules));

        // Re-parse for part 2 (fresh state)
        modules = parseInput(inputPath);
        System.out.println("Part 2: " + part2(modules));
    }
}
