import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Solution {

    static class Event implements Comparable<Event> {
        int position;
        String type;
        int x;
        int y;

        Event(int position, String type, int x, int y) {
            this.position = position;
            this.type = type;
            this.x = x;
            this.y = y;
        }

        @Override
        public int compareTo(Event other) {
            return Integer.compare(this.position, other.position);
        }
    }

    static int part1(String data) {
        // Find all valid mul(X,Y) instructions and sum their products
        Pattern pattern = Pattern.compile("mul\\((\\d{1,3}),(\\d{1,3})\\)");
        Matcher matcher = pattern.matcher(data);

        int total = 0;
        while (matcher.find()) {
            int x = Integer.parseInt(matcher.group(1));
            int y = Integer.parseInt(matcher.group(2));
            total += x * y;
        }

        return total;
    }

    static int part2(String data) {
        // Like part1, but do() enables and don't() disables mul instructions
        Pattern mulPattern = Pattern.compile("mul\\((\\d{1,3}),(\\d{1,3})\\)");
        Pattern doPattern = Pattern.compile("do\\(\\)");
        Pattern dontPattern = Pattern.compile("don't\\(\\)");

        List<Event> events = new ArrayList<>();

        // Find all mul instructions
        Matcher mulMatcher = mulPattern.matcher(data);
        while (mulMatcher.find()) {
            int x = Integer.parseInt(mulMatcher.group(1));
            int y = Integer.parseInt(mulMatcher.group(2));
            events.add(new Event(mulMatcher.start(), "mul", x, y));
        }

        // Find all do() instructions
        Matcher doMatcher = doPattern.matcher(data);
        while (doMatcher.find()) {
            events.add(new Event(doMatcher.start(), "do", 0, 0));
        }

        // Find all don't() instructions
        Matcher dontMatcher = dontPattern.matcher(data);
        while (dontMatcher.find()) {
            events.add(new Event(dontMatcher.start(), "dont", 0, 0));
        }

        // Sort by position
        events.sort(null);

        // Process events in order
        int total = 0;
        boolean enabled = true;

        for (Event event : events) {
            switch (event.type) {
                case "do":
                    enabled = true;
                    break;
                case "dont":
                    enabled = false;
                    break;
                case "mul":
                    if (enabled) {
                        total += event.x * event.y;
                    }
                    break;
            }
        }

        return total;
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of("../input.txt");
        String data = Files.readString(inputPath);

        System.out.println("Part 1: " + part1(data));
        System.out.println("Part 2: " + part2(data));
    }
}
