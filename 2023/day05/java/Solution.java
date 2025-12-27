import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Solution {

    record MapRange(long dstStart, long srcStart, long length) {
        long srcEnd() {
            return srcStart + length;
        }
    }

    record Range(long start, long end) {}

    public static void main(String[] args) throws IOException {
        String text = Files.readString(Path.of("../input.txt"));
        String[] sections = text.strip().split("\n\n");

        // Parse seeds
        long[] seeds = Arrays.stream(sections[0].split(": ")[1].split("\\s+"))
                .mapToLong(Long::parseLong)
                .toArray();

        // Parse maps
        List<List<MapRange>> maps = new ArrayList<>();
        for (int i = 1; i < sections.length; i++) {
            String[] lines = sections[i].strip().split("\n");
            List<MapRange> ranges = new ArrayList<>();
            for (int j = 1; j < lines.length; j++) {
                String[] parts = lines[j].split("\\s+");
                long dst = Long.parseLong(parts[0]);
                long src = Long.parseLong(parts[1]);
                long len = Long.parseLong(parts[2]);
                ranges.add(new MapRange(dst, src, len));
            }
            maps.add(ranges);
        }

        System.out.println("Part 1: " + part1(seeds, maps));
        System.out.println("Part 2: " + part2(seeds, maps));
    }

    static long applyMap(long value, List<MapRange> ranges) {
        for (MapRange range : ranges) {
            if (value >= range.srcStart && value < range.srcEnd()) {
                return range.dstStart + (value - range.srcStart);
            }
        }
        return value;
    }

    static long seedToLocation(long seed, List<List<MapRange>> maps) {
        long value = seed;
        for (List<MapRange> mapRanges : maps) {
            value = applyMap(value, mapRanges);
        }
        return value;
    }

    static long part1(long[] seeds, List<List<MapRange>> maps) {
        long min = Long.MAX_VALUE;
        for (long seed : seeds) {
            min = Math.min(min, seedToLocation(seed, maps));
        }
        return min;
    }

    static List<Range> applyMapToRanges(List<Range> inputRanges, List<MapRange> mapRanges) {
        List<Range> result = new ArrayList<>();

        for (Range input : inputRanges) {
            List<Range> remaining = new ArrayList<>();
            remaining.add(input);

            for (MapRange mr : mapRanges) {
                List<Range> newRemaining = new ArrayList<>();

                for (Range r : remaining) {
                    // Part before the map range (unmapped)
                    if (r.start < mr.srcStart) {
                        newRemaining.add(new Range(r.start, Math.min(r.end, mr.srcStart)));
                    }

                    // Part within the map range (mapped)
                    long overlapStart = Math.max(r.start, mr.srcStart);
                    long overlapEnd = Math.min(r.end, mr.srcEnd());
                    if (overlapStart < overlapEnd) {
                        long offset = mr.dstStart - mr.srcStart;
                        result.add(new Range(overlapStart + offset, overlapEnd + offset));
                    }

                    // Part after the map range (unmapped)
                    if (r.end > mr.srcEnd()) {
                        newRemaining.add(new Range(Math.max(r.start, mr.srcEnd()), r.end));
                    }
                }

                remaining = newRemaining;
            }

            // Any remaining parts are unmapped (identity)
            result.addAll(remaining);
        }

        return result;
    }

    static long part2(long[] seeds, List<List<MapRange>> maps) {
        // Convert seeds to ranges
        List<Range> ranges = new ArrayList<>();
        for (int i = 0; i < seeds.length; i += 2) {
            long start = seeds[i];
            long length = seeds[i + 1];
            ranges.add(new Range(start, start + length));
        }

        // Apply each map to the ranges
        for (List<MapRange> mapRanges : maps) {
            ranges = applyMapToRanges(ranges, mapRanges);
        }

        // Find minimum start of any range
        long min = Long.MAX_VALUE;
        for (Range r : ranges) {
            min = Math.min(min, r.start);
        }
        return min;
    }
}
