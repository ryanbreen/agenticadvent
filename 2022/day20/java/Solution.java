import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Solution {

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(System.getProperty("user.dir")).resolve("../input.txt");
        String content = Files.readString(inputPath);

        long[] numbers = parseInput(content);

        System.out.println("Part 1: " + part1(numbers));
        System.out.println("Part 2: " + part2(numbers));
    }

    static long[] parseInput(String text) {
        return text.strip().lines()
                   .mapToLong(Long::parseLong)
                   .toArray();
    }

    static long part1(long[] numbers) {
        long[] mixed = mix(numbers.clone(), 1);
        return groveCoordinates(mixed);
    }

    static long part2(long[] numbers) {
        long decryptionKey = 811589153L;
        long[] scaled = new long[numbers.length];
        for (int i = 0; i < numbers.length; i++) {
            scaled[i] = numbers[i] * decryptionKey;
        }
        long[] mixed = mix(scaled, 10);
        return groveCoordinates(mixed);
    }

    static long[] mix(long[] numbers, int times) {
        int n = numbers.length;

        // Store (original_index, value) pairs as a list for efficient manipulation
        List<long[]> indexed = new ArrayList<>(n);
        for (int i = 0; i < n; i++) {
            indexed.add(new long[]{i, numbers[i]});
        }

        for (int t = 0; t < times; t++) {
            for (int origIdx = 0; origIdx < n; origIdx++) {
                // Find current position of this element
                int currPos = -1;
                for (int i = 0; i < n; i++) {
                    if (indexed.get(i)[0] == origIdx) {
                        currPos = i;
                        break;
                    }
                }

                long[] element = indexed.get(currPos);
                long val = element[1];

                // Remove from current position
                indexed.remove(currPos);

                // Calculate new position (modulo n-1 because we removed the element)
                long newPos = ((currPos + val) % (n - 1) + (n - 1)) % (n - 1);

                // Insert at new position
                indexed.add((int) newPos, element);
            }
        }

        // Extract values
        long[] result = new long[n];
        for (int i = 0; i < n; i++) {
            result[i] = indexed.get(i)[1];
        }
        return result;
    }

    static long groveCoordinates(long[] mixed) {
        int n = mixed.length;
        int zeroIdx = -1;
        for (int i = 0; i < n; i++) {
            if (mixed[i] == 0) {
                zeroIdx = i;
                break;
            }
        }

        long sum = 0;
        for (int offset : new int[]{1000, 2000, 3000}) {
            sum += mixed[(zeroIdx + offset) % n];
        }
        return sum;
    }
}
