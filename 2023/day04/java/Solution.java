import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

public class Solution {

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt")).strip();
        Card[] cards = parseCards(input);

        System.out.println("Part 1: " + part1(cards));
        System.out.println("Part 2: " + part2(cards));
    }

    private static Card[] parseCards(String input) {
        return Arrays.stream(input.split("\n"))
            .map(line -> {
                String[] numberParts = line.split(":")[1].split("\\|");
                Set<Integer> winning = parseNumbers(numberParts[0]);
                Set<Integer> have = parseNumbers(numberParts[1]);
                return new Card(winning, have);
            })
            .toArray(Card[]::new);
    }

    private static Set<Integer> parseNumbers(String str) {
        return Arrays.stream(str.trim().split("\\s+"))
            .map(Integer::parseInt)
            .collect(Collectors.toSet());
    }

    private static int part1(Card[] cards) {
        return Arrays.stream(cards)
            .mapToInt(card -> {
                int matches = card.countMatches();
                return matches > 0 ? 1 << (matches - 1) : 0;
            })
            .sum();
    }

    private static int part2(Card[] cards) {
        int n = cards.length;
        int[] copies = new int[n];
        Arrays.fill(copies, 1);

        for (int i = 0; i < n; i++) {
            int matches = cards[i].countMatches();
            for (int j = i + 1; j < Math.min(i + 1 + matches, n); j++) {
                copies[j] += copies[i];
            }
        }

        return Arrays.stream(copies).sum();
    }

    private record Card(Set<Integer> winning, Set<Integer> have) {
        int countMatches() {
            return (int) have.stream()
                .filter(winning::contains)
                .count();
        }
    }
}
