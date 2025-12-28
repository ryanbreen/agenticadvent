import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.IntStream;

public class Solution {

    // Hand type constants (higher = stronger)
    private static final int HIGH_CARD = 0;
    private static final int ONE_PAIR = 1;
    private static final int TWO_PAIR = 2;
    private static final int THREE_OF_A_KIND = 3;
    private static final int FULL_HOUSE = 4;
    private static final int FOUR_OF_A_KIND = 5;
    private static final int FIVE_OF_A_KIND = 6;

    // Card strength order (higher index = stronger)
    private static final String CARD_STRENGTH = "23456789TJQKA";
    private static final String CARD_STRENGTH_JOKER = "J23456789TQKA";

    public static void main(String[] args) throws IOException {
        var input = Files.readString(Path.of("../input.txt")).trim();
        var lines = input.split("\n");

        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: " + part2(lines));
    }

    private static long part1(String[] lines) {
        var hands = Arrays.stream(lines)
                .map(line -> {
                    var parts = line.split("\\s+");
                    return new Hand(parts[0], Integer.parseInt(parts[1]));
                })
                .sorted(Comparator.comparingInt((Hand h) -> getHandType(h.cards))
                        .thenComparing(h -> getCardValues(h.cards, CARD_STRENGTH)))
                .toList();

        return IntStream.rangeClosed(1, hands.size())
                .mapToLong(rank -> (long) rank * hands.get(rank - 1).bid)
                .sum();
    }

    private static long part2(String[] lines) {
        var hands = Arrays.stream(lines)
                .map(line -> {
                    var parts = line.split("\\s+");
                    return new Hand(parts[0], Integer.parseInt(parts[1]));
                })
                .sorted(Comparator.comparingInt((Hand h) -> getHandTypeWithJokers(h.cards))
                        .thenComparing(h -> getCardValues(h.cards, CARD_STRENGTH_JOKER)))
                .toList();

        return IntStream.rangeClosed(1, hands.size())
                .mapToLong(rank -> (long) rank * hands.get(rank - 1).bid)
                .sum();
    }

    private static int getHandType(String hand) {
        var counts = getCardCounts(hand);
        Arrays.sort(counts);
        return classifyHand(counts);
    }

    private static int getHandTypeWithJokers(String hand) {
        var jokerCount = (int) hand.chars().filter(c -> c == 'J').count();

        if (jokerCount == 0) {
            return getHandType(hand);
        }
        if (jokerCount == 5) {
            return FIVE_OF_A_KIND;
        }

        // Count non-joker cards
        var counts = new int[13];
        for (char c : hand.toCharArray()) {
            if (c != 'J') {
                var idx = CARD_STRENGTH.indexOf(c);
                counts[idx]++;
            }
        }

        Arrays.sort(counts);
        // Add jokers to the highest count
        counts[12] += jokerCount;

        return classifyHand(counts);
    }

    private static int[] getCardCounts(String hand) {
        var counts = new int[13];
        for (char c : hand.toCharArray()) {
            var idx = CARD_STRENGTH.indexOf(c);
            counts[idx]++;
        }
        Arrays.sort(counts);
        return counts;
    }

    private static int classifyHand(int[] counts) {
        // counts is sorted ascending, so counts[12] is highest, counts[11] is second highest
        var highest = counts[12];
        var secondHighest = counts[11];

        if (highest == 5) return FIVE_OF_A_KIND;
        if (highest == 4) return FOUR_OF_A_KIND;
        if (highest == 3 && secondHighest == 2) return FULL_HOUSE;
        if (highest == 3) return THREE_OF_A_KIND;
        if (highest == 2 && secondHighest == 2) return TWO_PAIR;
        if (highest == 2) return ONE_PAIR;
        return HIGH_CARD;
    }

    private static CardValues getCardValues(String hand, String cardStrength) {
        var values = new int[5];
        for (var i = 0; i < 5; i++) {
            values[i] = cardStrength.indexOf(hand.charAt(i));
        }
        return new CardValues(values);
    }

    private record Hand(String cards, int bid) {}

    private record CardValues(int[] values) implements Comparable<CardValues> {
        @Override
        public int compareTo(CardValues other) {
            return Arrays.compare(this.values, other.values);
        }
    }
}
