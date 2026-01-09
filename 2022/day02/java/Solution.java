import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

public class Solution {

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of(System.getProperty("user.dir")).resolve("../input.txt");
        List<String> lines = Files.readAllLines(inputPath);

        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: " + part2(lines));
    }

    private static int part1(List<String> lines) {
        // Shape scores: X=Rock=1, Y=Paper=2, Z=Scissors=3
        Map<Character, Integer> shapeScore = Map.of('X', 1, 'Y', 2, 'Z', 3);

        // Outcome scores based on (opponent, me)
        // A=Rock, B=Paper, C=Scissors
        // 0=loss, 3=draw, 6=win
        Map<String, Integer> outcomes = Map.of(
            "AX", 3,  // Rock vs Rock = draw
            "AY", 6,  // Rock vs Paper = win
            "AZ", 0,  // Rock vs Scissors = loss
            "BX", 0,  // Paper vs Rock = loss
            "BY", 3,  // Paper vs Paper = draw
            "BZ", 6,  // Paper vs Scissors = win
            "CX", 6,  // Scissors vs Rock = win
            "CY", 0,  // Scissors vs Paper = loss
            "CZ", 3   // Scissors vs Scissors = draw
        );

        int total = 0;
        for (String line : lines) {
            if (line.isBlank()) continue;
            String[] parts = line.split(" ");
            char opponent = parts[0].charAt(0);
            char me = parts[1].charAt(0);
            String key = "" + opponent + me;
            total += shapeScore.get(me) + outcomes.get(key);
        }
        return total;
    }

    private static int part2(List<String> lines) {
        // What shape to play given opponent and desired outcome
        // Returns the shape score (1=Rock, 2=Paper, 3=Scissors)
        Map<String, Integer> choices = Map.of(
            "AX", 3,  // Rock, need to lose -> Scissors
            "AY", 1,  // Rock, need to draw -> Rock
            "AZ", 2,  // Rock, need to win -> Paper
            "BX", 1,  // Paper, need to lose -> Rock
            "BY", 2,  // Paper, need to draw -> Paper
            "BZ", 3,  // Paper, need to win -> Scissors
            "CX", 2,  // Scissors, need to lose -> Paper
            "CY", 3,  // Scissors, need to draw -> Scissors
            "CZ", 1   // Scissors, need to win -> Rock
        );

        // Outcome scores: X=lose=0, Y=draw=3, Z=win=6
        Map<Character, Integer> outcomeScore = Map.of('X', 0, 'Y', 3, 'Z', 6);

        int total = 0;
        for (String line : lines) {
            if (line.isBlank()) continue;
            String[] parts = line.split(" ");
            char opponent = parts[0].charAt(0);
            char outcome = parts[1].charAt(0);
            String key = "" + opponent + outcome;
            total += choices.get(key) + outcomeScore.get(outcome);
        }
        return total;
    }
}
