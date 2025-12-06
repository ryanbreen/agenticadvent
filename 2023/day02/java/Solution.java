import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Solution {

    static class CubeSet {
        int red;
        int green;
        int blue;

        CubeSet(int red, int green, int blue) {
            this.red = red;
            this.green = green;
            this.blue = blue;
        }
    }

    static class Game {
        int id;
        List<CubeSet> draws;

        Game(int id, List<CubeSet> draws) {
            this.id = id;
            this.draws = draws;
        }
    }

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Paths.get("../input.txt")).trim();
        List<String> lines = List.of(input.split("\n"));

        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: " + part2(lines));
    }

    static int part1(List<String> lines) {
        int sum = 0;
        int maxRed = 12;
        int maxGreen = 13;
        int maxBlue = 14;

        for (String line : lines) {
            Game game = parseGame(line);
            boolean possible = true;

            for (CubeSet draw : game.draws) {
                if (draw.red > maxRed || draw.green > maxGreen || draw.blue > maxBlue) {
                    possible = false;
                    break;
                }
            }

            if (possible) {
                sum += game.id;
            }
        }

        return sum;
    }

    static int part2(List<String> lines) {
        int sum = 0;

        for (String line : lines) {
            Game game = parseGame(line);
            int minRed = 0;
            int minGreen = 0;
            int minBlue = 0;

            // Find the minimum cubes needed for each color
            for (CubeSet draw : game.draws) {
                minRed = Math.max(minRed, draw.red);
                minGreen = Math.max(minGreen, draw.green);
                minBlue = Math.max(minBlue, draw.blue);
            }

            int power = minRed * minGreen * minBlue;
            sum += power;
        }

        return sum;
    }

    static Game parseGame(String line) {
        // Parse "Game X: ..."
        Pattern gamePattern = Pattern.compile("Game (\\d+): (.+)");
        Matcher gameMatcher = gamePattern.matcher(line);

        if (!gameMatcher.matches()) {
            throw new RuntimeException("Invalid game line: " + line);
        }

        int gameId = Integer.parseInt(gameMatcher.group(1));
        String drawsStr = gameMatcher.group(2);

        // Split by semicolons to get each draw
        String[] drawStrings = drawsStr.split(";");
        List<CubeSet> draws = new java.util.ArrayList<>();

        for (String drawStr : drawStrings) {
            draws.add(parseDraw(drawStr.trim()));
        }

        return new Game(gameId, draws);
    }

    static CubeSet parseDraw(String drawStr) {
        int red = 0;
        int green = 0;
        int blue = 0;

        // Split by commas to get each color count
        String[] parts = drawStr.split(",");

        for (String part : parts) {
            part = part.trim();

            // Parse "N color"
            String[] tokens = part.split(" ");
            int count = Integer.parseInt(tokens[0]);
            String color = tokens[1];

            switch (color) {
                case "red":
                    red = count;
                    break;
                case "green":
                    green = count;
                    break;
                case "blue":
                    blue = count;
                    break;
            }
        }

        return new CubeSet(red, green, blue);
    }
}
