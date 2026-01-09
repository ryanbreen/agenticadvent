component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Parse input into list of rounds
        var rounds = parseInput(inputText);

        // Solve both parts
        var part1Result = part1(rounds);
        var part2Result = part2(rounds);

        writeOutput("Part 1: " & part1Result & chr(10));
        writeOutput("Part 2: " & part2Result & chr(10));
    }

    function parseInput(inputText) {
        // Parse each line into [opponent, second_column] pairs
        var lines = listToArray(inputText, chr(10));
        var rounds = [];

        for (var line in lines) {
            var trimmedLine = trim(line);
            if (len(trimmedLine) > 0) {
                var parts = listToArray(trimmedLine, " ");
                arrayAppend(rounds, {
                    "opponent": parts[1],
                    "second": parts[2]
                });
            }
        }

        return rounds;
    }

    function part1(rounds) {
        // X=Rock, Y=Paper, Z=Scissors
        // Shape scores: Rock=1, Paper=2, Scissors=3
        var shapeScore = {
            "X": 1,
            "Y": 2,
            "Z": 3
        };

        // Outcome scores: 0=loss, 3=draw, 6=win
        // A=Rock, B=Paper, C=Scissors
        var outcomes = {
            "A_X": 3,  // Rock vs Rock = draw
            "A_Y": 6,  // Rock vs Paper = win
            "A_Z": 0,  // Rock vs Scissors = loss
            "B_X": 0,  // Paper vs Rock = loss
            "B_Y": 3,  // Paper vs Paper = draw
            "B_Z": 6,  // Paper vs Scissors = win
            "C_X": 6,  // Scissors vs Rock = win
            "C_Y": 0,  // Scissors vs Paper = loss
            "C_Z": 3   // Scissors vs Scissors = draw
        };

        var total = 0;
        for (var round in rounds) {
            var key = round.opponent & "_" & round.second;
            total += shapeScore[round.second] + outcomes[key];
        }

        return total;
    }

    function part2(rounds) {
        // X=lose, Y=draw, Z=win
        // What shape to play given opponent and desired outcome
        // Returns the shape score (1=Rock, 2=Paper, 3=Scissors)
        var choices = {
            "A_X": 3,  // Rock, need to lose -> Scissors (score 3)
            "A_Y": 1,  // Rock, need to draw -> Rock (score 1)
            "A_Z": 2,  // Rock, need to win -> Paper (score 2)
            "B_X": 1,  // Paper, need to lose -> Rock (score 1)
            "B_Y": 2,  // Paper, need to draw -> Paper (score 2)
            "B_Z": 3,  // Paper, need to win -> Scissors (score 3)
            "C_X": 2,  // Scissors, need to lose -> Paper (score 2)
            "C_Y": 3,  // Scissors, need to draw -> Scissors (score 3)
            "C_Z": 1   // Scissors, need to win -> Rock (score 1)
        };

        // Outcome scores: X=lose(0), Y=draw(3), Z=win(6)
        var outcomeScore = {
            "X": 0,
            "Y": 3,
            "Z": 6
        };

        var total = 0;
        for (var round in rounds) {
            var key = round.opponent & "_" & round.second;
            total += choices[key] + outcomeScore[round.second];
        }

        return total;
    }
}
