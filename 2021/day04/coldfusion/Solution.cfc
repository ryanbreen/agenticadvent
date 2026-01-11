component {

    /**
     * Parse input file and return struct with numbers and boards
     */
    public struct function parseInput(required string inputPath) {
        var content = fileRead(arguments.inputPath);
        // Use includeEmptyFields = true to preserve blank lines
        var lines = listToArray(content, chr(10), true);

        // First line is the drawn numbers
        var numbersLine = trim(lines[1]);
        var numbers = [];
        for (var n in listToArray(numbersLine, ",")) {
            arrayAppend(numbers, int(trim(n)));
        }

        // Parse boards - each board is 5 lines separated by blank lines
        var boards = [];
        var currentBoard = [];

        for (var i = 2; i <= arrayLen(lines); i++) {
            var line = trim(lines[i]);

            if (len(line) == 0) {
                // Empty line - save current board if complete
                if (arrayLen(currentBoard) == 5) {
                    arrayAppend(boards, currentBoard);
                    currentBoard = [];
                }
            } else {
                // Parse row of numbers
                var row = [];
                var parts = reMatch("\d+", line);
                for (var part in parts) {
                    arrayAppend(row, int(part));
                }
                if (arrayLen(row) == 5) {
                    arrayAppend(currentBoard, row);
                }
            }
        }

        // Don't forget the last board
        if (arrayLen(currentBoard) == 5) {
            arrayAppend(boards, currentBoard);
        }

        return {
            "numbers": numbers,
            "boards": boards
        };
    }

    /**
     * Create a 5x5 marked array initialized to false
     */
    private array function createMarkedBoard() {
        var marked = [];
        for (var r = 1; r <= 5; r++) {
            var row = [];
            for (var c = 1; c <= 5; c++) {
                arrayAppend(row, false);
            }
            arrayAppend(marked, row);
        }
        return marked;
    }

    /**
     * Mark a number on a board
     */
    private void function markNumber(required array board, required array marked, required numeric number) {
        for (var row = 1; row <= 5; row++) {
            for (var col = 1; col <= 5; col++) {
                if (board[row][col] == number) {
                    marked[row][col] = true;
                }
            }
        }
    }

    /**
     * Check if a board has won (complete row or column)
     */
    private boolean function checkWinner(required array marked) {
        // Check rows
        for (var row = 1; row <= 5; row++) {
            var rowComplete = true;
            for (var col = 1; col <= 5; col++) {
                if (!marked[row][col]) {
                    rowComplete = false;
                    break;
                }
            }
            if (rowComplete) return true;
        }

        // Check columns
        for (var col = 1; col <= 5; col++) {
            var colComplete = true;
            for (var row = 1; row <= 5; row++) {
                if (!marked[row][col]) {
                    colComplete = false;
                    break;
                }
            }
            if (colComplete) return true;
        }

        return false;
    }

    /**
     * Calculate the score for a winning board
     */
    private numeric function calculateScore(required array board, required array marked, required numeric lastNumber) {
        var unmarkedSum = 0;
        for (var row = 1; row <= 5; row++) {
            for (var col = 1; col <= 5; col++) {
                if (!marked[row][col]) {
                    unmarkedSum += board[row][col];
                }
            }
        }
        return unmarkedSum * lastNumber;
    }

    /**
     * Part 1: Find the first winning board and calculate its score
     */
    public numeric function part1(required array numbers, required array boards) {
        // Create marked arrays for all boards
        var marked = [];
        for (var i = 1; i <= arrayLen(boards); i++) {
            arrayAppend(marked, createMarkedBoard());
        }

        // Draw numbers
        for (var number in numbers) {
            for (var i = 1; i <= arrayLen(boards); i++) {
                markNumber(boards[i], marked[i], number);
                if (checkWinner(marked[i])) {
                    return calculateScore(boards[i], marked[i], number);
                }
            }
        }

        return 0;
    }

    /**
     * Part 2: Find the last winning board and calculate its score
     */
    public numeric function part2(required array numbers, required array boards) {
        // Create marked arrays for all boards
        var marked = [];
        var won = [];
        for (var i = 1; i <= arrayLen(boards); i++) {
            arrayAppend(marked, createMarkedBoard());
            arrayAppend(won, false);
        }

        var lastScore = 0;

        // Draw numbers
        for (var number in numbers) {
            for (var i = 1; i <= arrayLen(boards); i++) {
                if (won[i]) continue;

                markNumber(boards[i], marked[i], number);
                if (checkWinner(marked[i])) {
                    won[i] = true;
                    lastScore = calculateScore(boards[i], marked[i], number);
                }
            }
        }

        return lastScore;
    }

}
