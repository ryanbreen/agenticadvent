import java.io.IOException;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class Solution {

    static class Problem {
        List<BigInteger> numbers;
        char operator;

        Problem(List<BigInteger> numbers, char operator) {
            this.numbers = numbers;
            this.operator = operator;
        }
    }

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Path.of("../input.txt")).trim();
        String[] lines = input.split("\n");

        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: " + part2(lines));
    }

    static BigInteger part1(String[] lines) {
        List<Problem> problems = parseProblems(lines);
        BigInteger total = BigInteger.ZERO;

        for (Problem problem : problems) {
            BigInteger result = solveProblem(problem.numbers, problem.operator);
            total = total.add(result);
        }

        return total;
    }

    static BigInteger part2(String[] lines) {
        List<Problem> problems = parseProblemsPart2(lines);
        BigInteger total = BigInteger.ZERO;

        for (Problem problem : problems) {
            BigInteger result = solveProblem(problem.numbers, problem.operator);
            total = total.add(result);
        }

        return total;
    }

    static List<Problem> parseProblems(String[] lines) {
        if (lines.length == 0) {
            return new ArrayList<>();
        }

        // Find the operator row (last non-empty row with only +, *, and spaces)
        int opRowIdx = lines.length - 1;
        while (opRowIdx >= 0) {
            String line = lines[opRowIdx];
            if (line.trim().isEmpty() || !isOperatorRow(line)) {
                opRowIdx--;
            } else {
                break;
            }
        }

        if (opRowIdx < 0) {
            return new ArrayList<>();
        }

        String opRow = lines[opRowIdx];
        String[] numberRows = new String[opRowIdx];
        System.arraycopy(lines, 0, numberRows, 0, opRowIdx);

        // Find max width
        int maxWidth = 0;
        for (String line : lines) {
            maxWidth = Math.max(maxWidth, line.length());
        }

        // Pad all rows to the same width
        String[] paddedNumberRows = new String[numberRows.length];
        for (int i = 0; i < numberRows.length; i++) {
            paddedNumberRows[i] = padRight(numberRows[i], maxWidth);
        }
        String paddedOpRow = padRight(opRow, maxWidth);

        // Find problem boundaries
        List<Problem> problems = new ArrayList<>();
        int col = 0;

        while (col < maxWidth) {
            // Skip separator columns (all spaces)
            while (col < maxWidth && isAllSpaces(paddedNumberRows, col) && paddedOpRow.charAt(col) == ' ') {
                col++;
            }

            if (col >= maxWidth) {
                break;
            }

            // Find the end of this problem
            int startCol = col;
            while (col < maxWidth) {
                boolean isSeparator = isAllSpaces(paddedNumberRows, col) && paddedOpRow.charAt(col) == ' ';
                if (isSeparator) {
                    break;
                }
                col++;
            }

            int endCol = col;

            // Extract numbers and operator for this problem
            List<BigInteger> numbers = new ArrayList<>();
            for (String row : paddedNumberRows) {
                String numStr = row.substring(startCol, endCol).trim();
                if (!numStr.isEmpty()) {
                    numbers.add(new BigInteger(numStr));
                }
            }

            String opStr = paddedOpRow.substring(startCol, endCol).trim();
            if (!opStr.isEmpty() && !numbers.isEmpty()) {
                problems.add(new Problem(numbers, opStr.charAt(0)));
            }
        }

        return problems;
    }

    static List<Problem> parseProblemsPart2(String[] lines) {
        if (lines.length == 0) {
            return new ArrayList<>();
        }

        // Find the operator row
        int opRowIdx = lines.length - 1;
        while (opRowIdx >= 0) {
            String line = lines[opRowIdx];
            if (line.trim().isEmpty() || !isOperatorRow(line)) {
                opRowIdx--;
            } else {
                break;
            }
        }

        if (opRowIdx < 0) {
            return new ArrayList<>();
        }

        String opRow = lines[opRowIdx];
        String[] numberRows = new String[opRowIdx];
        System.arraycopy(lines, 0, numberRows, 0, opRowIdx);

        // Find max width
        int maxWidth = 0;
        for (String line : lines) {
            maxWidth = Math.max(maxWidth, line.length());
        }

        // Pad all rows to the same width
        String[] paddedNumberRows = new String[numberRows.length];
        for (int i = 0; i < numberRows.length; i++) {
            paddedNumberRows[i] = padRight(numberRows[i], maxWidth);
        }
        String paddedOpRow = padRight(opRow, maxWidth);

        // Find problem boundaries
        List<Problem> problems = new ArrayList<>();
        int col = 0;

        while (col < maxWidth) {
            // Skip separator columns
            while (col < maxWidth && isAllSpaces(paddedNumberRows, col) && paddedOpRow.charAt(col) == ' ') {
                col++;
            }

            if (col >= maxWidth) {
                break;
            }

            // Find the end of this problem
            int startCol = col;
            while (col < maxWidth) {
                boolean isSeparator = isAllSpaces(paddedNumberRows, col) && paddedOpRow.charAt(col) == ' ';
                if (isSeparator) {
                    break;
                }
                col++;
            }

            int endCol = col;

            // For Part 2: Read columns right-to-left
            List<BigInteger> numbers = new ArrayList<>();
            for (int c = endCol - 1; c >= startCol; c--) {
                StringBuilder digits = new StringBuilder();
                for (String row : paddedNumberRows) {
                    char ch = row.charAt(c);
                    if (Character.isDigit(ch)) {
                        digits.append(ch);
                    }
                }
                if (digits.length() > 0) {
                    numbers.add(new BigInteger(digits.toString()));
                }
            }

            String opStr = paddedOpRow.substring(startCol, endCol).trim();
            if (!opStr.isEmpty() && !numbers.isEmpty()) {
                problems.add(new Problem(numbers, opStr.charAt(0)));
            }
        }

        return problems;
    }

    static BigInteger solveProblem(List<BigInteger> numbers, char op) {
        if (op == '+') {
            BigInteger sum = BigInteger.ZERO;
            for (BigInteger num : numbers) {
                sum = sum.add(num);
            }
            return sum;
        } else if (op == '*') {
            BigInteger product = BigInteger.ONE;
            for (BigInteger num : numbers) {
                product = product.multiply(num);
            }
            return product;
        }
        return BigInteger.ZERO;
    }

    static boolean isOperatorRow(String line) {
        for (char c : line.toCharArray()) {
            if (c != '+' && c != '*' && c != ' ') {
                return false;
            }
        }
        return true;
    }

    static boolean isAllSpaces(String[] rows, int col) {
        for (String row : rows) {
            if (col < row.length() && row.charAt(col) != ' ') {
                return false;
            }
        }
        return true;
    }

    static String padRight(String str, int length) {
        if (str.length() >= length) {
            return str;
        }
        StringBuilder sb = new StringBuilder(str);
        while (sb.length() < length) {
            sb.append(' ');
        }
        return sb.toString();
    }
}
