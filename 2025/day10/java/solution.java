import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;
import java.math.BigInteger;

public class solution {

    // Part 1: Parse machine line for indicator lights and buttons
    static class MachinePart1 {
        int nLights;
        long target;
        long[] buttonMasks;

        MachinePart1(int nLights, long target, long[] buttonMasks) {
            this.nLights = nLights;
            this.target = target;
            this.buttonMasks = buttonMasks;
        }
    }

    static MachinePart1 parseLinePart1(String line) {
        // Extract indicator pattern [.##.]
        Pattern indicatorPattern = Pattern.compile("\\[([.#]+)\\]");
        Matcher indicatorMatcher = indicatorPattern.matcher(line);
        if (!indicatorMatcher.find()) {
            throw new RuntimeException("No indicator pattern found");
        }

        String indicator = indicatorMatcher.group(1);
        int nLights = indicator.length();

        // Target state: 1 where # appears
        long target = 0;
        for (int i = 0; i < indicator.length(); i++) {
            if (indicator.charAt(i) == '#') {
                target |= (1L << i);
            }
        }

        // Extract button schematics (0,1,2) etc.
        Pattern buttonPattern = Pattern.compile("\\(([0-9,]+)\\)");
        Matcher buttonMatcher = buttonPattern.matcher(line);
        List<Long> buttons = new ArrayList<>();

        while (buttonMatcher.find()) {
            String[] indices = buttonMatcher.group(1).split(",");
            long mask = 0;
            for (String idx : indices) {
                mask |= (1L << Integer.parseInt(idx));
            }
            buttons.add(mask);
        }

        long[] buttonArray = new long[buttons.size()];
        for (int i = 0; i < buttons.size(); i++) {
            buttonArray[i] = buttons.get(i);
        }

        return new MachinePart1(nLights, target, buttonArray);
    }

    // Part 2: Parse machine line for joltage requirements
    static class MachinePart2 {
        int nCounters;
        int[] joltage;
        List<Integer>[] buttons;

        MachinePart2(int nCounters, int[] joltage, List<Integer>[] buttons) {
            this.nCounters = nCounters;
            this.joltage = joltage;
            this.buttons = buttons;
        }
    }

    @SuppressWarnings("unchecked")
    static MachinePart2 parseLinePart2(String line) {
        // Extract joltage requirements {3,5,4,7}
        Pattern joltagePattern = Pattern.compile("\\{([0-9,]+)\\}");
        Matcher joltageMatcher = joltagePattern.matcher(line);
        if (!joltageMatcher.find()) {
            throw new RuntimeException("No joltage pattern found");
        }

        String[] joltageStrs = joltageMatcher.group(1).split(",");
        int[] joltage = new int[joltageStrs.length];
        for (int i = 0; i < joltageStrs.length; i++) {
            joltage[i] = Integer.parseInt(joltageStrs[i]);
        }
        int nCounters = joltage.length;

        // Extract button schematics (0,1,2) etc.
        Pattern buttonPattern = Pattern.compile("\\(([0-9,]+)\\)");
        Matcher buttonMatcher = buttonPattern.matcher(line);
        List<List<Integer>> buttonsList = new ArrayList<>();

        while (buttonMatcher.find()) {
            String[] indices = buttonMatcher.group(1).split(",");
            List<Integer> button = new ArrayList<>();
            for (String idx : indices) {
                button.add(Integer.parseInt(idx));
            }
            buttonsList.add(button);
        }

        List<Integer>[] buttons = new List[buttonsList.size()];
        for (int i = 0; i < buttonsList.size(); i++) {
            buttons[i] = buttonsList.get(i);
        }

        return new MachinePart2(nCounters, joltage, buttons);
    }

    // Count set bits
    static int countBits(long n) {
        return Long.bitCount(n);
    }

    // Solve Part 1: brute force all combinations
    static int solveMachinePart1(MachinePart1 machine) {
        int nButtons = machine.buttonMasks.length;
        int minPresses = Integer.MAX_VALUE;

        // Try all 2^n_buttons combinations
        for (long mask = 0; mask < (1L << nButtons); mask++) {
            long state = 0;
            int presses = 0;

            for (int i = 0; i < nButtons; i++) {
                if ((mask & (1L << i)) != 0) {
                    state ^= machine.buttonMasks[i];
                    presses++;
                }
            }

            if (state == machine.target) {
                minPresses = Math.min(minPresses, presses);
            }
        }

        return minPresses == Integer.MAX_VALUE ? 0 : minPresses;
    }

    // Rational number class for Part 2
    static class Rational {
        BigInteger num;
        BigInteger den;

        Rational(long n) {
            this(BigInteger.valueOf(n), BigInteger.ONE);
        }

        Rational(BigInteger n, BigInteger d) {
            if (d.equals(BigInteger.ZERO)) {
                throw new ArithmeticException("Division by zero");
            }
            if (d.compareTo(BigInteger.ZERO) < 0) {
                n = n.negate();
                d = d.negate();
            }
            BigInteger g = n.gcd(d);
            this.num = n.divide(g);
            this.den = d.divide(g);
        }

        Rational add(Rational other) {
            BigInteger n = this.num.multiply(other.den).add(other.num.multiply(this.den));
            BigInteger d = this.den.multiply(other.den);
            return new Rational(n, d);
        }

        Rational subtract(Rational other) {
            BigInteger n = this.num.multiply(other.den).subtract(other.num.multiply(this.den));
            BigInteger d = this.den.multiply(other.den);
            return new Rational(n, d);
        }

        Rational multiply(Rational other) {
            return new Rational(this.num.multiply(other.num), this.den.multiply(other.den));
        }

        Rational divide(Rational other) {
            return new Rational(this.num.multiply(other.den), this.den.multiply(other.num));
        }

        Rational negate() {
            return new Rational(this.num.negate(), this.den);
        }

        boolean isZero() {
            return num.equals(BigInteger.ZERO);
        }

        boolean isNegative() {
            return num.compareTo(BigInteger.ZERO) < 0;
        }

        boolean isInteger() {
            return den.equals(BigInteger.ONE);
        }

        int toInt() {
            return num.divide(den).intValue();
        }

        double toDouble() {
            return num.doubleValue() / den.doubleValue();
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof Rational)) return false;
            Rational r = (Rational) o;
            return num.equals(r.num) && den.equals(r.den);
        }

        @Override
        public String toString() {
            if (den.equals(BigInteger.ONE)) return num.toString();
            return num + "/" + den;
        }
    }

    // Solve Part 2: Gaussian elimination with rational arithmetic
    static int solveMachinePart2(MachinePart2 machine) {
        int nButtons = machine.buttons.length;
        int nCounters = machine.nCounters;

        if (nButtons == 0) {
            for (int j : machine.joltage) {
                if (j != 0) return Integer.MAX_VALUE;
            }
            return 0;
        }

        // Build matrix A (nCounters x nButtons)
        Rational[][] A = new Rational[nCounters][nButtons];
        for (int i = 0; i < nCounters; i++) {
            for (int j = 0; j < nButtons; j++) {
                A[i][j] = new Rational(0);
            }
        }

        for (int j = 0; j < nButtons; j++) {
            for (int idx : machine.buttons[j]) {
                if (idx < nCounters) {
                    A[idx][j] = new Rational(1);
                }
            }
        }

        Rational[] b = new Rational[nCounters];
        for (int i = 0; i < nCounters; i++) {
            b[i] = new Rational(machine.joltage[i]);
        }

        // Augmented matrix [A | b]
        Rational[][] aug = new Rational[nCounters][nButtons + 1];
        for (int i = 0; i < nCounters; i++) {
            for (int j = 0; j < nButtons; j++) {
                aug[i][j] = A[i][j];
            }
            aug[i][nButtons] = b[i];
        }

        // Gaussian elimination with partial pivoting
        List<int[]> pivotCols = new ArrayList<>();
        int pivotRow = 0;

        for (int col = 0; col < nButtons; col++) {
            // Find non-zero entry in this column
            int found = -1;
            for (int row = pivotRow; row < nCounters; row++) {
                if (!aug[row][col].isZero()) {
                    found = row;
                    break;
                }
            }

            if (found == -1) continue;

            // Swap rows
            Rational[] temp = aug[pivotRow];
            aug[pivotRow] = aug[found];
            aug[found] = temp;

            pivotCols.add(new int[]{col, pivotRow});

            // Scale pivot row
            Rational scale = aug[pivotRow][col];
            for (int c = 0; c <= nButtons; c++) {
                aug[pivotRow][c] = aug[pivotRow][c].divide(scale);
            }

            // Eliminate column in other rows
            for (int row = 0; row < nCounters; row++) {
                if (row != pivotRow && !aug[row][col].isZero()) {
                    Rational factor = aug[row][col];
                    for (int c = 0; c <= nButtons; c++) {
                        aug[row][c] = aug[row][c].subtract(factor.multiply(aug[pivotRow][c]));
                    }
                }
            }

            pivotRow++;
        }

        // Check for inconsistency
        for (int row = pivotRow; row < nCounters; row++) {
            if (!aug[row][nButtons].isZero()) {
                return Integer.MAX_VALUE; // No solution
            }
        }

        // Identify free variables
        Set<Integer> pivotColSet = new HashSet<>();
        for (int[] pc : pivotCols) {
            pivotColSet.add(pc[0]);
        }

        List<Integer> freeVars = new ArrayList<>();
        for (int c = 0; c < nButtons; c++) {
            if (!pivotColSet.contains(c)) {
                freeVars.add(c);
            }
        }

        int nFree = freeVars.size();

        // If no free variables, unique solution
        if (nFree == 0) {
            Rational[] solution = new Rational[nButtons];
            for (int i = 0; i < nButtons; i++) {
                solution[i] = new Rational(0);
            }

            for (int[] pc : pivotCols) {
                int col = pc[0];
                int row = pc[1];
                solution[col] = aug[row][nButtons];
            }

            int total = 0;
            for (Rational val : solution) {
                if (val.isNegative() || !val.isInteger()) {
                    return Integer.MAX_VALUE;
                }
                total += val.toInt();
            }
            return total;
        }

        // Extract null space vectors
        Rational[][] nullVectors = new Rational[nFree][nButtons];
        for (int i = 0; i < nFree; i++) {
            int fv = freeVars.get(i);
            for (int j = 0; j < nButtons; j++) {
                nullVectors[i][j] = new Rational(0);
            }
            nullVectors[i][fv] = new Rational(1);

            for (int[] pc : pivotCols) {
                int col = pc[0];
                int row = pc[1];
                nullVectors[i][col] = aug[row][fv].negate();
            }
        }

        // Extract particular solution
        Rational[] particular = new Rational[nButtons];
        for (int i = 0; i < nButtons; i++) {
            particular[i] = new Rational(0);
        }
        for (int[] pc : pivotCols) {
            int col = pc[0];
            int row = pc[1];
            particular[col] = aug[row][nButtons];
        }

        // Search for optimal solution
        int maxJ = 0;
        for (int j : machine.joltage) {
            maxJ = Math.max(maxJ, j);
        }
        if (maxJ == 0) maxJ = 100;

        return searchOptimalSolution(nButtons, nFree, particular, nullVectors, maxJ);
    }

    static int searchOptimalSolution(int nButtons, int nFree, Rational[] particular, Rational[][] nullVectors, int maxJ) {
        if (nFree == 1) {
            return searchOptimal1D(nButtons, particular, nullVectors[0], maxJ);
        } else if (nFree == 2) {
            return searchOptimal2D(nButtons, particular, nullVectors, maxJ);
        } else if (nFree == 3) {
            return searchOptimal3D(nButtons, particular, nullVectors, maxJ);
        } else if (nFree <= 6) {
            return searchOptimalND(nButtons, nFree, particular, nullVectors, maxJ);
        }
        return 0; // Fallback
    }

    static int searchOptimal1D(int nButtons, Rational[] particular, Rational[] nullVector, int maxJ) {
        double tLow = Double.NEGATIVE_INFINITY;
        double tHigh = Double.POSITIVE_INFINITY;

        for (int j = 0; j < nButtons; j++) {
            double p = particular[j].toDouble();
            double nv = nullVector[j].toDouble();

            if (nv == 0) {
                if (p < 0) return Integer.MAX_VALUE;
            } else if (nv > 0) {
                tLow = Math.max(tLow, -p / nv);
            } else {
                tHigh = Math.min(tHigh, -p / nv);
            }
        }

        if (tLow > tHigh) return Integer.MAX_VALUE;

        int tLowInt = (int) Math.ceil(tLow);
        int tHighInt = (int) Math.floor(tHigh);

        int minTotal = Integer.MAX_VALUE;
        for (int t = tLowInt; t <= tHighInt; t++) {
            Rational tFrac = new Rational(t);
            int total = 0;
            boolean valid = true;

            for (int j = 0; j < nButtons; j++) {
                Rational val = particular[j].add(tFrac.multiply(nullVector[j]));
                if (val.isNegative() || !val.isInteger()) {
                    valid = false;
                    break;
                }
                total += val.toInt();
            }

            if (valid) {
                minTotal = Math.min(minTotal, total);
            }
        }

        return minTotal;
    }

    static int searchOptimal2D(int nButtons, Rational[] particular, Rational[][] nullVectors, int maxJ) {
        int minTotal = Integer.MAX_VALUE;

        int bound = maxJ * 2;
        for (int t0 = -bound; t0 <= bound; t0++) {
            Rational t0Frac = new Rational(t0);
            Rational[] inter0 = new Rational[nButtons];
            for (int j = 0; j < nButtons; j++) {
                inter0[j] = particular[j].add(t0Frac.multiply(nullVectors[0][j]));
            }

            double t1Low = Double.NEGATIVE_INFINITY;
            double t1High = Double.POSITIVE_INFINITY;

            for (int j = 0; j < nButtons; j++) {
                double p = inter0[j].toDouble();
                double nv = nullVectors[1][j].toDouble();

                if (nv > 0) {
                    t1Low = Math.max(t1Low, -p / nv);
                } else if (nv < 0) {
                    t1High = Math.min(t1High, -p / nv);
                }
            }

            int t1LowInt = (int) Math.ceil(t1Low);
            int t1HighInt = (int) Math.floor(t1High);

            for (int t1 = t1LowInt; t1 <= t1HighInt; t1++) {
                Rational t1Frac = new Rational(t1);
                int total = 0;
                boolean valid = true;

                for (int j = 0; j < nButtons; j++) {
                    Rational val = inter0[j].add(t1Frac.multiply(nullVectors[1][j]));
                    if (val.isNegative() || !val.isInteger()) {
                        valid = false;
                        break;
                    }
                    total += val.toInt();
                }

                if (valid && total < minTotal) {
                    minTotal = total;
                }
            }
        }

        return minTotal;
    }

    static int searchOptimal3D(int nButtons, Rational[] particular, Rational[][] nullVectors, int maxJ) {
        int minTotal = Integer.MAX_VALUE;
        int bound = maxJ;

        for (int t0 = -bound; t0 <= bound; t0++) {
            Rational t0Frac = new Rational(t0);
            Rational[] inter0 = new Rational[nButtons];
            for (int j = 0; j < nButtons; j++) {
                inter0[j] = particular[j].add(t0Frac.multiply(nullVectors[0][j]));
            }

            double t1Low = Double.NEGATIVE_INFINITY;
            double t1High = Double.POSITIVE_INFINITY;

            for (int j = 0; j < nButtons; j++) {
                double p = inter0[j].toDouble();
                double nv = nullVectors[1][j].toDouble();

                if (nv > 0) {
                    t1Low = Math.max(t1Low, -p / nv - bound);
                } else if (nv < 0) {
                    t1High = Math.min(t1High, -p / nv + bound);
                }
            }

            int t1LowInt = Math.max((int) Math.ceil(t1Low), -bound);
            int t1HighInt = Math.min((int) Math.floor(t1High), bound);

            for (int t1 = t1LowInt; t1 <= t1HighInt; t1++) {
                Rational t1Frac = new Rational(t1);
                Rational[] inter1 = new Rational[nButtons];
                for (int j = 0; j < nButtons; j++) {
                    inter1[j] = inter0[j].add(t1Frac.multiply(nullVectors[1][j]));
                }

                double t2Low = Double.NEGATIVE_INFINITY;
                double t2High = Double.POSITIVE_INFINITY;

                for (int j = 0; j < nButtons; j++) {
                    double p = inter1[j].toDouble();
                    double nv = nullVectors[2][j].toDouble();

                    if (nv > 0) {
                        t2Low = Math.max(t2Low, -p / nv);
                    } else if (nv < 0) {
                        t2High = Math.min(t2High, -p / nv);
                    }
                }

                int t2LowInt = (int) Math.ceil(t2Low);
                int t2HighInt = (int) Math.floor(t2High);

                for (int t2 = t2LowInt; t2 <= t2HighInt; t2++) {
                    Rational t2Frac = new Rational(t2);
                    int total = 0;
                    boolean valid = true;

                    for (int j = 0; j < nButtons; j++) {
                        Rational val = inter1[j].add(t2Frac.multiply(nullVectors[2][j]));
                        if (val.isNegative() || !val.isInteger()) {
                            valid = false;
                            break;
                        }
                        total += val.toInt();
                    }

                    if (valid && total < minTotal) {
                        minTotal = total;
                    }
                }
            }
        }

        return minTotal;
    }

    static int searchOptimalND(int nButtons, int nFree, Rational[] particular, Rational[][] nullVectors, int maxJ) {
        int[] minTotal = {Integer.MAX_VALUE};
        searchRecursive(0, nFree, nButtons, particular.clone(), nullVectors, maxJ, minTotal);
        return minTotal[0];
    }

    static void searchRecursive(int idx, int nFree, int nButtons, Rational[] partial, Rational[][] nullVectors, int maxJ, int[] minTotal) {
        if (idx == nFree) {
            int total = 0;
            boolean valid = true;
            for (Rational val : partial) {
                if (val.isNegative() || !val.isInteger()) {
                    valid = false;
                    break;
                }
                total += val.toInt();
            }
            if (valid) {
                minTotal[0] = Math.min(minTotal[0], total);
            }
            return;
        }

        double tLow = Double.NEGATIVE_INFINITY;
        double tHigh = Double.POSITIVE_INFINITY;

        for (int j = 0; j < nButtons; j++) {
            double p = partial[j].toDouble();
            double nv = nullVectors[idx][j].toDouble();

            if (nv > 0) {
                tLow = Math.max(tLow, -p / nv);
            } else if (nv < 0) {
                tHigh = Math.min(tHigh, -p / nv);
            }
        }

        if (tLow > tHigh || Double.isInfinite(tLow) || Double.isInfinite(tHigh)) {
            return;
        }

        int tLowInt = Math.max((int) Math.ceil(tLow) - maxJ, -maxJ * 2);
        int tHighInt = Math.min((int) Math.floor(tHigh) + maxJ, maxJ * 2);

        for (int t = tLowInt; t <= tHighInt; t++) {
            Rational tFrac = new Rational(t);
            Rational[] newPartial = new Rational[nButtons];
            for (int j = 0; j < nButtons; j++) {
                newPartial[j] = partial[j].add(tFrac.multiply(nullVectors[idx][j]));
            }
            searchRecursive(idx + 1, nFree, nButtons, newPartial, nullVectors, maxJ, minTotal);
        }
    }

    static int part1(List<String> lines) {
        int total = 0;
        for (String line : lines) {
            line = line.trim();
            if (line.isEmpty()) continue;

            MachinePart1 machine = parseLinePart1(line);
            int minPresses = solveMachinePart1(machine);
            total += minPresses;
        }
        return total;
    }

    static int part2(List<String> lines) {
        int total = 0;
        for (String line : lines) {
            line = line.trim();
            if (line.isEmpty()) continue;

            MachinePart2 machine = parseLinePart2(line);
            int minPresses = solveMachinePart2(machine);
            total += minPresses;
        }
        return total;
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get(System.getProperty("user.dir")).getParent().resolve("input.txt");
        List<String> lines = Files.readAllLines(inputPath);

        System.out.println("Part 1: " + part1(lines));
        System.out.println("Part 2: " + part2(lines));
    }
}
