import java.io.*;
import java.math.BigInteger;
import java.util.*;

public class solution {

    // Fraction class for exact arithmetic with BigInteger
    static class Fraction implements Comparable<Fraction> {
        final BigInteger num;
        final BigInteger den;

        static final Fraction ZERO = new Fraction(BigInteger.ZERO, BigInteger.ONE);
        static final Fraction ONE = new Fraction(BigInteger.ONE, BigInteger.ONE);

        Fraction(BigInteger numerator, BigInteger denominator) {
            if (denominator.equals(BigInteger.ZERO)) {
                throw new ArithmeticException("Division by zero");
            }
            // Normalize sign: denominator always positive
            if (denominator.compareTo(BigInteger.ZERO) < 0) {
                numerator = numerator.negate();
                denominator = denominator.negate();
            }
            // Reduce to lowest terms
            BigInteger gcd = numerator.gcd(denominator);
            this.num = numerator.divide(gcd);
            this.den = denominator.divide(gcd);
        }

        Fraction(long n) {
            this(BigInteger.valueOf(n), BigInteger.ONE);
        }

        Fraction(BigInteger n) {
            this(n, BigInteger.ONE);
        }

        Fraction add(Fraction other) {
            BigInteger newNum = this.num.multiply(other.den).add(other.num.multiply(this.den));
            BigInteger newDen = this.den.multiply(other.den);
            return new Fraction(newNum, newDen);
        }

        Fraction subtract(Fraction other) {
            BigInteger newNum = this.num.multiply(other.den).subtract(other.num.multiply(this.den));
            BigInteger newDen = this.den.multiply(other.den);
            return new Fraction(newNum, newDen);
        }

        Fraction multiply(Fraction other) {
            return new Fraction(this.num.multiply(other.num), this.den.multiply(other.den));
        }

        Fraction divide(Fraction other) {
            return new Fraction(this.num.multiply(other.den), this.den.multiply(other.num));
        }

        Fraction negate() {
            return new Fraction(this.num.negate(), this.den);
        }

        Fraction abs() {
            return new Fraction(this.num.abs(), this.den);
        }

        boolean isZero() {
            return this.num.equals(BigInteger.ZERO);
        }

        @Override
        public int compareTo(Fraction other) {
            BigInteger lhs = this.num.multiply(other.den);
            BigInteger rhs = other.num.multiply(this.den);
            return lhs.compareTo(rhs);
        }

        double toDouble() {
            return this.num.doubleValue() / this.den.doubleValue();
        }

        BigInteger toBigInteger() {
            return this.num.divide(this.den);
        }

        @Override
        public String toString() {
            if (this.den.equals(BigInteger.ONE)) {
                return this.num.toString();
            }
            return this.num + "/" + this.den;
        }
    }

    // Hailstone record
    static class Hailstone {
        final long px, py, pz;
        final long vx, vy, vz;

        Hailstone(long px, long py, long pz, long vx, long vy, long vz) {
            this.px = px;
            this.py = py;
            this.pz = pz;
            this.vx = vx;
            this.vy = vy;
            this.vz = vz;
        }
    }

    static List<Hailstone> parseInput(String filename) throws IOException {
        List<Hailstone> hailstones = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split("@");
                String[] pos = parts[0].split(",");
                String[] vel = parts[1].split(",");

                long px = Long.parseLong(pos[0].trim());
                long py = Long.parseLong(pos[1].trim());
                long pz = Long.parseLong(pos[2].trim());
                long vx = Long.parseLong(vel[0].trim());
                long vy = Long.parseLong(vel[1].trim());
                long vz = Long.parseLong(vel[2].trim());

                hailstones.add(new Hailstone(px, py, pz, vx, vy, vz));
            }
        }
        return hailstones;
    }

    // 2D intersection result
    static class Intersection2D {
        final double x, y, t1, t2;

        Intersection2D(double x, double y, double t1, double t2) {
            this.x = x;
            this.y = y;
            this.t1 = t1;
            this.t2 = t2;
        }
    }

    static Intersection2D findIntersection2D(Hailstone h1, Hailstone h2) {
        // Line 1: position at time t1 is (px1 + vx1*t1, py1 + vy1*t1)
        // Line 2: position at time t2 is (px2 + vx2*t2, py2 + vy2*t2)
        //
        // At intersection:
        // px1 + vx1*t1 = px2 + vx2*t2
        // py1 + vy1*t1 = py2 + vy2*t2
        //
        // vx1*t1 - vx2*t2 = px2 - px1
        // vy1*t1 - vy2*t2 = py2 - py1
        //
        // Using Cramer's rule:
        // det = vx1*(-vy2) - (-vx2)*vy1 = -vx1*vy2 + vx2*vy1

        double det = (double)h1.vx * (-h2.vy) - (double)(-h2.vx) * h1.vy;

        if (det == 0) {
            return null; // Parallel lines
        }

        double dx = h2.px - h1.px;
        double dy = h2.py - h1.py;

        double t1 = (dx * (-h2.vy) - (-h2.vx) * dy) / det;
        double t2 = ((double)h1.vx * dy - dx * h1.vy) / det;

        double x = h1.px + h1.vx * t1;
        double y = h1.py + h1.vy * t1;

        return new Intersection2D(x, y, t1, t2);
    }

    static int part1(List<Hailstone> hailstones) {
        double minCoord = 200000000000000.0;
        double maxCoord = 400000000000000.0;

        int count = 0;

        for (int i = 0; i < hailstones.size(); i++) {
            for (int j = i + 1; j < hailstones.size(); j++) {
                Intersection2D result = findIntersection2D(hailstones.get(i), hailstones.get(j));

                if (result == null) {
                    continue;
                }

                // Check if intersection is in the future for both hailstones
                if (result.t1 < 0 || result.t2 < 0) {
                    continue;
                }

                // Check if intersection is within test area
                if (result.x >= minCoord && result.x <= maxCoord &&
                    result.y >= minCoord && result.y <= maxCoord) {
                    count++;
                }
            }
        }

        return count;
    }

    // Solve system of linear equations using Gaussian elimination with Fractions
    static Fraction[] solveSystem(Fraction[][] matrix, Fraction[] rhs) {
        int n = matrix.length;

        // Create augmented matrix
        Fraction[][] aug = new Fraction[n][n + 1];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                aug[i][j] = matrix[i][j];
            }
            aug[i][n] = rhs[i];
        }

        // Forward elimination
        for (int col = 0; col < n; col++) {
            // Find pivot (row with largest absolute value in this column)
            int maxRow = col;
            for (int row = col + 1; row < n; row++) {
                if (aug[row][col].abs().compareTo(aug[maxRow][col].abs()) > 0) {
                    maxRow = row;
                }
            }

            // Swap rows
            Fraction[] temp = aug[col];
            aug[col] = aug[maxRow];
            aug[maxRow] = temp;

            if (aug[col][col].isZero()) {
                continue;
            }

            // Eliminate column
            for (int row = col + 1; row < n; row++) {
                if (!aug[row][col].isZero()) {
                    Fraction factor = aug[row][col].divide(aug[col][col]);
                    for (int j = col; j <= n; j++) {
                        aug[row][j] = aug[row][j].subtract(factor.multiply(aug[col][j]));
                    }
                }
            }
        }

        // Back substitution
        Fraction[] solution = new Fraction[n];
        for (int i = n - 1; i >= 0; i--) {
            solution[i] = aug[i][n];
            for (int j = i + 1; j < n; j++) {
                solution[i] = solution[i].subtract(aug[i][j].multiply(solution[j]));
            }
            solution[i] = solution[i].divide(aug[i][i]);
        }

        return solution;
    }

    static long part2(List<Hailstone> hailstones) {
        // Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
        // Take first 5 hailstones to get 4 pairs

        Fraction[][] matrixXY = new Fraction[4][4];
        Fraction[] rhsXY = new Fraction[4];

        for (int i = 0; i < 4; i++) {
            Hailstone h1 = hailstones.get(i);
            Hailstone h2 = hailstones.get(i + 1);

            // Coefficients for rx, ry, rvx, rvy
            // (vy1 - vy2)*rx + (vx2 - vx1)*ry + (py2 - py1)*rvx + (px1 - px2)*rvy =
            //     px1*vy1 - py1*vx1 - (px2*vy2 - py2*vx2)
            long a = h1.vy - h2.vy;
            long b = h2.vx - h1.vx;
            long c = h2.py - h1.py;
            long d = h1.px - h2.px;

            BigInteger px1 = BigInteger.valueOf(h1.px);
            BigInteger py1 = BigInteger.valueOf(h1.py);
            BigInteger vx1 = BigInteger.valueOf(h1.vx);
            BigInteger vy1 = BigInteger.valueOf(h1.vy);
            BigInteger px2 = BigInteger.valueOf(h2.px);
            BigInteger py2 = BigInteger.valueOf(h2.py);
            BigInteger vx2 = BigInteger.valueOf(h2.vx);
            BigInteger vy2 = BigInteger.valueOf(h2.vy);

            // e = px1*vy1 - py1*vx1 - (px2*vy2 - py2*vx2)
            BigInteger e = px1.multiply(vy1).subtract(py1.multiply(vx1))
                          .subtract(px2.multiply(vy2).subtract(py2.multiply(vx2)));

            matrixXY[i][0] = new Fraction(a);
            matrixXY[i][1] = new Fraction(b);
            matrixXY[i][2] = new Fraction(c);
            matrixXY[i][3] = new Fraction(d);
            rhsXY[i] = new Fraction(e);
        }

        Fraction[] solutionXY = solveSystem(matrixXY, rhsXY);
        Fraction rx = solutionXY[0];
        Fraction ry = solutionXY[1];
        // rvx = solutionXY[2], rvy = solutionXY[3]

        // Build system for XZ plane (4 equations, 4 unknowns: rx, rz, rvx, rvz)
        Fraction[][] matrixXZ = new Fraction[4][4];
        Fraction[] rhsXZ = new Fraction[4];

        for (int i = 0; i < 4; i++) {
            Hailstone h1 = hailstones.get(i);
            Hailstone h2 = hailstones.get(i + 1);

            // Same structure as XY but with Z instead of Y
            long a = h1.vz - h2.vz;
            long b = h2.vx - h1.vx;
            long c = h2.pz - h1.pz;
            long d = h1.px - h2.px;

            BigInteger px1 = BigInteger.valueOf(h1.px);
            BigInteger pz1 = BigInteger.valueOf(h1.pz);
            BigInteger vx1 = BigInteger.valueOf(h1.vx);
            BigInteger vz1 = BigInteger.valueOf(h1.vz);
            BigInteger px2 = BigInteger.valueOf(h2.px);
            BigInteger pz2 = BigInteger.valueOf(h2.pz);
            BigInteger vx2 = BigInteger.valueOf(h2.vx);
            BigInteger vz2 = BigInteger.valueOf(h2.vz);

            // e = px1*vz1 - pz1*vx1 - (px2*vz2 - pz2*vx2)
            BigInteger e = px1.multiply(vz1).subtract(pz1.multiply(vx1))
                          .subtract(px2.multiply(vz2).subtract(pz2.multiply(vx2)));

            matrixXZ[i][0] = new Fraction(a);
            matrixXZ[i][1] = new Fraction(b);
            matrixXZ[i][2] = new Fraction(c);
            matrixXZ[i][3] = new Fraction(d);
            rhsXZ[i] = new Fraction(e);
        }

        Fraction[] solutionXZ = solveSystem(matrixXZ, rhsXZ);
        // rx2 = solutionXZ[0] (should match rx)
        Fraction rz = solutionXZ[1];

        // Sum of rx, ry, rz
        Fraction sum = rx.add(ry).add(rz);
        return sum.toBigInteger().longValue();
    }

    public static void main(String[] args) throws IOException {
        String inputFile = args.length > 0 ? args[0] : "../input.txt";
        List<Hailstone> hailstones = parseInput(inputFile);

        System.out.println("Part 1: " + part1(hailstones));
        System.out.println("Part 2: " + part2(hailstones));
    }
}
