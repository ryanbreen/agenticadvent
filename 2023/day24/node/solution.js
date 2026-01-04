import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

function parseInput(filename) {
  const content = readFileSync(filename, 'utf-8').trim();
  return content.split('\n').map(line => {
    const [pos, vel] = line.split('@');
    const [px, py, pz] = pos.split(',').map(s => BigInt(s.trim()));
    const [vx, vy, vz] = vel.split(',').map(s => BigInt(s.trim()));
    return { px, py, pz, vx, vy, vz };
  });
}

function findIntersection2D(h1, h2) {
  // Line 1: position at time t1 is (px1 + vx1*t1, py1 + vy1*t1)
  // Line 2: position at time t2 is (px2 + vx2*t2, py2 + vy2*t2)
  // Solve: px1 + vx1*t1 = px2 + vx2*t2, py1 + vy1*t1 = py2 + vy2*t2

  const { px: px1, py: py1, vx: vx1, vy: vy1 } = h1;
  const { px: px2, py: py2, vx: vx2, vy: vy2 } = h2;

  // Using Cramer's rule with BigInt for exactness
  const det = vx1 * (-vy2) - (-vx2) * vy1;
  if (det === 0n) return null;

  const dx = px2 - px1;
  const dy = py2 - py1;

  // t1 = (dx * (-vy2) - (-vx2) * dy) / det
  // t2 = (vx1 * dy - dx * vy1) / det
  const t1Num = dx * (-vy2) - (-vx2) * dy;
  const t2Num = vx1 * dy - dx * vy1;

  // Check signs (same sign as det means positive time)
  const t1Positive = (t1Num >= 0n) === (det > 0n);
  const t2Positive = (t2Num >= 0n) === (det > 0n);

  if (!t1Positive || !t2Positive) return null;

  // Calculate intersection point using floating point for comparison
  const t1 = Number(t1Num) / Number(det);
  const x = Number(px1) + Number(vx1) * t1;
  const y = Number(py1) + Number(vy1) * t1;

  return { x, y };
}

function part1(hailstones) {
  const minCoord = 200000000000000;
  const maxCoord = 400000000000000;
  let count = 0;

  for (let i = 0; i < hailstones.length; i++) {
    for (let j = i + 1; j < hailstones.length; j++) {
      const result = findIntersection2D(hailstones[i], hailstones[j]);
      if (!result) continue;

      const { x, y } = result;
      if (x >= minCoord && x <= maxCoord && y >= minCoord && y <= maxCoord) {
        count++;
      }
    }
  }

  return count;
}

// Fraction class for exact arithmetic in part 2
class Fraction {
  constructor(num, den = 1n) {
    if (den === 0n) throw new Error('Division by zero');
    // Normalize sign
    if (den < 0n) {
      num = -num;
      den = -den;
    }
    const g = this.gcd(num < 0n ? -num : num, den);
    this.num = num / g;
    this.den = den / g;
  }

  gcd(a, b) {
    while (b !== 0n) {
      [a, b] = [b, a % b];
    }
    return a;
  }

  add(other) {
    return new Fraction(
      this.num * other.den + other.num * this.den,
      this.den * other.den
    );
  }

  sub(other) {
    return new Fraction(
      this.num * other.den - other.num * this.den,
      this.den * other.den
    );
  }

  mul(other) {
    return new Fraction(this.num * other.num, this.den * other.den);
  }

  div(other) {
    return new Fraction(this.num * other.den, this.den * other.num);
  }

  abs() {
    return new Fraction(this.num < 0n ? -this.num : this.num, this.den);
  }

  gt(other) {
    return this.num * other.den > other.num * this.den;
  }

  isZero() {
    return this.num === 0n;
  }

  toNumber() {
    return Number(this.num) / Number(this.den);
  }

  toBigInt() {
    if (this.num % this.den !== 0n) {
      throw new Error(`Not an integer: ${this.num}/${this.den}`);
    }
    return this.num / this.den;
  }
}

function solveSystem(matrix, rhs) {
  const n = matrix.length;
  // Create augmented matrix with fractions
  const aug = matrix.map((row, i) => [
    ...row.map(x => new Fraction(x)),
    new Fraction(rhs[i])
  ]);

  // Forward elimination with partial pivoting
  for (let col = 0; col < n; col++) {
    // Find pivot
    let maxRow = col;
    for (let row = col + 1; row < n; row++) {
      if (aug[row][col].abs().gt(aug[maxRow][col].abs())) {
        maxRow = row;
      }
    }
    [aug[col], aug[maxRow]] = [aug[maxRow], aug[col]];

    if (aug[col][col].isZero()) continue;

    // Eliminate column
    for (let row = col + 1; row < n; row++) {
      if (!aug[row][col].isZero()) {
        const factor = aug[row][col].div(aug[col][col]);
        for (let j = col; j <= n; j++) {
          aug[row][j] = aug[row][j].sub(factor.mul(aug[col][j]));
        }
      }
    }
  }

  // Back substitution
  const solution = new Array(n).fill(new Fraction(0n));
  for (let i = n - 1; i >= 0; i--) {
    solution[i] = aug[i][n];
    for (let j = i + 1; j < n; j++) {
      solution[i] = solution[i].sub(aug[i][j].mul(solution[j]));
    }
    solution[i] = solution[i].div(aug[i][i]);
  }

  return solution;
}

function part2(hailstones) {
  // Use first 5 hailstones to build 4 linear equations
  const h = hailstones.slice(0, 5);

  // Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
  const matrixXY = [];
  const rhsXY = [];

  for (let i = 0; i < 4; i++) {
    const { px: px1, py: py1, vx: vx1, vy: vy1 } = h[i];
    const { px: px2, py: py2, vx: vx2, vy: vy2 } = h[i + 1];

    // Coefficients for rx, ry, rvx, rvy
    const a = vy1 - vy2;
    const b = vx2 - vx1;
    const c = py2 - py1;
    const d = px1 - px2;
    const e = px1 * vy1 - py1 * vx1 - (px2 * vy2 - py2 * vx2);

    matrixXY.push([a, b, c, d]);
    rhsXY.push(e);
  }

  const [rx, ry, rvx, rvy] = solveSystem(matrixXY, rhsXY);

  // Build system for XZ plane
  const matrixXZ = [];
  const rhsXZ = [];

  for (let i = 0; i < 4; i++) {
    const { px: px1, pz: pz1, vx: vx1, vz: vz1 } = h[i];
    const { px: px2, pz: pz2, vx: vx2, vz: vz2 } = h[i + 1];

    const a = vz1 - vz2;
    const b = vx2 - vx1;
    const c = pz2 - pz1;
    const d = px1 - px2;
    const e = px1 * vz1 - pz1 * vx1 - (px2 * vz2 - pz2 * vx2);

    matrixXZ.push([a, b, c, d]);
    rhsXZ.push(e);
  }

  const [rx2, rz, rvx2, rvz] = solveSystem(matrixXZ, rhsXZ);

  return rx.toBigInt() + ry.toBigInt() + rz.toBigInt();
}

const inputFile = process.argv[2] || join(__dirname, '../input.txt');
const hailstones = parseInput(inputFile);

console.log('Part 1:', part1(hailstones));
console.log('Part 2:', String(part2(hailstones)));
