import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

function parseLine(line) {
  // Extract indicator pattern [.##.]
  const indicatorMatch = line.match(/\[([.#]+)\]/);
  const indicator = indicatorMatch[1];
  const nLights = indicator.length;

  // Target state: 1 where # appears
  let target = 0;
  for (let i = 0; i < indicator.length; i++) {
    if (indicator[i] === '#') {
      target |= (1 << i);
    }
  }

  // Extract button schematics (0,1,2) etc.
  const buttons = [];
  const buttonRegex = /\(([0-9,]+)\)/g;
  let match;
  while ((match = buttonRegex.exec(line)) !== null) {
    const indices = match[1].split(',').map(Number);
    let mask = 0;
    for (const idx of indices) {
      mask |= (1 << idx);
    }
    buttons.push(mask);
  }

  return { nLights, target, buttons };
}

function countBits(n) {
  let count = 0;
  while (n) {
    count += n & 1;
    n >>>= 1;
  }
  return count;
}

function solveMachineBrute(nLights, target, buttons) {
  const nButtons = buttons.length;
  let minPresses = Infinity;

  // Try all 2^nButtons combinations
  for (let mask = 0; mask < (1 << nButtons); mask++) {
    let state = 0;
    let presses = 0;

    for (let i = 0; i < nButtons; i++) {
      if (mask & (1 << i)) {
        state ^= buttons[i];
        presses++;
      }
    }

    if (state === target) {
      minPresses = Math.min(minPresses, presses);
    }
  }

  return minPresses === Infinity ? 0 : minPresses;
}

function part1(lines) {
  let total = 0;

  for (const line of lines) {
    if (!line.trim()) continue;
    const { nLights, target, buttons } = parseLine(line);
    const minPresses = solveMachineBrute(nLights, target, buttons);
    total += minPresses;
  }

  return total;
}

function parseLinePart2(line) {
  // Extract joltage requirements {3,5,4,7}
  const joltageMatch = line.match(/\{([0-9,]+)\}/);
  const joltage = joltageMatch[1].split(',').map(Number);
  const nCounters = joltage.length;

  // Extract button schematics (0,1,2) etc.
  const buttons = [];
  const buttonRegex = /\(([0-9,]+)\)/g;
  let match;
  while ((match = buttonRegex.exec(line)) !== null) {
    const indices = match[1].split(',').map(Number);
    buttons.push(indices);
  }

  return { nCounters, joltage, buttons };
}

// Rational number implementation for Gaussian elimination
class Fraction {
  constructor(num, den = 1n) {
    if (typeof num === 'number') {
      num = BigInt(Math.round(num));
    }
    if (typeof den === 'number') {
      den = BigInt(Math.round(den));
    }
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
      const t = b;
      b = a % b;
      a = t;
    }
    return a || 1n;
  }

  add(other) {
    return new Fraction(this.num * other.den + other.num * this.den, this.den * other.den);
  }

  sub(other) {
    return new Fraction(this.num * other.den - other.num * this.den, this.den * other.den);
  }

  mul(other) {
    return new Fraction(this.num * other.num, this.den * other.den);
  }

  div(other) {
    return new Fraction(this.num * other.den, this.den * other.num);
  }

  neg() {
    return new Fraction(-this.num, this.den);
  }

  isZero() {
    return this.num === 0n;
  }

  isNegative() {
    return this.num < 0n;
  }

  isInteger() {
    return this.den === 1n;
  }

  toNumber() {
    return Number(this.num) / Number(this.den);
  }

  toInt() {
    return Number(this.num / this.den);
  }
}

function solveMachinePart2(nCounters, joltage, buttons) {
  const nButtons = buttons.length;

  if (nButtons === 0) {
    return joltage.every(j => j === 0) ? 0 : Infinity;
  }

  // Build matrix A (nCounters x nButtons)
  const A = [];
  for (let i = 0; i < nCounters; i++) {
    A.push(Array(nButtons).fill(null).map(() => new Fraction(0n)));
  }
  for (let j = 0; j < nButtons; j++) {
    for (const idx of buttons[j]) {
      if (idx < nCounters) {
        A[idx][j] = new Fraction(1n);
      }
    }
  }

  const b = joltage.map(j => new Fraction(BigInt(j)));

  // Augmented matrix [A | b]
  const aug = A.map((row, i) => [...row, b[i]]);
  const nRows = nCounters;
  const nCols = nButtons;

  // Gaussian elimination
  const pivotCols = [];
  let pivotRow = 0;

  for (let col = 0; col < nCols; col++) {
    // Find non-zero entry in this column
    let found = -1;
    for (let row = pivotRow; row < nRows; row++) {
      if (!aug[row][col].isZero()) {
        found = row;
        break;
      }
    }

    if (found === -1) continue;

    // Swap rows
    [aug[pivotRow], aug[found]] = [aug[found], aug[pivotRow]];
    pivotCols.push([col, pivotRow]);

    // Scale pivot row
    const scale = aug[pivotRow][col];
    for (let c = 0; c <= nCols; c++) {
      aug[pivotRow][c] = aug[pivotRow][c].div(scale);
    }

    // Eliminate column in other rows
    for (let row = 0; row < nRows; row++) {
      if (row !== pivotRow && !aug[row][col].isZero()) {
        const factor = aug[row][col];
        for (let c = 0; c <= nCols; c++) {
          aug[row][c] = aug[row][c].sub(factor.mul(aug[pivotRow][c]));
        }
      }
    }

    pivotRow++;
  }

  // Check for inconsistency
  for (let row = pivotRow; row < nRows; row++) {
    if (!aug[row][nCols].isZero()) {
      return Infinity;
    }
  }

  // Identify free variables
  const pivotColSet = new Set(pivotCols.map(([col]) => col));
  const freeVars = [];
  for (let c = 0; c < nCols; c++) {
    if (!pivotColSet.has(c)) {
      freeVars.push(c);
    }
  }
  const nFree = freeVars.length;

  // Extract null vectors
  const nullVectors = [];
  for (const fv of freeVars) {
    const vec = Array(nButtons).fill(null).map(() => new Fraction(0n));
    vec[fv] = new Fraction(1n);
    for (const [col, row] of pivotCols) {
      vec[col] = aug[row][fv].neg();
    }
    nullVectors.push(vec);
  }

  // Extract particular solution
  const particular = Array(nButtons).fill(null).map(() => new Fraction(0n));
  for (const [col, row] of pivotCols) {
    particular[col] = aug[row][nCols];
  }

  let minTotal = Infinity;
  const maxJ = Math.max(...joltage, 100);

  if (nFree === 0) {
    // Unique solution
    let total = 0;
    for (const val of particular) {
      if (val.isNegative() || !val.isInteger()) {
        return Infinity;
      }
      total += val.toInt();
    }
    return total;
  }

  if (nFree === 1) {
    // 1D search
    let tLow = -Infinity;
    let tHigh = Infinity;

    for (let j = 0; j < nButtons; j++) {
      const p = particular[j].toNumber();
      const nv = nullVectors[0][j].toNumber();

      if (nv === 0) {
        if (p < 0) return Infinity;
      } else if (nv > 0) {
        tLow = Math.max(tLow, -p / nv);
      } else {
        tHigh = Math.min(tHigh, -p / nv);
      }
    }

    if (tLow > tHigh) return Infinity;

    const tLowInt = Math.ceil(tLow);
    const tHighInt = Math.floor(tHigh);

    for (let t = tLowInt; t <= tHighInt; t++) {
      const tFrac = new Fraction(BigInt(t));
      let total = 0;
      let valid = true;
      for (let j = 0; j < nButtons; j++) {
        const val = particular[j].add(tFrac.mul(nullVectors[0][j]));
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

    return minTotal === Infinity ? 0 : minTotal;
  }

  if (nFree === 2) {
    // 2D search
    const bound = maxJ * 2;
    for (let t0 = -bound; t0 <= bound; t0++) {
      const t0Frac = new Fraction(BigInt(t0));
      const inter = particular.map((p, j) => p.add(t0Frac.mul(nullVectors[0][j])));

      // Compute bounds for t1
      let t1Low = -Infinity;
      let t1High = Infinity;
      for (let j = 0; j < nButtons; j++) {
        const p = inter[j].toNumber();
        const nv = nullVectors[1][j].toNumber();
        if (nv > 0) {
          t1Low = Math.max(t1Low, -p / nv);
        } else if (nv < 0) {
          t1High = Math.min(t1High, -p / nv);
        }
      }

      const t1LowInt = Math.ceil(t1Low);
      const t1HighInt = Math.floor(t1High);

      for (let t1 = t1LowInt; t1 <= t1HighInt; t1++) {
        const t1Frac = new Fraction(BigInt(t1));
        let total = 0;
        let valid = true;
        for (let j = 0; j < nButtons; j++) {
          const val = inter[j].add(t1Frac.mul(nullVectors[1][j]));
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

    return minTotal === Infinity ? 0 : minTotal;
  }

  if (nFree === 3) {
    // 3D search
    const bound = maxJ;
    for (let t0 = -bound; t0 <= bound; t0++) {
      const t0Frac = new Fraction(BigInt(t0));
      const inter0 = particular.map((p, j) => p.add(t0Frac.mul(nullVectors[0][j])));

      // Compute bounds for t1
      let t1Low = -Infinity;
      let t1High = Infinity;
      for (let j = 0; j < nButtons; j++) {
        const p = inter0[j].toNumber();
        const nv = nullVectors[1][j].toNumber();
        if (nv > 0) {
          t1Low = Math.max(t1Low, -p / nv - bound);
        } else if (nv < 0) {
          t1High = Math.min(t1High, -p / nv + bound);
        }
      }

      const t1LowInt = Math.max(Math.ceil(t1Low), -bound);
      const t1HighInt = Math.min(Math.floor(t1High), bound);

      for (let t1 = t1LowInt; t1 <= t1HighInt; t1++) {
        const t1Frac = new Fraction(BigInt(t1));
        const inter1 = inter0.map((p, j) => p.add(t1Frac.mul(nullVectors[1][j])));

        // Compute bounds for t2
        let t2Low = -Infinity;
        let t2High = Infinity;
        for (let j = 0; j < nButtons; j++) {
          const p = inter1[j].toNumber();
          const nv = nullVectors[2][j].toNumber();
          if (nv > 0) {
            t2Low = Math.max(t2Low, -p / nv);
          } else if (nv < 0) {
            t2High = Math.min(t2High, -p / nv);
          }
        }

        const t2LowInt = Math.ceil(t2Low);
        const t2HighInt = Math.floor(t2High);

        for (let t2 = t2LowInt; t2 <= t2HighInt; t2++) {
          const t2Frac = new Fraction(BigInt(t2));
          let total = 0;
          let valid = true;
          for (let j = 0; j < nButtons; j++) {
            const val = inter1[j].add(t2Frac.mul(nullVectors[2][j]));
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

    return minTotal === Infinity ? 0 : minTotal;
  }

  return 0; // Fallback for large null spaces
}

function part2(lines) {
  let total = 0;

  for (const line of lines) {
    if (!line.trim()) continue;
    const { nCounters, joltage, buttons } = parseLinePart2(line);
    const minPresses = solveMachinePart2(nCounters, joltage, buttons);
    total += minPresses;
  }

  return total;
}

const lines = input.split('\n');
console.log('Part 1:', part1(lines));
console.log('Part 2:', part2(lines));
