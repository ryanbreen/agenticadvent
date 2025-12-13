import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

function parseMachines(text) {
  const machines = [];
  const blocks = text.split('\n\n');

  for (const block of blocks) {
    const lines = block.trim().split('\n');
    // Button A: X+ax, Y+ay
    const aMatch = lines[0].match(/Button A: X\+(\d+), Y\+(\d+)/);
    const ax = parseInt(aMatch[1]), ay = parseInt(aMatch[2]);
    // Button B: X+bx, Y+by
    const bMatch = lines[1].match(/Button B: X\+(\d+), Y\+(\d+)/);
    const bx = parseInt(bMatch[1]), by = parseInt(bMatch[2]);
    // Prize: X=px, Y=py
    const pMatch = lines[2].match(/Prize: X=(\d+), Y=(\d+)/);
    const px = parseInt(pMatch[1]), py = parseInt(pMatch[2]);

    machines.push({ ax, ay, bx, by, px, py });
  }

  return machines;
}

function solveMachine(ax, ay, bx, by, px, py, maxPresses = null) {
  // Solve using Cramer's rule
  // a*ax + b*bx = px
  // a*ay + b*by = py
  //
  // det = ax*by - ay*bx
  // a = (px*by - py*bx) / det
  // b = (ax*py - ay*px) / det

  // Use BigInt for Part 2's large numbers
  const axB = BigInt(ax), ayB = BigInt(ay);
  const bxB = BigInt(bx), byB = BigInt(by);
  const pxB = BigInt(px), pyB = BigInt(py);

  const det = axB * byB - ayB * bxB;

  if (det === 0n) {
    return null; // No unique solution
  }

  const aNum = pxB * byB - pyB * bxB;
  const bNum = axB * pyB - ayB * pxB;

  // Check if solutions are integers
  if (aNum % det !== 0n || bNum % det !== 0n) {
    return null;
  }

  const a = aNum / det;
  const b = bNum / det;

  // Check non-negative
  if (a < 0n || b < 0n) {
    return null;
  }

  // Check max presses constraint (Part 1)
  if (maxPresses !== null && (a > BigInt(maxPresses) || b > BigInt(maxPresses))) {
    return null;
  }

  return 3n * a + b;
}

function part1() {
  const machines = parseMachines(input);
  let total = 0n;

  for (const { ax, ay, bx, by, px, py } of machines) {
    const cost = solveMachine(ax, ay, bx, by, px, py, 100);
    if (cost !== null) {
      total += cost;
    }
  }

  return total;
}

function part2() {
  const machines = parseMachines(input);
  const offset = 10_000_000_000_000;
  let total = 0n;

  for (const { ax, ay, bx, by, px, py } of machines) {
    const cost = solveMachine(ax, ay, bx, by, px + offset, py + offset, null);
    if (cost !== null) {
      total += cost;
    }
  }

  return total;
}

console.log('Part 1:', part1().toString());
console.log('Part 2:', part2().toString());
