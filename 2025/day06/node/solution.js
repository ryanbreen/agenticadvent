import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

function parseProblems(lines) {
  if (lines.length === 0) return [];

  // Find the operator row (last non-empty row with only +, *, and spaces)
  let opRowIdx = lines.length - 1;
  while (opRowIdx >= 0 && (!lines[opRowIdx].trim() ||
         ![...lines[opRowIdx]].every(c => c === '+' || c === '*' || c === ' '))) {
    opRowIdx--;
  }

  if (opRowIdx < 0) return [];

  const opRow = lines[opRowIdx];
  const numberRows = lines.slice(0, opRowIdx);

  // Find max width
  const maxWidth = Math.max(...lines.map(l => l.length));

  // Pad all rows to the same width
  const paddedNumberRows = numberRows.map(row => row.padEnd(maxWidth));
  const paddedOpRow = opRow.padEnd(maxWidth);

  // Find problem boundaries by looking for columns that are all spaces
  const problems = [];
  let col = 0;

  while (col < maxWidth) {
    // Skip separator columns (all spaces)
    while (col < maxWidth && paddedNumberRows.every(row => row[col] === ' ') && paddedOpRow[col] === ' ') {
      col++;
    }

    if (col >= maxWidth) break;

    // Find the end of this problem
    const startCol = col;
    while (col < maxWidth) {
      // Check if this is a separator column
      const isSeparator = paddedNumberRows.every(row => row[col] === ' ') && paddedOpRow[col] === ' ';
      if (isSeparator) break;
      col++;
    }

    const endCol = col;

    // Extract numbers and operator for this problem
    const numbers = [];
    for (const row of paddedNumberRows) {
      const numStr = row.slice(startCol, endCol).trim();
      if (numStr) {
        numbers.push(BigInt(numStr));
      }
    }

    const opStr = paddedOpRow.slice(startCol, endCol).trim();
    if (opStr && numbers.length > 0) {
      problems.push({ numbers, op: opStr });
    }
  }

  return problems;
}

function solveProblem(numbers, op) {
  if (op === '+') {
    return numbers.reduce((a, b) => a + b, 0n);
  } else if (op === '*') {
    return numbers.reduce((a, b) => a * b, 1n);
  }
  return 0n;
}

// Part 1
function part1() {
  const problems = parseProblems(lines);
  let total = 0n;
  for (const { numbers, op } of problems) {
    total += solveProblem(numbers, op);
  }
  return total.toString();
}

function parseProblemsPart2(lines) {
  if (lines.length === 0) return [];

  // Find the operator row (last non-empty row with only +, *, and spaces)
  let opRowIdx = lines.length - 1;
  while (opRowIdx >= 0 && (!lines[opRowIdx].trim() ||
         ![...lines[opRowIdx]].every(c => c === '+' || c === '*' || c === ' '))) {
    opRowIdx--;
  }

  if (opRowIdx < 0) return [];

  const opRow = lines[opRowIdx];
  const numberRows = lines.slice(0, opRowIdx);

  // Find max width
  const maxWidth = Math.max(...lines.map(l => l.length));

  // Pad all rows to the same width
  const paddedNumberRows = numberRows.map(row => row.padEnd(maxWidth));
  const paddedOpRow = opRow.padEnd(maxWidth);

  // Find problem boundaries by looking for columns that are all spaces
  const problems = [];
  let col = 0;

  while (col < maxWidth) {
    // Skip separator columns (all spaces)
    while (col < maxWidth && paddedNumberRows.every(row => row[col] === ' ') && paddedOpRow[col] === ' ') {
      col++;
    }

    if (col >= maxWidth) break;

    // Find the end of this problem
    const startCol = col;
    while (col < maxWidth) {
      // Check if this is a separator column
      const isSeparator = paddedNumberRows.every(row => row[col] === ' ') && paddedOpRow[col] === ' ';
      if (isSeparator) break;
      col++;
    }

    const endCol = col;

    // For Part 2: Read columns right-to-left, each column forms a number
    // reading top-to-bottom as most-to-least significant digit
    const numbers = [];
    for (let c = endCol - 1; c >= startCol; c--) {
      const digits = [];
      for (const row of paddedNumberRows) {
        const ch = row[c];
        if (ch >= '0' && ch <= '9') {
          digits.push(ch);
        }
      }
      if (digits.length > 0) {
        const num = BigInt(digits.join(''));
        numbers.push(num);
      }
    }

    const opStr = paddedOpRow.slice(startCol, endCol).trim();
    if (opStr && numbers.length > 0) {
      problems.push({ numbers, op: opStr });
    }
  }

  return problems;
}

// Part 2
function part2() {
  const problems = parseProblemsPart2(lines);
  let total = 0n;
  for (const { numbers, op } of problems) {
    total += solveProblem(numbers, op);
  }
  return total.toString();
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
