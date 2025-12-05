import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input - split into rules and updates sections
const [rulesSection, updatesSection] = input.split('\n\n');

// Parse rules: X|Y means X must come before Y
// Store as: rules[X] = Set of pages that must come AFTER X
const rules = new Map();
for (const line of rulesSection.split('\n')) {
  const [before, after] = line.split('|').map(Number);
  if (!rules.has(before)) rules.set(before, new Set());
  rules.get(before).add(after);
}

// Parse updates
const updates = updatesSection.split('\n').map(line => line.split(',').map(Number));

function isValidOrder(update) {
  const pagePositions = new Map(update.map((page, i) => [page, i]));

  for (let i = 0; i < update.length; i++) {
    const page = update[i];
    const mustBeAfter = rules.get(page);
    if (mustBeAfter) {
      for (const afterPage of mustBeAfter) {
        if (pagePositions.has(afterPage) && pagePositions.get(afterPage) < i) {
          return false;
        }
      }
    }
  }
  return true;
}

// Part 1
function part1() {
  let total = 0;
  for (const update of updates) {
    if (isValidOrder(update)) {
      const middleIdx = Math.floor(update.length / 2);
      total += update[middleIdx];
    }
  }
  return total;
}

function fixOrder(update) {
  // Sort using a custom comparator based on the rules
  return [...update].sort((a, b) => {
    // If a must come before b, return -1
    if (rules.has(a) && rules.get(a).has(b)) return -1;
    // If b must come before a, return 1
    if (rules.has(b) && rules.get(b).has(a)) return 1;
    return 0;
  });
}

// Part 2
function part2() {
  let total = 0;
  for (const update of updates) {
    if (!isValidOrder(update)) {
      const fixed = fixOrder(update);
      const middleIdx = Math.floor(fixed.length / 2);
      total += fixed[middleIdx];
    }
  }
  return total;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
