import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

function parseCards() {
  return lines.map(line => {
    const [_, numbers] = line.split(':');
    const [winningPart, havePart] = numbers.split('|');
    const winning = new Set(winningPart.trim().split(/\s+/).map(Number));
    const have = havePart.trim().split(/\s+/).map(Number);
    return { winning, have };
  });
}

// Part 1
function part1() {
  const cards = parseCards();
  let total = 0;
  for (const { winning, have } of cards) {
    const matches = have.filter(n => winning.has(n)).length;
    if (matches > 0) {
      total += Math.pow(2, matches - 1);
    }
  }
  return total;
}

// Part 2
function part2() {
  const cards = parseCards();
  const matches = cards.map(({ winning, have }) =>
    have.filter(n => winning.has(n)).length
  );
  const copies = new Array(cards.length).fill(1);

  for (let i = 0; i < cards.length; i++) {
    for (let j = i + 1; j < Math.min(i + 1 + matches[i], cards.length); j++) {
      copies[j] += copies[i];
    }
  }

  return copies.reduce((a, b) => a + b, 0);
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
