import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

// Part 1
function part1() {
  // TODO: Implement
  return null;
}

// Part 2
function part2() {
  // TODO: Implement
  return null;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
