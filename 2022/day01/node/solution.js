import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  const content = readFileSync(filename, 'utf-8').trim();

  return content.split('\n\n').map(group => {
    return group.split('\n')
      .filter(line => line)
      .reduce((sum, line) => sum + parseInt(line, 10), 0);
  });
}

function part1(elves) {
  return Math.max(...elves);
}

function part2(elves) {
  const sorted = [...elves].sort((a, b) => b - a);
  return sorted[0] + sorted[1] + sorted[2];
}

const inputFile = join(__dirname, '..', 'input.txt');
const elves = parseInput(inputFile);

console.log('Part 1:', part1(elves));
console.log('Part 2:', part2(elves));
