import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function* simulateCpu(instructions) {
  let x = 1;
  let cycle = 0;

  for (const line of instructions) {
    if (line === 'noop') {
      cycle++;
      yield [cycle, x];
    } else {
      const v = parseInt(line.split(' ')[1], 10);
      cycle++;
      yield [cycle, x];
      cycle++;
      yield [cycle, x];
      x += v;
    }
  }
}

function part1(instructions) {
  const targetCycles = new Set([20, 60, 100, 140, 180, 220]);
  let total = 0;

  for (const [cycle, x] of simulateCpu(instructions)) {
    if (targetCycles.has(cycle)) {
      total += cycle * x;
    }
  }

  return total;
}

function part2(instructions) {
  const screen = [];
  let row = [];

  for (const [cycle, x] of simulateCpu(instructions)) {
    const pos = (cycle - 1) % 40;
    if (Math.abs(pos - x) <= 1) {
      row.push('#');
    } else {
      row.push('.');
    }

    if (cycle % 40 === 0) {
      screen.push(row.join(''));
      row = [];
    }
  }

  return screen.join('\n');
}

const inputFile = join(__dirname, '..', 'input.txt');
const instructions = readFileSync(inputFile, 'utf-8').trim().split('\n');

console.log('Part 1:', part1(instructions));
console.log('Part 2:');
console.log(part2(instructions));
