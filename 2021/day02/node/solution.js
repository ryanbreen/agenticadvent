import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf8').trim();

const commands = input.split('\n').map(line => {
  const [cmd, val] = line.split(' ');
  return [cmd, parseInt(val)];
});

function part1() {
  let horizontal = 0;
  let depth = 0;
  for (const [cmd, val] of commands) {
    if (cmd === 'forward') horizontal += val;
    else if (cmd === 'down') depth += val;
    else if (cmd === 'up') depth -= val;
  }
  return horizontal * depth;
}

function part2() {
  let horizontal = 0;
  let depth = 0;
  let aim = 0;
  for (const [cmd, val] of commands) {
    if (cmd === 'forward') {
      horizontal += val;
      depth += aim * val;
    } else if (cmd === 'down') {
      aim += val;
    } else if (cmd === 'up') {
      aim -= val;
    }
  }
  return horizontal * depth;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
