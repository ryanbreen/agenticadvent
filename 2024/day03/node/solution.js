import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8');

function part1(data) {
  const pattern = /mul\((\d{1,3}),(\d{1,3})\)/g;
  let total = 0;
  let match;
  while ((match = pattern.exec(data)) !== null) {
    total += parseInt(match[1]) * parseInt(match[2]);
  }
  return total;
}

function part2(data) {
  // Find all events: mul, do(), don't()
  const mulPattern = /mul\((\d{1,3}),(\d{1,3})\)/g;
  const doPattern = /do\(\)/g;
  const dontPattern = /don't\(\)/g;

  const events = [];

  let match;
  while ((match = mulPattern.exec(data)) !== null) {
    events.push({ pos: match.index, type: 'mul', x: parseInt(match[1]), y: parseInt(match[2]) });
  }
  while ((match = doPattern.exec(data)) !== null) {
    events.push({ pos: match.index, type: 'do' });
  }
  while ((match = dontPattern.exec(data)) !== null) {
    events.push({ pos: match.index, type: 'dont' });
  }

  // Sort by position
  events.sort((a, b) => a.pos - b.pos);

  let total = 0;
  let enabled = true;

  for (const event of events) {
    if (event.type === 'do') {
      enabled = true;
    } else if (event.type === 'dont') {
      enabled = false;
    } else if (event.type === 'mul' && enabled) {
      total += event.x * event.y;
    }
  }

  return total;
}

console.log('Part 1:', part1(input));
console.log('Part 2:', part2(input));
