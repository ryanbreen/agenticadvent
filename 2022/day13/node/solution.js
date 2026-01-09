import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function compare(left, right) {
  // Both integers
  if (typeof left === 'number' && typeof right === 'number') {
    if (left < right) return -1;
    if (left > right) return 1;
    return 0;
  }

  // Both arrays
  if (Array.isArray(left) && Array.isArray(right)) {
    const minLen = Math.min(left.length, right.length);
    for (let i = 0; i < minLen; i++) {
      const result = compare(left[i], right[i]);
      if (result !== 0) return result;
    }
    if (left.length < right.length) return -1;
    if (left.length > right.length) return 1;
    return 0;
  }

  // Mixed types - convert integer to array
  if (typeof left === 'number') {
    return compare([left], right);
  } else {
    return compare(left, [right]);
  }
}

function part1(text) {
  const pairs = text.trim().split('\n\n');
  let total = 0;

  for (let i = 0; i < pairs.length; i++) {
    const lines = pairs[i].trim().split('\n');
    const left = JSON.parse(lines[0]);
    const right = JSON.parse(lines[1]);

    if (compare(left, right) === -1) {
      total += i + 1;
    }
  }

  return total;
}

function part2(text) {
  const lines = text.trim().split('\n').filter(line => line);
  const packets = lines.map(line => JSON.parse(line));

  // Add divider packets
  const divider1 = [[2]];
  const divider2 = [[6]];
  packets.push(divider1);
  packets.push(divider2);

  // Sort using comparison function
  packets.sort(compare);

  // Find positions of dividers (1-indexed)
  let pos1 = 0, pos2 = 0;
  for (let i = 0; i < packets.length; i++) {
    if (JSON.stringify(packets[i]) === JSON.stringify(divider1)) pos1 = i + 1;
    if (JSON.stringify(packets[i]) === JSON.stringify(divider2)) pos2 = i + 1;
  }

  return pos1 * pos2;
}

const inputFile = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputFile, 'utf-8');

console.log('Part 1:', part1(text));
console.log('Part 2:', part2(text));
