import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  const content = readFileSync(filename, 'utf-8');
  const [stackPart, movePart] = content.split('\n\n');

  const stackLines = stackPart.split('\n');
  const numStacks = stackLines[stackLines.length - 1].trim().split(/\s+/).length;

  // Parse stacks
  const stacks = Array.from({ length: numStacks }, () => []);
  for (let lineIdx = 0; lineIdx < stackLines.length - 1; lineIdx++) {
    const line = stackLines[lineIdx];
    for (let i = 0; i < numStacks; i++) {
      const pos = 1 + i * 4;
      if (pos < line.length && line[pos] !== ' ') {
        stacks[i].push(line[pos]);
      }
    }
  }

  // Reverse so bottom is at index 0
  stacks.forEach(stack => stack.reverse());

  // Parse moves
  const moves = movePart.trim().split('\n').map(line => {
    const match = line.match(/move (\d+) from (\d+) to (\d+)/);
    return {
      count: parseInt(match[1], 10),
      from: parseInt(match[2], 10) - 1,
      to: parseInt(match[3], 10) - 1
    };
  });

  return { stacks, moves };
}

function part1(initialStacks, moves) {
  const stacks = initialStacks.map(s => [...s]);
  for (const { count, from, to } of moves) {
    for (let i = 0; i < count; i++) {
      stacks[to].push(stacks[from].pop());
    }
  }
  return stacks.map(s => s[s.length - 1] || '').join('');
}

function part2(initialStacks, moves) {
  const stacks = initialStacks.map(s => [...s]);
  for (const { count, from, to } of moves) {
    const crates = stacks[from].splice(-count);
    stacks[to].push(...crates);
  }
  return stacks.map(s => s[s.length - 1] || '').join('');
}

const inputFile = join(__dirname, '..', 'input.txt');
const { stacks, moves } = parseInput(inputFile);

console.log('Part 1:', part1(stacks, moves));
console.log('Part 2:', part2(stacks, moves));
