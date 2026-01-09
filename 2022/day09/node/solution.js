import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

const DIRECTIONS = {
  'U': [0, 1],
  'D': [0, -1],
  'L': [-1, 0],
  'R': [1, 0],
};

function sign(x) {
  return x === 0 ? 0 : (x > 0 ? 1 : -1);
}

function moveTail(head, tail) {
  const dx = head[0] - tail[0];
  const dy = head[1] - tail[1];

  // If adjacent or overlapping, don't move
  if (Math.abs(dx) <= 1 && Math.abs(dy) <= 1) {
    return tail;
  }

  // Move toward head
  return [tail[0] + sign(dx), tail[1] + sign(dy)];
}

function simulateRope(moves, ropeLength) {
  const knots = Array.from({ length: ropeLength }, () => [0, 0]);
  const visited = new Set();
  visited.add('0,0');

  for (const line of moves) {
    const [direction, countStr] = line.split(' ');
    const count = parseInt(countStr, 10);
    const [dx, dy] = DIRECTIONS[direction];

    for (let step = 0; step < count; step++) {
      // Move head
      knots[0] = [knots[0][0] + dx, knots[0][1] + dy];

      // Move each subsequent knot
      for (let i = 1; i < ropeLength; i++) {
        knots[i] = moveTail(knots[i - 1], knots[i]);
      }

      visited.add(`${knots[ropeLength - 1][0]},${knots[ropeLength - 1][1]}`);
    }
  }

  return visited.size;
}

function part1(moves) {
  return simulateRope(moves, 2);
}

function part2(moves) {
  return simulateRope(moves, 10);
}

const inputFile = join(__dirname, '..', 'input.txt');
const moves = readFileSync(inputFile, 'utf-8').trim().split('\n');

console.log('Part 1:', part1(moves));
console.log('Part 2:', part2(moves));
