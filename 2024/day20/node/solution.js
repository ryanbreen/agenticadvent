import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

const lines = input.split('\n');
const grid = lines.map(line => line.split(''));
const rows = grid.length;
const cols = grid[0].length;

let start, end;
for (let r = 0; r < rows; r++) {
  for (let c = 0; c < cols; c++) {
    if (grid[r][c] === 'S') start = [r, c];
    if (grid[r][c] === 'E') end = [r, c];
  }
}

function tracePath() {
  const dist = new Map();
  dist.set(`${start[0]},${start[1]}`, 0);
  const queue = [start];
  const dirs = [[-1, 0], [1, 0], [0, -1], [0, 1]];

  while (queue.length > 0) {
    const [r, c] = queue.shift();
    if (r === end[0] && c === end[1]) break;

    const currDist = dist.get(`${r},${c}`);
    for (const [dr, dc] of dirs) {
      const nr = r + dr, nc = c + dc;
      const key = `${nr},${nc}`;
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols &&
          grid[nr][nc] !== '#' && !dist.has(key)) {
        dist.set(key, currDist + 1);
        queue.push([nr, nc]);
      }
    }
  }
  return dist;
}

function countCheats(dist, maxCheatTime, minSavings) {
  let count = 0;
  const positions = [];
  for (const [key, d] of dist) {
    const [r, c] = key.split(',').map(Number);
    positions.push([r, c, d]);
  }

  for (const [r1, c1, d1] of positions) {
    for (const [r2, c2, d2] of positions) {
      const cheatCost = Math.abs(r2 - r1) + Math.abs(c2 - c1);
      if (cheatCost <= maxCheatTime) {
        const savings = d2 - d1 - cheatCost;
        if (savings >= minSavings) {
          count++;
        }
      }
    }
  }
  return count;
}

function part1() {
  const dist = tracePath();
  return countCheats(dist, 2, 100);
}

function part2() {
  const dist = tracePath();
  return countCheats(dist, 20, 100);
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
