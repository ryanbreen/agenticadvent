import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input into grid
const lines = input.split('\n');
const grid = lines.map(line => line.split('').map(Number));
const rows = grid.length;
const cols = grid[0].length;

// Directions: up, down, left, right
const DIRS = [[-1, 0], [1, 0], [0, -1], [0, 1]];

function findTrailheads() {
  const trailheads = [];
  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      if (grid[r][c] === 0) {
        trailheads.push([r, c]);
      }
    }
  }
  return trailheads;
}

function countReachableNines(startR, startC) {
  const visited = new Set();
  visited.add(`${startR},${startC}`);
  const queue = [[startR, startC]];
  const nines = new Set();

  while (queue.length > 0) {
    const [r, c] = queue.shift();
    const currentHeight = grid[r][c];

    if (currentHeight === 9) {
      nines.add(`${r},${c}`);
      continue;
    }

    for (const [dr, dc] of DIRS) {
      const nr = r + dr;
      const nc = c + dc;
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
        const key = `${nr},${nc}`;
        if (!visited.has(key) && grid[nr][nc] === currentHeight + 1) {
          visited.add(key);
          queue.push([nr, nc]);
        }
      }
    }
  }

  return nines.size;
}

// Part 1
function part1() {
  const trailheads = findTrailheads();
  let totalScore = 0;
  for (const [r, c] of trailheads) {
    totalScore += countReachableNines(r, c);
  }
  return totalScore;
}

function countDistinctTrails(startR, startC) {
  function dfs(r, c) {
    const currentHeight = grid[r][c];
    if (currentHeight === 9) {
      return 1;
    }

    let total = 0;
    for (const [dr, dc] of DIRS) {
      const nr = r + dr;
      const nc = c + dc;
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
        if (grid[nr][nc] === currentHeight + 1) {
          total += dfs(nr, nc);
        }
      }
    }
    return total;
  }

  return dfs(startR, startC);
}

// Part 2
function part2() {
  const trailheads = findTrailheads();
  let totalRating = 0;
  for (const [r, c] of trailheads) {
    totalRating += countDistinctTrails(r, c);
  }
  return totalRating;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
