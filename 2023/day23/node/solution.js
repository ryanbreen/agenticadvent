#!/usr/bin/env node
/**
 * Day 23: A Long Walk - Longest path through hiking trails.
 */

import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  return readFileSync(filename, 'utf-8').trim().split('\n');
}

function findJunctions(grid) {
  const rows = grid.length;
  const cols = grid[0].length;
  const junctions = new Set();

  // Start and end points
  const startCol = grid[0].indexOf('.');
  const endCol = grid[rows - 1].indexOf('.');
  junctions.add(`0,${startCol}`);
  junctions.add(`${rows - 1},${endCol}`);

  // Find intersections (cells with 3+ walkable neighbors)
  const dirs = [[-1, 0], [1, 0], [0, -1], [0, 1]];
  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      if (grid[r][c] === '#') continue;
      let neighbors = 0;
      for (const [dr, dc] of dirs) {
        const nr = r + dr, nc = c + dc;
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] !== '#') {
          neighbors++;
        }
      }
      if (neighbors >= 3) {
        junctions.add(`${r},${c}`);
      }
    }
  }

  return junctions;
}

function buildGraph(grid, junctions, respectSlopes) {
  const rows = grid.length;
  const cols = grid[0].length;

  const slopeDirs = {
    '^': [-1, 0],
    'v': [1, 0],
    '<': [0, -1],
    '>': [0, 1]
  };

  const graph = new Map();
  for (const junction of junctions) {
    graph.set(junction, new Map());
  }

  const dirs = [[-1, 0], [1, 0], [0, -1], [0, 1]];

  for (const startJunction of junctions) {
    const [startR, startC] = startJunction.split(',').map(Number);
    const stack = [[startR, startC, 0]];
    const visited = new Set([startJunction]);

    while (stack.length > 0) {
      const [r, c, dist] = stack.pop();
      const key = `${r},${c}`;

      if (dist > 0 && junctions.has(key)) {
        graph.get(startJunction).set(key, dist);
        continue;
      }

      for (const [dr, dc] of dirs) {
        const nr = r + dr, nc = c + dc;
        if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) continue;
        if (grid[nr][nc] === '#') continue;

        const nkey = `${nr},${nc}`;
        if (visited.has(nkey)) continue;

        // Check slope constraints for Part 1
        if (respectSlopes) {
          const cell = grid[r][c];
          if (cell in slopeDirs) {
            const [reqDr, reqDc] = slopeDirs[cell];
            if (dr !== reqDr || dc !== reqDc) continue;
          }
        }

        visited.add(nkey);
        stack.push([nr, nc, dist + 1]);
      }
    }
  }

  return graph;
}

function longestPathDFS(graph, start, end) {
  const visited = new Set();

  function dfs(node) {
    if (node === end) return 0;

    visited.add(node);
    let maxDist = -Infinity;

    for (const [neighbor, dist] of graph.get(node).entries()) {
      if (!visited.has(neighbor)) {
        const result = dfs(neighbor);
        if (result !== -Infinity) {
          maxDist = Math.max(maxDist, dist + result);
        }
      }
    }

    visited.delete(node);
    return maxDist;
  }

  return dfs(start);
}

function solve(grid, respectSlopes) {
  const rows = grid.length;
  const startCol = grid[0].indexOf('.');
  const endCol = grid[rows - 1].indexOf('.');
  const start = `0,${startCol}`;
  const end = `${rows - 1},${endCol}`;

  const junctions = findJunctions(grid);
  const graph = buildGraph(grid, junctions, respectSlopes);

  return longestPathDFS(graph, start, end);
}

function part1(grid) {
  return solve(grid, true);
}

function part2(grid) {
  return solve(grid, false);
}

const inputPath = join(__dirname, '..', 'input.txt');
const grid = parseInput(inputPath);
console.log(`Part 1: ${part1(grid)}`);
console.log(`Part 2: ${part2(grid)}`);
