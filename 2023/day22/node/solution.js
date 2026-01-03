#!/usr/bin/env node
/**
 * Day 22: Sand Slabs - 3D falling bricks simulation.
 */

import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  const bricks = [];
  for (const line of readFileSync(filename, 'utf-8').trim().split('\n')) {
    const [left, right] = line.split('~');
    let [x1, y1, z1] = left.split(',').map(Number);
    let [x2, y2, z2] = right.split(',').map(Number);
    // Ensure z1 <= z2
    if (z1 > z2) {
      [x1, y1, z1, x2, y2, z2] = [x2, y2, z2, x1, y1, z1];
    }
    bricks.push([x1, y1, z1, x2, y2, z2]);
  }
  return bricks;
}

function settleBricks(bricks) {
  // Sort by minimum z coordinate
  const sortedBricks = bricks.map((b, i) => [i, b]).sort((a, b) => Math.min(a[1][2], a[1][5]) - Math.min(b[1][2], b[1][5]));

  // Track occupied cells: "x,y,z" -> brick index
  const occupied = new Map();
  const settled = new Array(bricks.length);

  // supports[i] = set of brick indices that brick i supports
  // supporters[i] = set of brick indices that support brick i
  const supports = new Map();
  const supporters = new Map();
  for (let i = 0; i < bricks.length; i++) {
    supports.set(i, new Set());
    supporters.set(i, new Set());
  }

  for (const [origIdx, brick] of sortedBricks) {
    const [x1, y1, z1, x2, y2, z2] = brick;

    // Find how far this brick can drop
    let drop = z1 - 1; // Maximum drop to z=1

    const minX = Math.min(x1, x2);
    const maxX = Math.max(x1, x2);
    const minY = Math.min(y1, y2);
    const maxY = Math.max(y1, y2);

    for (let x = minX; x <= maxX; x++) {
      for (let y = minY; y <= maxY; y++) {
        for (let z = z1 - 1; z >= 1; z--) {
          if (occupied.has(`${x},${y},${z}`)) {
            drop = Math.min(drop, z1 - z - 1);
            break;
          }
        }
      }
    }

    // Drop the brick
    const newZ1 = z1 - drop;
    const newZ2 = z2 - drop;
    settled[origIdx] = [x1, y1, newZ1, x2, y2, newZ2];

    // Mark cells as occupied and find supporters
    for (let x = minX; x <= maxX; x++) {
      for (let y = minY; y <= maxY; y++) {
        // Check for supporters below
        const belowKey = `${x},${y},${newZ1 - 1}`;
        if (occupied.has(belowKey)) {
          const supporterIdx = occupied.get(belowKey);
          supporters.get(origIdx).add(supporterIdx);
          supports.get(supporterIdx).add(origIdx);
        }

        // Mark all cells of this brick
        for (let z = newZ1; z <= newZ2; z++) {
          occupied.set(`${x},${y},${z}`, origIdx);
        }
      }
    }
  }

  return { settled, supports, supporters };
}

function part1(bricks) {
  const { supports, supporters } = settleBricks(bricks);

  let safeCount = 0;
  for (let i = 0; i < bricks.length; i++) {
    let canRemove = true;
    for (const supported of supports.get(i)) {
      if (supporters.get(supported).size === 1) {
        canRemove = false;
        break;
      }
    }
    if (canRemove) safeCount++;
  }
  return safeCount;
}

function part2(bricks) {
  const { supports, supporters } = settleBricks(bricks);

  let totalFalls = 0;

  for (let i = 0; i < bricks.length; i++) {
    // BFS to count chain reaction
    const falling = new Set([i]);
    const queue = [i];
    let head = 0;

    while (head < queue.length) {
      const brick = queue[head++];

      for (const supported of supports.get(brick)) {
        if (falling.has(supported)) continue;

        // Check if all supporters have fallen
        let allFallen = true;
        for (const supporter of supporters.get(supported)) {
          if (!falling.has(supporter)) {
            allFallen = false;
            break;
          }
        }

        if (allFallen) {
          falling.add(supported);
          queue.push(supported);
        }
      }
    }

    totalFalls += falling.size - 1;
  }

  return totalFalls;
}

const inputPath = join(__dirname, '..', 'input.txt');
const bricks = parseInput(inputPath);
console.log(`Part 1: ${part1(bricks)}`);
console.log(`Part 2: ${part2(bricks)}`);
