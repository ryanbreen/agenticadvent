import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

// Direction offsets for 8 neighbors (including diagonals)
const DIRECTIONS = [
  [-1, -1], [-1, 0], [-1, 1],  // top-left, top, top-right
  [0, -1],           [0, 1],   // left, right
  [1, -1],  [1, 0],  [1, 1]    // bottom-left, bottom, bottom-right
];

// Count the number of adjacent rolls ('@') around position (row, col)
function countAdjacentRolls(grid, row, col) {
  const rows = grid.length;
  const cols = grid[0].length;
  let count = 0;

  for (const [dr, dc] of DIRECTIONS) {
    const newRow = row + dr;
    const newCol = col + dc;

    // Check if neighbor is within bounds and is a paper roll
    if (newRow >= 0 && newRow < rows &&
        newCol >= 0 && newCol < cols &&
        grid[newRow][newCol] === '@') {
      count++;
    }
  }

  return count;
}

// Part 1
function part1() {
  const grid = lines.map(line => line.split(''));
  const rows = grid.length;
  const cols = grid[0].length;

  let accessibleCount = 0;

  // Check each cell in the grid
  for (let row = 0; row < rows; row++) {
    for (let col = 0; col < cols; col++) {
      // Only count if this cell contains a paper roll
      if (grid[row][col] === '@') {
        const adjacentRolls = countAdjacentRolls(grid, row, col);
        // A roll is accessible if it has fewer than 4 adjacent rolls
        if (adjacentRolls < 4) {
          accessibleCount++;
        }
      }
    }
  }

  return accessibleCount;
}

// Part 2
function part2() {
  // Create a mutable copy of the grid
  const grid = lines.map(line => line.split(''));
  const rows = grid.length;
  const cols = grid[0].length;

  let totalRemoved = 0;

  // Loop until no more rolls can be removed
  while (true) {
    // Find all rolls with fewer than 4 adjacent rolls
    const rollsToRemove = [];

    for (let row = 0; row < rows; row++) {
      for (let col = 0; col < cols; col++) {
        // Only check cells that still contain a paper roll
        if (grid[row][col] === '@') {
          const adjacentRolls = countAdjacentRolls(grid, row, col);
          // A roll can be removed if it has fewer than 4 adjacent rolls
          if (adjacentRolls < 4) {
            rollsToRemove.push([row, col]);
          }
        }
      }
    }

    // If no rolls can be removed, we're done
    if (rollsToRemove.length === 0) {
      break;
    }

    // Remove all accessible rolls
    for (const [row, col] of rollsToRemove) {
      grid[row][col] = '.';  // Mark as empty
    }

    totalRemoved += rollsToRemove.length;
  }

  return totalRemoved;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
