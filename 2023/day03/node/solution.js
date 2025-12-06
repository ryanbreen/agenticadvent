import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');
const grid = lines.map(line => line.split(''));
const height = grid.length;
const width = grid[0].length;

// Helper to check if a character is a symbol (not digit, not period)
function isSymbol(char) {
  return char !== '.' && isNaN(parseInt(char));
}

// Helper to get all adjacent positions (including diagonals)
function getAdjacentPositions(row, col) {
  const positions = [];
  for (let dr = -1; dr <= 1; dr++) {
    for (let dc = -1; dc <= 1; dc++) {
      if (dr === 0 && dc === 0) continue;
      const newRow = row + dr;
      const newCol = col + dc;
      if (newRow >= 0 && newRow < height && newCol >= 0 && newCol < width) {
        positions.push([newRow, newCol]);
      }
    }
  }
  return positions;
}

// Find all numbers in the grid with their positions
function findNumbers() {
  const numbers = [];

  for (let row = 0; row < height; row++) {
    let col = 0;
    while (col < width) {
      if (!isNaN(parseInt(grid[row][col]))) {
        // Found start of a number
        let numStr = '';
        let startCol = col;
        const positions = [];

        while (col < width && !isNaN(parseInt(grid[row][col]))) {
          numStr += grid[row][col];
          positions.push([row, col]);
          col++;
        }

        numbers.push({
          value: parseInt(numStr),
          positions: positions
        });
      } else {
        col++;
      }
    }
  }

  return numbers;
}

// Part 1
function part1() {
  const numbers = findNumbers();
  let sum = 0;

  for (const num of numbers) {
    // Check if any position of the number is adjacent to a symbol
    let isPartNumber = false;

    for (const [row, col] of num.positions) {
      const adjacent = getAdjacentPositions(row, col);
      for (const [adjRow, adjCol] of adjacent) {
        if (isSymbol(grid[adjRow][adjCol])) {
          isPartNumber = true;
          break;
        }
      }
      if (isPartNumber) break;
    }

    if (isPartNumber) {
      sum += num.value;
    }
  }

  return sum;
}

// Part 2
function part2() {
  const numbers = findNumbers();

  // Find all * symbols and track which numbers are adjacent to each
  const gears = new Map(); // Map of "row,col" -> array of adjacent numbers

  for (const num of numbers) {
    const adjacentGears = new Set();

    for (const [row, col] of num.positions) {
      const adjacent = getAdjacentPositions(row, col);
      for (const [adjRow, adjCol] of adjacent) {
        if (grid[adjRow][adjCol] === '*') {
          adjacentGears.add(`${adjRow},${adjCol}`);
        }
      }
    }

    // Add this number to all gears it's adjacent to
    for (const gearKey of adjacentGears) {
      if (!gears.has(gearKey)) {
        gears.set(gearKey, []);
      }
      gears.get(gearKey).push(num.value);
    }
  }

  // Calculate sum of gear ratios (only gears with exactly 2 adjacent numbers)
  let sum = 0;
  for (const adjacentNumbers of gears.values()) {
    if (adjacentNumbers.length === 2) {
      sum += adjacentNumbers[0] * adjacentNumbers[1];
    }
  }

  return sum;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
