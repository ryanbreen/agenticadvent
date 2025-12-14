import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');
const grid = lines.map(line => line.split(''));
const rows = grid.length;
const cols = grid[0].length;

// Directions constant to avoid duplication
const DIRECTIONS = [[0, 1], [0, -1], [1, 0], [-1, 0]];

// Encode coordinates as a single number for better performance
const encode = (r, c) => r * cols + c;
const decode = (key) => [Math.floor(key / cols), key % cols];

function findRegions() {
  const visited = new Set();
  const regions = [];

  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      const key = encode(r, c);
      if (visited.has(key)) continue;

      // DFS to find all cells in this region (using array.pop() for O(1) instead of shift())
      const plant = grid[r][c];
      const region = new Set();
      const stack = [[r, c]];

      while (stack.length > 0) {
        const [cr, cc] = stack.pop();
        const currKey = encode(cr, cc);

        if (visited.has(currKey)) continue;
        if (cr < 0 || cr >= rows || cc < 0 || cc >= cols) continue;
        if (grid[cr][cc] !== plant) continue;

        visited.add(currKey);
        region.add(currKey);

        for (const [dr, dc] of DIRECTIONS) {
          const nr = cr + dr;
          const nc = cc + dc;
          const nextKey = encode(nr, nc);
          if (!visited.has(nextKey)) {
            stack.push([nr, nc]);
          }
        }
      }

      regions.push(region);
    }
  }

  return regions;
}

function calculatePerimeter(region) {
  let perimeter = 0;

  for (const key of region) {
    const [r, c] = decode(key);
    for (const [dr, dc] of DIRECTIONS) {
      const nr = r + dr;
      const nc = c + dc;
      const neighborKey = encode(nr, nc);
      if (!region.has(neighborKey)) {
        perimeter++;
      }
    }
  }

  return perimeter;
}

function countSides(region) {
  let corners = 0;

  for (const key of region) {
    const [r, c] = decode(key);

    // Check all 4 corners of this cell
    const up = region.has(encode(r - 1, c));
    const down = region.has(encode(r + 1, c));
    const left = region.has(encode(r, c - 1));
    const right = region.has(encode(r, c + 1));
    const upLeft = region.has(encode(r - 1, c - 1));
    const upRight = region.has(encode(r - 1, c + 1));
    const downLeft = region.has(encode(r + 1, c - 1));
    const downRight = region.has(encode(r + 1, c + 1));

    // Top-left corner
    if (!up && !left) corners++;  // convex
    else if (up && left && !upLeft) corners++;  // concave

    // Top-right corner
    if (!up && !right) corners++;  // convex
    else if (up && right && !upRight) corners++;  // concave

    // Bottom-left corner
    if (!down && !left) corners++;  // convex
    else if (down && left && !downLeft) corners++;  // concave

    // Bottom-right corner
    if (!down && !right) corners++;  // convex
    else if (down && right && !downRight) corners++;  // concave
  }

  return corners;
}

// Compute regions once and reuse for both parts
const regions = findRegions();

// Part 1
function part1() {
  let total = 0;
  for (const region of regions) {
    const area = region.size;
    const perimeter = calculatePerimeter(region);
    total += area * perimeter;
  }
  return total;
}

// Part 2
function part2() {
  let total = 0;
  for (const region of regions) {
    const area = region.size;
    const sides = countSides(region);
    total += area * sides;
  }
  return total;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
