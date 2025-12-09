import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input - each line is "x,y"
const points = input.split('\n').map(line => {
  const [x, y] = line.split(',').map(Number);
  return { x, y };
});

// Part 1: Find largest rectangle area using two red tiles as opposite corners
function part1() {
  let maxArea = 0;
  const n = points.length;

  // Check all pairs of points as opposite corners
  for (let i = 0; i < n; i++) {
    const { x: x1, y: y1 } = points[i];
    for (let j = i + 1; j < n; j++) {
      const { x: x2, y: y2 } = points[j];
      // Rectangle area = width * height (inclusive of both corners)
      const width = Math.abs(x2 - x1) + 1;
      const height = Math.abs(y2 - y1) + 1;
      const area = width * height;
      maxArea = Math.max(maxArea, area);
    }
  }

  return maxArea;
}

// Part 2: Find largest rectangle using only red and green tiles
function part2() {
  const n = points.length;
  const horizontalEdges = [];  // [y, xMin, xMax]
  const verticalEdges = [];    // [x, yMin, yMax]

  // Build edges from consecutive points
  for (let i = 0; i < n; i++) {
    const { x: x1, y: y1 } = points[i];
    const { x: x2, y: y2 } = points[(i + 1) % n];

    if (y1 === y2) {  // Horizontal edge
      horizontalEdges.push([y1, Math.min(x1, x2), Math.max(x1, x2)]);
    } else {  // Vertical edge
      verticalEdges.push([x1, Math.min(y1, y2), Math.max(y1, y2)]);
    }
  }

  // Build maps for efficient lookup
  const vertByX = new Map();
  for (const [x, yMin, yMax] of verticalEdges) {
    if (!vertByX.has(x)) vertByX.set(x, []);
    vertByX.get(x).push([yMin, yMax]);
  }

  const horizByY = new Map();
  for (const [y, xMin, xMax] of horizontalEdges) {
    if (!horizByY.has(y)) horizByY.set(y, []);
    horizByY.get(y).push([xMin, xMax]);
  }

  // Check if point is inside polygon using ray casting
  function isInsidePolygon(x, y) {
    let crossings = 0;
    for (const vx of [...vertByX.keys()].sort((a, b) => a - b)) {
      if (vx <= x) continue;
      for (const [yMin, yMax] of vertByX.get(vx)) {
        if (yMin < y && y < yMax) {
          crossings += 1;
        } else if (y === yMin || y === yMax) {
          crossings += 0.5;
        }
      }
    }
    return crossings % 2 === 1;
  }

  // Check if rectangle is entirely inside polygon
  function rectangleValid(x1, y1, x2, y2) {
    const minX = Math.min(x1, x2);
    const maxX = Math.max(x1, x2);
    const minY = Math.min(y1, y2);
    const maxY = Math.max(y1, y2);

    // Check if any vertical edge crosses through rectangle interior
    for (const [vx, edges] of vertByX) {
      if (minX < vx && vx < maxX) {
        for (const [yMin, yMax] of edges) {
          if (!(yMax <= minY || yMin >= maxY)) {
            return false;
          }
        }
      }
    }

    // Check if any horizontal edge crosses through rectangle interior
    for (const [hy, edges] of horizByY) {
      if (minY < hy && hy < maxY) {
        for (const [xMin, xMax] of edges) {
          if (!(xMax <= minX || xMin >= maxX)) {
            return false;
          }
        }
      }
    }

    // Check center is inside polygon
    const centerX = (minX + maxX) / 2;
    const centerY = (minY + maxY) / 2;
    return isInsidePolygon(centerX, centerY);
  }

  // Find largest valid rectangle
  let maxArea = 0;

  for (let i = 0; i < n; i++) {
    const { x: x1, y: y1 } = points[i];
    for (let j = i + 1; j < n; j++) {
      const { x: x2, y: y2 } = points[j];

      if (rectangleValid(x1, y1, x2, y2)) {
        const width = Math.abs(x2 - x1) + 1;
        const height = Math.abs(y2 - y1) + 1;
        const area = width * height;
        maxArea = Math.max(maxArea, area);
      }
    }
  }

  return maxArea;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
