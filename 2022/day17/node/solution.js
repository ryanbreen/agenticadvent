import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Rock shapes as list of [dx, dy] offsets from bottom-left
const ROCKS = [
  [[0, 0], [1, 0], [2, 0], [3, 0]],           // Horizontal line
  [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]],   // Plus
  [[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]],   // L shape
  [[0, 0], [0, 1], [0, 2], [0, 3]],           // Vertical line
  [[0, 0], [1, 0], [0, 1], [1, 1]]            // Square
];

const WIDTH = 7;

function simulate(jets, numRocks) {
  const occupied = new Set();
  let height = 0;
  let jetIdx = 0;

  // For cycle detection
  const states = new Map();
  const heights = [];

  for (let rockNum = 0; rockNum < numRocks; rockNum++) {
    const rockType = rockNum % 5;
    const rock = ROCKS[rockType];

    // Starting position: left edge at x=2, bottom at y=height+3
    let x = 2;
    let y = height + 3;

    while (true) {
      // Jet push
      const jet = jets[jetIdx];
      jetIdx = (jetIdx + 1) % jets.length;

      const dx = jet === '>' ? 1 : -1;

      // Check if can move horizontally
      let canMove = true;
      for (const [rx, ry] of rock) {
        const nx = x + rx + dx;
        const ny = y + ry;
        if (nx < 0 || nx >= WIDTH || occupied.has(`${nx},${ny}`)) {
          canMove = false;
          break;
        }
      }

      if (canMove) {
        x += dx;
      }

      // Fall down
      let canFall = true;
      for (const [rx, ry] of rock) {
        const nx = x + rx;
        const ny = y + ry - 1;
        if (ny < 0 || occupied.has(`${nx},${ny}`)) {
          canFall = false;
          break;
        }
      }

      if (canFall) {
        y -= 1;
      } else {
        // Rock stops
        for (const [rx, ry] of rock) {
          occupied.add(`${x + rx},${y + ry}`);
          height = Math.max(height, y + ry + 1);
        }
        break;
      }
    }

    heights.push(height);

    // Cycle detection for Part 2
    if (numRocks > 10000) {
      // Create state key from surface profile
      const profileDepth = 30;
      const profile = [];
      for (let col = 0; col < WIDTH; col++) {
        let found = false;
        for (let row = 0; row < profileDepth; row++) {
          if (occupied.has(`${col},${height - 1 - row}`)) {
            profile.push(row);
            found = true;
            break;
          }
        }
        if (!found) {
          profile.push(profileDepth);
        }
      }

      const state = `${rockType},${jetIdx},${profile.join(',')}`;

      if (states.has(state)) {
        // Found cycle
        const cycleStart = states.get(state);
        const cycleLen = rockNum - cycleStart;
        const cycleHeight = height - heights[cycleStart];

        // Calculate final height
        const remaining = numRocks - rockNum - 1;
        const fullCycles = Math.floor(remaining / cycleLen);
        const leftover = remaining % cycleLen;

        let finalHeight = height + fullCycles * cycleHeight;
        if (leftover > 0) {
          finalHeight += heights[cycleStart + leftover] - heights[cycleStart];
        }

        return finalHeight;
      }

      states.set(state, rockNum);
    }
  }

  return height;
}

function part1() {
  return simulate(input, 2022);
}

function part2() {
  return simulate(input, 1000000000000);
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
