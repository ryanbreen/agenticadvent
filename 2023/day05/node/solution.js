import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(text) {
  const sections = text.trim().split('\n\n');

  // Parse seeds
  const seeds = sections[0].split(': ')[1].split(' ').map(Number);

  // Parse maps
  const maps = [];
  for (let i = 1; i < sections.length; i++) {
    const lines = sections[i].trim().split('\n');
    const ranges = [];
    for (let j = 1; j < lines.length; j++) {
      const [dstStart, srcStart, length] = lines[j].split(' ').map(Number);
      ranges.push({ dstStart, srcStart, length });
    }
    maps.push(ranges);
  }

  return { seeds, maps };
}

function applyMap(value, ranges) {
  for (const { dstStart, srcStart, length } of ranges) {
    if (value >= srcStart && value < srcStart + length) {
      return dstStart + (value - srcStart);
    }
  }
  return value;
}

function seedToLocation(seed, maps) {
  let value = seed;
  for (const mapRanges of maps) {
    value = applyMap(value, mapRanges);
  }
  return value;
}

function part1(seeds, maps) {
  return Math.min(...seeds.map(seed => seedToLocation(seed, maps)));
}

function applyMapToRanges(inputRanges, mapRanges) {
  const result = [];

  for (const [start, end] of inputRanges) {
    let remaining = [[start, end]];

    for (const { dstStart, srcStart, length } of mapRanges) {
      const srcEnd = srcStart + length;
      const newRemaining = [];

      for (const [rStart, rEnd] of remaining) {
        // Part before the map range (unmapped)
        if (rStart < srcStart) {
          newRemaining.push([rStart, Math.min(rEnd, srcStart)]);
        }

        // Part within the map range (mapped)
        const overlapStart = Math.max(rStart, srcStart);
        const overlapEnd = Math.min(rEnd, srcEnd);
        if (overlapStart < overlapEnd) {
          const offset = dstStart - srcStart;
          result.push([overlapStart + offset, overlapEnd + offset]);
        }

        // Part after the map range (unmapped)
        if (rEnd > srcEnd) {
          newRemaining.push([Math.max(rStart, srcEnd), rEnd]);
        }
      }

      remaining = newRemaining;
    }

    // Any remaining parts are unmapped (identity)
    result.push(...remaining);
  }

  return result;
}

function part2(seeds, maps) {
  // Convert seeds to ranges
  let ranges = [];
  for (let i = 0; i < seeds.length; i += 2) {
    const start = seeds[i];
    const length = seeds[i + 1];
    ranges.push([start, start + length]);
  }

  // Apply each map to the ranges
  for (const mapRanges of maps) {
    ranges = applyMapToRanges(ranges, mapRanges);
  }

  // Find minimum start of any range
  return Math.min(...ranges.map(([start]) => start));
}

const inputPath = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputPath, 'utf-8');

const { seeds, maps } = parseInput(text);

console.log('Part 1:', part1(seeds, maps));
console.log('Part 2:', part2(seeds, maps));
