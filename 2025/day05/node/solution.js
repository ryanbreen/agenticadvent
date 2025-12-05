import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const [rangesSection, idsSection] = input.split('\n\n');

// Parse fresh ingredient ranges
const ranges = rangesSection.split('\n').map(line => {
  const [start, end] = line.split('-').map(Number);
  return { start, end };
});

// Parse available ingredient IDs
const availableIds = idsSection.split('\n').map(Number);

// Part 1
function part1() {
  let freshCount = 0;

  for (const id of availableIds) {
    // Check if ID falls within any range
    const isFresh = ranges.some(range => id >= range.start && id <= range.end);
    if (isFresh) {
      freshCount++;
    }
  }

  return freshCount;
}

// Part 2
function part2() {
  // Sort ranges by start position
  const sortedRanges = [...ranges].sort((a, b) => a.start - b.start);

  // Merge overlapping ranges
  const mergedRanges = [];
  let current = sortedRanges[0];

  for (let i = 1; i < sortedRanges.length; i++) {
    const next = sortedRanges[i];

    // Check if ranges overlap or are adjacent
    if (next.start <= current.end + 1) {
      // Merge: extend current range to include next
      current = {
        start: current.start,
        end: Math.max(current.end, next.end)
      };
    } else {
      // No overlap: save current and move to next
      mergedRanges.push(current);
      current = next;
    }
  }
  // Don't forget the last range
  mergedRanges.push(current);

  // Count total unique IDs across all merged ranges
  let totalIds = 0;
  for (const range of mergedRanges) {
    totalIds += (range.end - range.start + 1);
  }

  return totalIds;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
