#!/usr/bin/env node
/**
 * Day 25: Code Chronicle - Lock and key matching
 */

import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(text) {
  const locks = [];
  const keys = [];

  const schematics = text.trim().split('\n\n');

  for (const schematic of schematics) {
    const lines = schematic.trim().split('\n');

    if (lines[0] === '#####') {
      // Lock: count # from top (excluding top row)
      const heights = [];
      for (let col = 0; col < 5; col++) {
        let height = 0;
        for (let row = 1; row < 7; row++) {
          if (lines[row][col] === '#') {
            height++;
          } else {
            break;
          }
        }
        heights.push(height);
      }
      locks.push(heights);
    } else {
      // Key: count # from bottom (excluding bottom row)
      const heights = [];
      for (let col = 0; col < 5; col++) {
        let height = 0;
        for (let row = 5; row >= 0; row--) {
          if (lines[row][col] === '#') {
            height++;
          } else {
            break;
          }
        }
        heights.push(height);
      }
      keys.push(heights);
    }
  }

  return { locks, keys };
}

function fits(lock, key) {
  for (let i = 0; i < 5; i++) {
    if (lock[i] + key[i] > 5) {
      return false;
    }
  }
  return true;
}

function part1(locks, keys) {
  let count = 0;
  for (const lock of locks) {
    for (const key of keys) {
      if (fits(lock, key)) {
        count++;
      }
    }
  }
  return count;
}

function main() {
  const inputPath = join(__dirname, '..', 'input.txt');
  const text = readFileSync(inputPath, 'utf-8');

  const { locks, keys } = parseInput(text);

  const answer1 = part1(locks, keys);
  console.log(`Part 1: ${answer1}`);

  // Day 25 typically only has Part 1
  console.log('Part 2: Merry Christmas! 🎄');
}

main();
