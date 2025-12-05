import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

/**
 * Parse input into left and right lists
 * @param {string} input - Raw input string
 * @returns {{left: number[], right: number[]}} - Parsed lists
 */
function parseInput(input) {
  const left = [];
  const right = [];

  for (const line of input.split('\n')) {
    const [leftNum, rightNum] = line.split(/\s+/).map(Number);
    left.push(leftNum);
    right.push(rightNum);
  }

  return { left, right };
}

/**
 * Calculate total distance between sorted left and right lists
 * @param {number[]} left - Left list
 * @param {number[]} right - Right list
 * @returns {number} - Total distance
 */
function calculateTotalDistance(left, right) {
  // Sort both lists
  const sortedLeft = [...left].sort((a, b) => a - b);
  const sortedRight = [...right].sort((a, b) => a - b);

  // Calculate sum of absolute differences
  return sortedLeft.reduce((sum, val, i) => sum + Math.abs(val - sortedRight[i]), 0);
}

/**
 * Calculate similarity score based on frequency of left list numbers in right list
 * @param {number[]} left - Left list
 * @param {number[]} right - Right list
 * @returns {number} - Similarity score
 */
function calculateSimilarityScore(left, right) {
  // Build frequency map for right list
  const rightCounts = right.reduce((map, num) => {
    map.set(num, (map.get(num) || 0) + 1);
    return map;
  }, new Map());

  // Calculate similarity score
  return left.reduce((score, num) => score + num * (rightCounts.get(num) || 0), 0);
}

// Part 1
function part1(left, right) {
  return calculateTotalDistance(left, right);
}

// Part 2
function part2(left, right) {
  return calculateSimilarityScore(left, right);
}

// Parse input once and reuse
const { left, right } = parseInput(input);

console.log('Part 1:', part1(left, right));
console.log('Part 2:', part2(left, right));
