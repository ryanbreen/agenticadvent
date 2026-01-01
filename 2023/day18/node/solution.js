#!/usr/bin/env node
/**
 * Day 18: Lavaduct Lagoon - Polygon area with Shoelace formula and Pick's theorem.
 */

import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  const text = readFileSync(filename, 'utf-8').trim();
  return text.split('\n').map(line => {
    const parts = line.split(' ');
    return {
      direction: parts[0],
      distance: parseInt(parts[1]),
      color: parts[2].slice(2, -1)  // Remove (# and )
    };
  });
}

/**
 * Calculate total area using Shoelace formula and Pick's theorem.
 *
 * Shoelace gives us twice the signed area of the polygon.
 * Pick's theorem: A = i + b/2 - 1, where i = interior points, b = boundary points
 * We want: Total = i + b = A + b/2 + 1
 */
function calculateArea(vertices, perimeter) {
  // Shoelace formula for polygon area
  const n = vertices.length;
  let area = 0n;
  for (let i = 0; i < n; i++) {
    const j = (i + 1) % n;
    area += BigInt(vertices[i][0]) * BigInt(vertices[j][1]);
    area -= BigInt(vertices[j][0]) * BigInt(vertices[i][1]);
  }
  if (area < 0n) area = -area;
  area = area / 2n;

  // Total points = interior + boundary
  // From Pick's theorem: interior = area - boundary/2 + 1
  // Total = interior + boundary = area + boundary/2 + 1
  return area + BigInt(perimeter) / 2n + 1n;
}

function part1(instructions) {
  const directionMap = {
    'R': [0, 1],
    'D': [1, 0],
    'L': [0, -1],
    'U': [-1, 0]
  };

  const vertices = [[0, 0]];
  let perimeter = 0;
  let r = 0, c = 0;

  for (const { direction, distance } of instructions) {
    const [dr, dc] = directionMap[direction];
    r += dr * distance;
    c += dc * distance;
    vertices.push([r, c]);
    perimeter += distance;
  }

  return calculateArea(vertices, perimeter);
}

function part2(instructions) {
  // Last digit of hex: 0=R, 1=D, 2=L, 3=U
  // First 5 digits: distance in hex
  const directionMap = {
    '0': [0, 1],   // R
    '1': [1, 0],   // D
    '2': [0, -1],  // L
    '3': [-1, 0]   // U
  };

  const vertices = [[0, 0]];
  let perimeter = 0;
  let r = 0, c = 0;

  for (const { color } of instructions) {
    const distance = parseInt(color.slice(0, 5), 16);
    const direction = color[5];
    const [dr, dc] = directionMap[direction];
    r += dr * distance;
    c += dc * distance;
    vertices.push([r, c]);
    perimeter += distance;
  }

  return calculateArea(vertices, perimeter);
}

const instructions = parseInput(join(__dirname, '..', 'input.txt'));
console.log(`Part 1: ${part1(instructions)}`);
console.log(`Part 2: ${part2(instructions)}`);
