import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const lines = input.split('\n');

function parseGame(line) {
  // Parse "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  const [gameInfo, cubeData] = line.split(': ');
  const gameId = parseInt(gameInfo.replace('Game ', ''));

  // Parse each draw (separated by semicolons)
  const draws = cubeData.split('; ').map(draw => {
    const cubes = { red: 0, green: 0, blue: 0 };

    // Parse each color count in the draw
    draw.split(', ').forEach(cube => {
      const [count, color] = cube.split(' ');
      cubes[color] = parseInt(count);
    });

    return cubes;
  });

  return { gameId, draws };
}

// Part 1
function part1() {
  const maxCubes = { red: 12, green: 13, blue: 14 };
  let sum = 0;

  for (const line of lines) {
    const { gameId, draws } = parseGame(line);

    // Check if all draws in this game are possible
    const isPossible = draws.every(draw =>
      draw.red <= maxCubes.red &&
      draw.green <= maxCubes.green &&
      draw.blue <= maxCubes.blue
    );

    if (isPossible) {
      sum += gameId;
    }
  }

  return sum;
}

// Part 2
function part2() {
  let totalPower = 0;

  for (const line of lines) {
    const { draws } = parseGame(line);

    // Find minimum cubes needed for each color
    const minCubes = {
      red: Math.max(...draws.map(d => d.red)),
      green: Math.max(...draws.map(d => d.green)),
      blue: Math.max(...draws.map(d => d.blue))
    };

    // Calculate power (product of minimum cubes)
    const power = minCubes.red * minCubes.green * minCubes.blue;
    totalPower += power;
  }

  return totalPower;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
