import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

function parseInput(text) {
  const blueprints = [];
  const pattern = /Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\./;

  for (const line of text.split('\n')) {
    const m = line.match(pattern);
    if (m) {
      blueprints.push(m.slice(1).map(Number));
    }
  }
  return blueprints;
}

function maxGeodes(bp, timeLimit) {
  const [bpId, oreOre, clayOre, obsOre, obsClay, geoOre, geoObs] = bp;

  // Max robots needed per type
  const maxOre = Math.max(oreOre, clayOre, obsOre, geoOre);
  const maxClay = obsClay;
  const maxObs = geoObs;

  let best = 0;

  // DFS with stack (faster than BFS with shift)
  function dfs(time, ore, clay, obs, geodes, oreR, clayR, obsR, geoR, seen) {
    // Pruning: upper bound on possible geodes
    const remaining = timeLimit - time;
    const upperBound = geodes + geoR * remaining + (remaining * (remaining - 1)) / 2;
    if (upperBound <= best) return;

    if (time === timeLimit) {
      best = Math.max(best, geodes);
      return;
    }

    // Cap resources
    const cappedOre = Math.min(ore, remaining * maxOre);
    const cappedClay = Math.min(clay, remaining * maxClay);
    const cappedObs = Math.min(obs, remaining * maxObs);

    // State deduplication
    const key = `${time},${cappedOre},${cappedClay},${cappedObs},${oreR},${clayR},${obsR},${geoR}`;
    if (seen.has(key) && seen.get(key) >= geodes) return;
    seen.set(key, geodes);

    // Collect resources
    const newOre = cappedOre + oreR;
    const newClay = cappedClay + clayR;
    const newObs = cappedObs + obsR;
    const newGeodes = geodes + geoR;

    // Try building geode robot (always do if possible)
    if (cappedOre >= geoOre && cappedObs >= geoObs) {
      dfs(time + 1, newOre - geoOre, newClay, newObs - geoObs, newGeodes,
          oreR, clayR, obsR, geoR + 1, seen);
      return; // If we can build geode, always do
    }

    // Try building obsidian robot
    if (cappedOre >= obsOre && cappedClay >= obsClay && obsR < maxObs) {
      dfs(time + 1, newOre - obsOre, newClay - obsClay, newObs, newGeodes,
          oreR, clayR, obsR + 1, geoR, seen);
    }

    // Try building clay robot
    if (cappedOre >= clayOre && clayR < maxClay) {
      dfs(time + 1, newOre - clayOre, newClay, newObs, newGeodes,
          oreR, clayR + 1, obsR, geoR, seen);
    }

    // Try building ore robot
    if (cappedOre >= oreOre && oreR < maxOre) {
      dfs(time + 1, newOre - oreOre, newClay, newObs, newGeodes,
          oreR + 1, clayR, obsR, geoR, seen);
    }

    // Do nothing (wait)
    dfs(time + 1, newOre, newClay, newObs, newGeodes,
        oreR, clayR, obsR, geoR, seen);
  }

  dfs(0, 0, 0, 0, 0, 1, 0, 0, 0, new Map());
  return best;
}

function part1() {
  const blueprints = parseInput(input);
  let total = 0;
  for (const bp of blueprints) {
    const geodes = maxGeodes(bp, 24);
    total += bp[0] * geodes;
  }
  return total;
}

function part2() {
  const blueprints = parseInput(input).slice(0, 3);
  let result = 1;
  for (const bp of blueprints) {
    const geodes = maxGeodes(bp, 32);
    result *= geodes;
  }
  return result;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
