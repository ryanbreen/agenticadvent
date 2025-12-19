import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Parse input
const parts = input.split('\n\n');
const patterns = parts[0].split(',').map(p => p.trim());
const designs = parts[1].trim().split('\n');

// Part 1
function canForm(design) {
  const memo = new Map();

  function dp(pos) {
    if (pos === design.length) return true;
    if (memo.has(pos)) return memo.get(pos);

    for (const pattern of patterns) {
      const plen = pattern.length;
      if (design.slice(pos, pos + plen) === pattern) {
        if (dp(pos + plen)) {
          memo.set(pos, true);
          return true;
        }
      }
    }
    memo.set(pos, false);
    return false;
  }

  return dp(0);
}

function part1() {
  return designs.filter(d => canForm(d)).length;
}

// Part 2
function countWays(design) {
  const memo = new Map();

  function dp(pos) {
    if (pos === design.length) return 1n;
    if (memo.has(pos)) return memo.get(pos);

    let total = 0n;
    for (const pattern of patterns) {
      const plen = pattern.length;
      if (design.slice(pos, pos + plen) === pattern) {
        total += dp(pos + plen);
      }
    }
    memo.set(pos, total);
    return total;
  }

  return dp(0);
}

function part2() {
  return designs.reduce((sum, d) => sum + countWays(d), 0n);
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
