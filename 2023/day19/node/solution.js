#!/usr/bin/env node
/**
 * Day 19: Aplenty - Workflow processing and range analysis.
 */

import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  const text = readFileSync(filename, 'utf-8').trim();
  const [workflowSection, partsSection] = text.split('\n\n');

  // Parse workflows
  const workflows = {};
  for (const line of workflowSection.split('\n')) {
    const [name, rulesStr] = line.split('{');
    const rules = rulesStr.slice(0, -1).split(',').map(rule => {
      if (rule.includes(':')) {
        const [condition, destination] = rule.split(':');
        const match = condition.match(/([xmas])([<>])(\d+)/);
        return {
          attr: match[1],
          op: match[2],
          value: parseInt(match[3]),
          destination
        };
      } else {
        return { attr: null, op: null, value: null, destination: rule };
      }
    });
    workflows[name] = rules;
  }

  // Parse parts
  const parts = partsSection.split('\n').map(line => {
    const part = {};
    for (const match of line.matchAll(/([xmas])=(\d+)/g)) {
      part[match[1]] = parseInt(match[2]);
    }
    return part;
  });

  return { workflows, parts };
}

function processPart(workflows, part) {
  let current = 'in';

  while (current !== 'A' && current !== 'R') {
    for (const { attr, op, value, destination } of workflows[current]) {
      if (attr === null) {
        current = destination;
        break;
      } else if (op === '<' && part[attr] < value) {
        current = destination;
        break;
      } else if (op === '>' && part[attr] > value) {
        current = destination;
        break;
      }
    }
  }

  return current === 'A';
}

function part1(workflows, parts) {
  let total = 0;
  for (const part of parts) {
    if (processPart(workflows, part)) {
      total += part.x + part.m + part.a + part.s;
    }
  }
  return total;
}

/**
 * Count combinations of xmas values that lead to acceptance.
 * Uses range splitting to process all possible paths through workflows.
 */
function countAccepted(workflows, workflow, ranges) {
  if (workflow === 'R') {
    return 0n;
  }
  if (workflow === 'A') {
    // Count all combinations in current ranges
    let result = 1n;
    for (const [lo, hi] of Object.values(ranges)) {
      result *= BigInt(Math.max(0, hi - lo + 1));
    }
    return result;
  }

  let total = 0n;
  ranges = { ...ranges };  // Make a copy

  for (const { attr, op, value, destination } of workflows[workflow]) {
    if (attr === null) {
      total += countAccepted(workflows, destination, ranges);
    } else {
      const [lo, hi] = ranges[attr];

      if (op === '<') {
        // Split: [lo, value-1] goes to destination, [value, hi] continues
        if (lo < value) {
          const newRanges = { ...ranges };
          newRanges[attr] = [lo, Math.min(hi, value - 1)];
          total += countAccepted(workflows, destination, newRanges);
        }
        // Remaining part continues to next rule
        if (hi >= value) {
          ranges[attr] = [Math.max(lo, value), hi];
        } else {
          break;
        }
      } else {  // op === '>'
        // Split: [value+1, hi] goes to destination, [lo, value] continues
        if (hi > value) {
          const newRanges = { ...ranges };
          newRanges[attr] = [Math.max(lo, value + 1), hi];
          total += countAccepted(workflows, destination, newRanges);
        }
        // Remaining part continues to next rule
        if (lo <= value) {
          ranges[attr] = [lo, Math.min(hi, value)];
        } else {
          break;
        }
      }
    }
  }

  return total;
}

function part2(workflows) {
  const initialRanges = {
    x: [1, 4000],
    m: [1, 4000],
    a: [1, 4000],
    s: [1, 4000]
  };
  return countAccepted(workflows, 'in', initialRanges);
}

const { workflows, parts } = parseInput(join(__dirname, '..', 'input.txt'));
console.log(`Part 1: ${part1(workflows, parts)}`);
console.log(`Part 2: ${part2(workflows)}`);
