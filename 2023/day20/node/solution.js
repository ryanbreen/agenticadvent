#!/usr/bin/env node
/**
 * Day 20: Pulse Propagation - Module communication simulation.
 */

import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  const text = readFileSync(filename, 'utf-8').trim();
  const modules = {};

  for (const line of text.split('\n')) {
    const [namePart, destPart] = line.split(' -> ');
    const destinations = destPart.split(',').map(d => d.trim());

    if (namePart === 'broadcaster') {
      modules.broadcaster = { type: 'broadcaster', destinations };
    } else if (namePart.startsWith('%')) {
      const name = namePart.slice(1);
      modules[name] = { type: 'flip-flop', destinations, state: false };
    } else if (namePart.startsWith('&')) {
      const name = namePart.slice(1);
      modules[name] = { type: 'conjunction', destinations, memory: {} };
    }
  }

  // Initialize conjunction memory for all inputs
  for (const [name, module] of Object.entries(modules)) {
    for (const dest of module.destinations) {
      if (modules[dest] && modules[dest].type === 'conjunction') {
        modules[dest].memory[name] = false;
      }
    }
  }

  return modules;
}

function simulateButtonPress(modules, watchNodes = null) {
  let lowCount = 0;
  let highCount = 0;
  const highSenders = new Set();

  // Queue: [source, destination, pulse] where pulse is true for high, false for low
  const queue = [['button', 'broadcaster', false]];

  while (queue.length > 0) {
    const [source, dest, pulse] = queue.shift();

    if (pulse) {
      highCount++;
    } else {
      lowCount++;
    }

    // Track if watched nodes send high pulses
    if (watchNodes && watchNodes.has(source) && pulse) {
      highSenders.add(source);
    }

    if (!modules[dest]) continue;

    const module = modules[dest];

    if (module.type === 'broadcaster') {
      for (const nextDest of module.destinations) {
        queue.push([dest, nextDest, pulse]);
      }
    } else if (module.type === 'flip-flop') {
      if (!pulse) {  // Only react to low pulses
        module.state = !module.state;
        for (const nextDest of module.destinations) {
          queue.push([dest, nextDest, module.state]);
        }
      }
    } else if (module.type === 'conjunction') {
      module.memory[source] = pulse;
      // Send low if all inputs are high, otherwise send high
      const output = !Object.values(module.memory).every(v => v);
      for (const nextDest of module.destinations) {
        queue.push([dest, nextDest, output]);
      }
    }
  }

  return { lowCount, highCount, highSenders };
}

function resetModules(modules) {
  for (const module of Object.values(modules)) {
    if (module.type === 'flip-flop') {
      module.state = false;
    } else if (module.type === 'conjunction') {
      for (const key of Object.keys(module.memory)) {
        module.memory[key] = false;
      }
    }
  }
}

function part1(modules) {
  resetModules(modules);

  let totalLow = 0;
  let totalHigh = 0;

  for (let i = 0; i < 1000; i++) {
    const { lowCount, highCount } = simulateButtonPress(modules);
    totalLow += lowCount;
    totalHigh += highCount;
  }

  return totalLow * totalHigh;
}

function gcd(a, b) {
  while (b !== 0n) {
    [a, b] = [b, a % b];
  }
  return a;
}

function lcm(a, b) {
  return (a * b) / gcd(a, b);
}

function part2(modules) {
  resetModules(modules);

  // Find the module that feeds into rx
  let rxInput = null;
  for (const [name, module] of Object.entries(modules)) {
    if (module.destinations.includes('rx')) {
      rxInput = name;
      break;
    }
  }

  if (!rxInput) return 0;

  // Find all modules that feed into rxInput
  const watchNodes = new Set(Object.keys(modules[rxInput].memory));
  const cycleLengths = {};

  let buttonPress = 0n;
  while (Object.keys(cycleLengths).length < watchNodes.size) {
    buttonPress++;
    const { highSenders } = simulateButtonPress(modules, watchNodes);

    for (const node of highSenders) {
      if (!(node in cycleLengths)) {
        cycleLengths[node] = buttonPress;
      }
    }
  }

  // LCM of all cycle lengths
  let result = 1n;
  for (const length of Object.values(cycleLengths)) {
    result = lcm(result, length);
  }

  return result;
}

const inputPath = join(__dirname, '..', 'input.txt');
const modules1 = parseInput(inputPath);
console.log(`Part 1: ${part1(modules1)}`);

const modules2 = parseInput(inputPath);
console.log(`Part 2: ${part2(modules2)}`);
