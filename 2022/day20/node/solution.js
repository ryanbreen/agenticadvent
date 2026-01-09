#!/usr/bin/env node
import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(text) {
    return text.trim().split('\n').map(Number);
}

function mix(numbers, times = 1) {
    const n = numbers.length;
    // Store [originalIndex, value] pairs
    const indexed = numbers.map((val, idx) => [idx, val]);

    for (let t = 0; t < times; t++) {
        for (let origIdx = 0; origIdx < n; origIdx++) {
            // Find current position of this element
            let currPos = indexed.findIndex(([idx]) => idx === origIdx);

            // Get the value
            const [, val] = indexed[currPos];

            // Remove from current position
            indexed.splice(currPos, 1);

            // Calculate new position (modulo n-1 because we removed the element)
            let newPos = ((currPos + val) % (n - 1) + (n - 1)) % (n - 1);

            // Insert at new position
            indexed.splice(newPos, 0, [origIdx, val]);
        }
    }

    return indexed.map(([, val]) => val);
}

function groveCoordinates(mixed) {
    const n = mixed.length;
    const zeroIdx = mixed.indexOf(0);
    return [1000, 2000, 3000].reduce((sum, offset) =>
        sum + mixed[(zeroIdx + offset) % n], 0);
}

function part1(text) {
    const numbers = parseInput(text);
    const mixed = mix(numbers, 1);
    return groveCoordinates(mixed);
}

function part2(text) {
    const numbers = parseInput(text);
    const decryptionKey = 811589153;
    const scaled = numbers.map(n => n * decryptionKey);
    const mixed = mix(scaled, 10);
    return groveCoordinates(mixed);
}

const inputFile = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputFile, 'utf-8');

console.log('Part 1:', part1(text));
console.log('Part 2:', part2(text));
