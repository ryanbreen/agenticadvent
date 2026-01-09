#!/usr/bin/env node
import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(text) {
    const elves = new Set();
    text.trim().split('\n').forEach((line, r) => {
        [...line].forEach((ch, c) => {
            if (ch === '#') elves.add(`${r},${c}`);
        });
    });
    return elves;
}

function simulateRound(elves, directions) {
    const dirChecks = {
        'N': [[[-1, -1], [-1, 0], [-1, 1]], [-1, 0]],
        'S': [[[1, -1], [1, 0], [1, 1]], [1, 0]],
        'W': [[[-1, -1], [0, -1], [1, -1]], [0, -1]],
        'E': [[[-1, 1], [0, 1], [1, 1]], [0, 1]],
    };
    
    const allNeighbors = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]];
    
    const proposals = new Map();
    const proposalCounts = new Map();
    
    for (const elf of elves) {
        const [r, c] = elf.split(',').map(Number);
        
        const hasNeighbor = allNeighbors.some(([dr, dc]) => elves.has(`${r + dr},${c + dc}`));
        if (!hasNeighbor) continue;
        
        for (const d of directions) {
            const [checks, [dr, dc]] = dirChecks[d];
            if (checks.every(([cr, cc]) => !elves.has(`${r + cr},${c + cc}`))) {
                const newPos = `${r + dr},${c + dc}`;
                proposals.set(elf, newPos);
                proposalCounts.set(newPos, (proposalCounts.get(newPos) || 0) + 1);
                break;
            }
        }
    }
    
    const newElves = new Set();
    let moved = false;
    
    for (const elf of elves) {
        if (proposals.has(elf)) {
            const newPos = proposals.get(elf);
            if (proposalCounts.get(newPos) === 1) {
                newElves.add(newPos);
                moved = true;
            } else {
                newElves.add(elf);
            }
        } else {
            newElves.add(elf);
        }
    }
    
    return [newElves, moved];
}

function boundingRectEmpty(elves) {
    let minR = Infinity, maxR = -Infinity, minC = Infinity, maxC = -Infinity;
    for (const elf of elves) {
        const [r, c] = elf.split(',').map(Number);
        minR = Math.min(minR, r);
        maxR = Math.max(maxR, r);
        minC = Math.min(minC, c);
        maxC = Math.max(maxC, c);
    }
    const area = (maxR - minR + 1) * (maxC - minC + 1);
    return area - elves.size;
}

function part1(text) {
    let elves = parseInput(text);
    let directions = ['N', 'S', 'W', 'E'];
    
    for (let i = 0; i < 10; i++) {
        [elves] = simulateRound(elves, directions);
        directions = [...directions.slice(1), directions[0]];
    }
    
    return boundingRectEmpty(elves);
}

function part2(text) {
    let elves = parseInput(text);
    let directions = ['N', 'S', 'W', 'E'];
    let roundNum = 0;
    
    while (true) {
        roundNum++;
        const [newElves, moved] = simulateRound(elves, directions);
        if (!moved) return roundNum;
        elves = newElves;
        directions = [...directions.slice(1), directions[0]];
    }
}

const inputFile = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputFile, 'utf-8');

console.log('Part 1:', part1(text));
console.log('Part 2:', part2(text));
