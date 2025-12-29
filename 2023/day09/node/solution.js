import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

function parseInput(text) {
    return text.split('\n').map(line =>
        line.split(/\s+/).map(Number)
    );
}

function getDifferences(seq) {
    const result = [];
    for (let i = 0; i < seq.length - 1; i++) {
        result.push(seq[i + 1] - seq[i]);
    }
    return result;
}

function extrapolateNext(seq) {
    const sequences = [[...seq]];
    let current = seq;

    while (!current.every(x => x === 0)) {
        current = getDifferences(current);
        sequences.push(current);
    }

    for (let i = sequences.length - 2; i >= 0; i--) {
        sequences[i].push(sequences[i].at(-1) + sequences[i + 1].at(-1));
    }

    return sequences[0].at(-1);
}

function extrapolatePrev(seq) {
    const sequences = [[...seq]];
    let current = seq;

    while (!current.every(x => x === 0)) {
        current = getDifferences(current);
        sequences.push(current);
    }

    for (let i = sequences.length - 2; i >= 0; i--) {
        sequences[i].unshift(sequences[i][0] - sequences[i + 1][0]);
    }

    return sequences[0][0];
}

const histories = parseInput(input);

function part1() {
    return histories.reduce((sum, h) => sum + extrapolateNext(h), 0);
}

function part2() {
    return histories.reduce((sum, h) => sum + extrapolatePrev(h), 0);
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
