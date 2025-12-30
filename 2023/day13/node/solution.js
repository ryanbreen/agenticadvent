import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(text) {
    return text.trim().split('\n\n').map(block => block.split('\n'));
}

function findVerticalReflection(pattern) {
    if (!pattern.length) return 0;
    const width = pattern[0].length;

    for (let col = 1; col < width; col++) {
        let isReflection = true;
        for (const row of pattern) {
            const left = row.slice(0, col).split('').reverse().join('');
            const right = row.slice(col);
            const minLen = Math.min(left.length, right.length);
            if (left.slice(0, minLen) !== right.slice(0, minLen)) {
                isReflection = false;
                break;
            }
        }
        if (isReflection) return col;
    }
    return 0;
}

function findHorizontalReflection(pattern) {
    if (!pattern.length) return 0;
    const height = pattern.length;

    for (let row = 1; row < height; row++) {
        let isReflection = true;
        const top = pattern.slice(0, row).reverse();
        const bottom = pattern.slice(row);
        const minLen = Math.min(top.length, bottom.length);
        for (let i = 0; i < minLen; i++) {
            if (top[i] !== bottom[i]) {
                isReflection = false;
                break;
            }
        }
        if (isReflection) return row;
    }
    return 0;
}

function summarizePattern(pattern) {
    const v = findVerticalReflection(pattern);
    if (v > 0) return v;
    return findHorizontalReflection(pattern) * 100;
}

function part1(patterns) {
    return patterns.reduce((sum, p) => sum + summarizePattern(p), 0);
}

function countDifferences(s1, s2) {
    let diff = 0;
    const minLen = Math.min(s1.length, s2.length);
    for (let i = 0; i < minLen; i++) {
        if (s1[i] !== s2[i]) diff++;
    }
    return diff;
}

function findVerticalReflectionWithSmudge(pattern) {
    if (!pattern.length) return 0;
    const width = pattern[0].length;

    for (let col = 1; col < width; col++) {
        let totalDiff = 0;
        for (const row of pattern) {
            const left = row.slice(0, col).split('').reverse().join('');
            const right = row.slice(col);
            const minLen = Math.min(left.length, right.length);
            totalDiff += countDifferences(left.slice(0, minLen), right.slice(0, minLen));
            if (totalDiff > 1) break;
        }
        if (totalDiff === 1) return col;
    }
    return 0;
}

function findHorizontalReflectionWithSmudge(pattern) {
    if (!pattern.length) return 0;
    const height = pattern.length;

    for (let row = 1; row < height; row++) {
        let totalDiff = 0;
        const top = pattern.slice(0, row).reverse();
        const bottom = pattern.slice(row);
        const minLen = Math.min(top.length, bottom.length);
        for (let i = 0; i < minLen; i++) {
            totalDiff += countDifferences(top[i], bottom[i]);
            if (totalDiff > 1) break;
        }
        if (totalDiff === 1) return row;
    }
    return 0;
}

function summarizePatternWithSmudge(pattern) {
    const v = findVerticalReflectionWithSmudge(pattern);
    if (v > 0) return v;
    return findHorizontalReflectionWithSmudge(pattern) * 100;
}

function part2(patterns) {
    return patterns.reduce((sum, p) => sum + summarizePatternWithSmudge(p), 0);
}

const inputFile = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputFile, 'utf-8');
const patterns = parseInput(text);

console.log(`Part 1: ${part1(patterns)}`);
console.log(`Part 2: ${part2(patterns)}`);
