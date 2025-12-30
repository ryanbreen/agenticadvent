import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

const parseInput = (text) => text.trim().split('\n\n').map(block => block.split('\n'));

const countDifferences = (s1, s2) => {
    const len = Math.min(s1.length, s2.length);
    return [...Array(len)].reduce((diff, _, i) => diff + (s1[i] !== s2[i] ? 1 : 0), 0);
};

const findVerticalReflection = (pattern, targetDiff) => {
    if (!pattern.length) return 0;
    const [firstRow] = pattern;

    for (let col = 1; col < firstRow.length; col++) {
        const totalDiff = pattern.reduce((diff, row) => {
            const left = [...row.slice(0, col)].reverse().join('');
            const right = row.slice(col);
            const len = Math.min(left.length, right.length);
            return diff + countDifferences(left.slice(0, len), right.slice(0, len));
        }, 0);

        if (totalDiff === targetDiff) return col;
    }
    return 0;
};

const findHorizontalReflection = (pattern, targetDiff) => {
    if (!pattern.length) return 0;

    for (let row = 1; row < pattern.length; row++) {
        const top = pattern.slice(0, row).reverse();
        const bottom = pattern.slice(row);
        const len = Math.min(top.length, bottom.length);

        const totalDiff = [...Array(len)].reduce(
            (diff, _, i) => diff + countDifferences(top[i], bottom[i]),
            0
        );

        if (totalDiff === targetDiff) return row;
    }
    return 0;
};

const summarizePattern = (pattern, targetDiff) => {
    const vertical = findVerticalReflection(pattern, targetDiff);
    return vertical > 0 ? vertical : findHorizontalReflection(pattern, targetDiff) * 100;
};

const part1 = (patterns) => patterns.reduce((sum, p) => sum + summarizePattern(p, 0), 0);

const part2 = (patterns) => patterns.reduce((sum, p) => sum + summarizePattern(p, 1), 0);

const inputFile = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputFile, 'utf-8');
const patterns = parseInput(text);

console.log(`Part 1: ${part1(patterns)}`);
console.log(`Part 2: ${part2(patterns)}`);
