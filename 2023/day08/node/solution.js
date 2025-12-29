import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(text) {
    const lines = text.trim().split('\n');
    const instructions = lines[0];

    const network = new Map();
    for (let i = 2; i < lines.length; i++) {
        const line = lines[i].trim();
        if (!line) continue;

        // Parse: AAA = (BBB, CCC)
        const [node, rest] = line.split(' = ');
        const [left, right] = rest.slice(1, -1).split(', ');
        network.set(node, [left, right]);
    }

    return { instructions, network };
}

function part1(instructions, network) {
    let current = 'AAA';
    let steps = 0;
    const instructionLen = instructions.length;

    while (current !== 'ZZZ') {
        const instruction = instructions[steps % instructionLen];
        if (instruction === 'L') {
            current = network.get(current)[0];
        } else {
            current = network.get(current)[1];
        }
        steps++;
    }

    return steps;
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

function part2(instructions, network) {
    // Find all starting nodes (ending in A)
    const startingNodes = [...network.keys()].filter(node => node.endsWith('A'));

    const instructionLen = instructions.length;
    const cycleLengths = [];

    for (const startNode of startingNodes) {
        let current = startNode;
        let steps = 0;

        while (!current.endsWith('Z')) {
            const instruction = instructions[steps % instructionLen];
            if (instruction === 'L') {
                current = network.get(current)[0];
            } else {
                current = network.get(current)[1];
            }
            steps++;
        }
        cycleLengths.push(BigInt(steps));
    }

    // Find LCM of all cycle lengths
    let result = cycleLengths[0];
    for (let i = 1; i < cycleLengths.length; i++) {
        result = lcm(result, cycleLengths[i]);
    }

    return result;
}

function main() {
    const inputPath = join(__dirname, '..', 'input.txt');
    const text = readFileSync(inputPath, 'utf-8');

    const { instructions, network } = parseInput(text);

    console.log('Part 1:', part1(instructions, network));
    console.log('Part 2:', part2(instructions, network).toString());
}

main();
