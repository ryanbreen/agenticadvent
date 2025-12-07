import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf8').trim();
const lines = input.split('\n');

function part1() {
    const rows = lines.length;
    const cols = lines[0]?.length || 0;

    // Find starting position S
    let startCol = null;
    for (let col = 0; col < cols; col++) {
        if (lines[0][col] === 'S') {
            startCol = col;
            break;
        }
    }

    if (startCol === null) return 0;

    // Track active beam columns at each row
    let activeBeams = new Set([startCol]);
    let splitCount = 0;

    // Process row by row starting from row 1 (below S)
    for (let row = 1; row < rows; row++) {
        const newBeams = new Set();

        for (const col of activeBeams) {
            if (col >= 0 && col < cols) {
                const cell = lines[row][col];
                if (cell === '^') {
                    // Beam hits splitter - count it and emit left/right
                    splitCount++;
                    if (col - 1 >= 0) newBeams.add(col - 1);
                    if (col + 1 < cols) newBeams.add(col + 1);
                } else if (cell === '.') {
                    // Beam continues straight down
                    newBeams.add(col);
                } else {
                    // Other characters - beam continues
                    newBeams.add(col);
                }
            }
        }

        activeBeams = newBeams;
        if (activeBeams.size === 0) break;
    }

    return splitCount;
}

function part2() {
    const rows = lines.length;
    const cols = lines[0]?.length || 0;

    // Find starting position S
    let startCol = null;
    for (let col = 0; col < cols; col++) {
        if (lines[0][col] === 'S') {
            startCol = col;
            break;
        }
    }

    if (startCol === null) return 0n;

    // Track number of timelines at each column position
    // Use a Map: col -> count of timelines (BigInt for large numbers)
    let timelines = new Map();
    timelines.set(startCol, 1n);

    // Process row by row starting from row 1 (below S)
    for (let row = 1; row < rows; row++) {
        const newTimelines = new Map();

        for (const [col, count] of timelines) {
            if (col >= 0 && col < cols) {
                const cell = lines[row][col];
                if (cell === '^') {
                    // Each timeline splits into 2 (left and right)
                    if (col - 1 >= 0) {
                        newTimelines.set(col - 1, (newTimelines.get(col - 1) || 0n) + count);
                    }
                    if (col + 1 < cols) {
                        newTimelines.set(col + 1, (newTimelines.get(col + 1) || 0n) + count);
                    }
                } else if (cell === '.') {
                    // Timelines continue straight down
                    newTimelines.set(col, (newTimelines.get(col) || 0n) + count);
                } else {
                    // Other characters - timelines continue
                    newTimelines.set(col, (newTimelines.get(col) || 0n) + count);
                }
            }
        }

        timelines = newTimelines;
        if (timelines.size === 0) break;
    }

    // Total number of timelines
    let total = 0n;
    for (const count of timelines.values()) {
        total += count;
    }
    return total;
}

console.log(`Part 1: ${part1()}`);
console.log(`Part 2: ${part2()}`);
