import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

const parseInput = (text) => text.trim().split('\n');

const DR = [0, 1, 0, -1];
const DC = [1, 0, -1, 0];

const countEnergized = (grid, startRow, startCol, startDir) => {
    const rows = grid.length;
    const cols = grid[0].length;
    const visited = new Set();
    const queue = [[startRow, startCol, startDir]];

    while (queue.length > 0) {
        const [r, c, d] = queue.shift();

        if (r < 0 || r >= rows || c < 0 || c >= cols) continue;

        const state = `${r},${c},${d}`;
        if (visited.has(state)) continue;
        visited.add(state);

        const cell = grid[r][c];
        let nextDirs;

        if (cell === '.') {
            nextDirs = [d];
        } else if (cell === '/') {
            nextDirs = [[3, 2, 1, 0][d]];
        } else if (cell === '\\') {
            nextDirs = [[1, 0, 3, 2][d]];
        } else if (cell === '|') {
            nextDirs = (d === 0 || d === 2) ? [1, 3] : [d];
        } else if (cell === '-') {
            nextDirs = (d === 1 || d === 3) ? [0, 2] : [d];
        }

        for (const nd of nextDirs) {
            queue.push([r + DR[nd], c + DC[nd], nd]);
        }
    }

    const positions = new Set();
    for (const state of visited) {
        const [r, c] = state.split(',');
        positions.add(`${r},${c}`);
    }
    return positions.size;
};

const part1 = (grid) => countEnergized(grid, 0, 0, 0);

const part2 = (grid) => {
    const rows = grid.length;
    const cols = grid[0].length;
    let maxEnergized = 0;

    // Top row, heading down
    for (let c = 0; c < cols; c++) {
        maxEnergized = Math.max(maxEnergized, countEnergized(grid, 0, c, 1));
    }

    // Bottom row, heading up
    for (let c = 0; c < cols; c++) {
        maxEnergized = Math.max(maxEnergized, countEnergized(grid, rows - 1, c, 3));
    }

    // Left column, heading right
    for (let r = 0; r < rows; r++) {
        maxEnergized = Math.max(maxEnergized, countEnergized(grid, r, 0, 0));
    }

    // Right column, heading left
    for (let r = 0; r < rows; r++) {
        maxEnergized = Math.max(maxEnergized, countEnergized(grid, r, cols - 1, 2));
    }

    return maxEnergized;
};

const inputFile = join(__dirname, '../input.txt');
const text = readFileSync(inputFile, 'utf-8');
const grid = parseInput(text);

console.log(`Part 1: ${part1(grid)}`);
console.log(`Part 2: ${part2(grid)}`);
