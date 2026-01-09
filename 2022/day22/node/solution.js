#!/usr/bin/env node
import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(text) {
    const parts = text.split('\n\n');
    const gridLines = parts[0].split('\n');
    const path = parts[1].trim();
    
    const height = gridLines.length;
    const width = Math.max(...gridLines.map(l => l.length));
    
    // Pad lines to consistent width
    const grid = gridLines.map(line => line.padEnd(width));
    
    // Parse path
    const instructions = [];
    let i = 0;
    while (i < path.length) {
        if (/\d/.test(path[i])) {
            let j = i;
            while (j < path.length && /\d/.test(path[j])) j++;
            instructions.push(parseInt(path.slice(i, j)));
            i = j;
        } else {
            instructions.push(path[i]);
            i++;
        }
    }
    
    return { grid, instructions };
}

function part1(text) {
    const { grid, instructions } = parseInput(text);
    const height = grid.length;
    const width = grid[0].length;
    
    // Directions: 0=right, 1=down, 2=left, 3=up
    const DR = [0, 1, 0, -1];
    const DC = [1, 0, -1, 0];
    
    // Find starting position
    let row = 0;
    let col = grid[0].indexOf('.');
    let facing = 0;
    
    for (const instr of instructions) {
        if (typeof instr === 'number') {
            for (let step = 0; step < instr; step++) {
                let nr = row + DR[facing];
                let nc = col + DC[facing];
                
                // Wrap around
                if (facing === 0) { // Right
                    if (nc >= width || grid[nr][nc] === ' ') {
                        nc = 0;
                        while (grid[nr][nc] === ' ') nc++;
                    }
                } else if (facing === 2) { // Left
                    if (nc < 0 || grid[nr][nc] === ' ') {
                        nc = width - 1;
                        while (grid[nr][nc] === ' ') nc--;
                    }
                } else if (facing === 1) { // Down
                    if (nr >= height || grid[nr][nc] === ' ') {
                        nr = 0;
                        while (grid[nr][nc] === ' ') nr++;
                    }
                } else if (facing === 3) { // Up
                    if (nr < 0 || grid[nr][nc] === ' ') {
                        nr = height - 1;
                        while (grid[nr][nc] === ' ') nr--;
                    }
                }
                
                if (grid[nr][nc] === '#') break;
                row = nr;
                col = nc;
            }
        } else {
            if (instr === 'R') facing = (facing + 1) % 4;
            else facing = (facing + 3) % 4;
        }
    }
    
    return 1000 * (row + 1) + 4 * (col + 1) + facing;
}

function getCubeFaceAndLocal(row, col, S) {
    const faceRow = Math.floor(row / S);
    const faceCol = Math.floor(col / S);
    const lr = row % S;
    const lc = col % S;
    
    if (faceRow === 0 && faceCol === 1) return [1, lr, lc];
    if (faceRow === 0 && faceCol === 2) return [2, lr, lc];
    if (faceRow === 1 && faceCol === 1) return [3, lr, lc];
    if (faceRow === 2 && faceCol === 0) return [4, lr, lc];
    if (faceRow === 2 && faceCol === 1) return [5, lr, lc];
    if (faceRow === 3 && faceCol === 0) return [6, lr, lc];
    return [-1, lr, lc];
}

function wrapCube(row, col, facing, S) {
    const [face, lr, lc] = getCubeFaceAndLocal(row, col, S);
    
    if (face === 1) {
        if (facing === 3) return [3*S + lc, 0, 0];
        if (facing === 2) return [3*S - 1 - lr, 0, 0];
    } else if (face === 2) {
        if (facing === 0) return [3*S - 1 - lr, 2*S - 1, 2];
        if (facing === 1) return [S + lc, 2*S - 1, 2];
        if (facing === 3) return [4*S - 1, lc, 3];
    } else if (face === 3) {
        if (facing === 0) return [S - 1, 2*S + lr, 3];
        if (facing === 2) return [2*S, lr, 1];
    } else if (face === 4) {
        if (facing === 3) return [S + lc, S, 0];
        if (facing === 2) return [S - 1 - lr, S, 0];
    } else if (face === 5) {
        if (facing === 0) return [S - 1 - lr, 3*S - 1, 2];
        if (facing === 1) return [3*S + lc, S - 1, 2];
    } else if (face === 6) {
        if (facing === 0) return [3*S - 1, S + lr, 3];
        if (facing === 1) return [0, 2*S + lc, 1];
        if (facing === 2) return [0, S + lr, 1];
    }
    
    return [row, col, facing];
}

function part2(text) {
    const { grid, instructions } = parseInput(text);
    const height = grid.length;
    const width = grid[0].length;
    const S = height > 50 ? 50 : 4;
    
    const DR = [0, 1, 0, -1];
    const DC = [1, 0, -1, 0];
    
    let row = 0;
    let col = grid[0].indexOf('.');
    let facing = 0;
    
    for (const instr of instructions) {
        if (typeof instr === 'number') {
            for (let step = 0; step < instr; step++) {
                let nr = row + DR[facing];
                let nc = col + DC[facing];
                let nf = facing;
                
                let needWrap = false;
                if (nr < 0 || nr >= height || nc < 0 || nc >= width) {
                    needWrap = true;
                } else if (grid[nr][nc] === ' ') {
                    needWrap = true;
                }
                
                if (needWrap) {
                    [nr, nc, nf] = wrapCube(row, col, facing, S);
                }
                
                if (grid[nr][nc] === '#') break;
                row = nr;
                col = nc;
                facing = nf;
            }
        } else {
            if (instr === 'R') facing = (facing + 1) % 4;
            else facing = (facing + 3) % 4;
        }
    }
    
    return 1000 * (row + 1) + 4 * (col + 1) + facing;
}

const inputFile = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputFile, 'utf-8');

console.log('Part 1:', part1(text));
console.log('Part 2:', part2(text));
