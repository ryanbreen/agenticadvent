import { readFileSync } from 'fs';
import { argv } from 'process';

function parseGrid(lines) {
    const galaxies = [];
    for (let r = 0; r < lines.length; r++) {
        for (let c = 0; c < lines[r].length; c++) {
            if (lines[r][c] === '#') {
                galaxies.push([r, c]);
            }
        }
    }
    return galaxies;
}

function findEmptyRowsAndCols(lines) {
    const rows = lines.length;
    const cols = lines[0]?.length || 0;

    const emptyRows = new Set();
    const emptyCols = new Set();

    // Find empty rows
    for (let r = 0; r < rows; r++) {
        if (!lines[r].includes('#')) {
            emptyRows.add(r);
        }
    }

    // Find empty columns
    for (let c = 0; c < cols; c++) {
        let hasGalaxy = false;
        for (let r = 0; r < rows; r++) {
            if (lines[r][c] === '#') {
                hasGalaxy = true;
                break;
            }
        }
        if (!hasGalaxy) {
            emptyCols.add(c);
        }
    }

    return { emptyRows, emptyCols };
}

function calculateDistances(galaxies, emptyRows, emptyCols, expansionFactor = 2) {
    let total = 0;

    for (let i = 0; i < galaxies.length; i++) {
        for (let j = i + 1; j < galaxies.length; j++) {
            const [r1, c1] = galaxies[i];
            const [r2, c2] = galaxies[j];

            // Calculate row distance with expansion
            const minR = Math.min(r1, r2);
            const maxR = Math.max(r1, r2);
            let rowDist = maxR - minR;
            for (let r = minR; r < maxR; r++) {
                if (emptyRows.has(r)) {
                    rowDist += expansionFactor - 1;
                }
            }

            // Calculate column distance with expansion
            const minC = Math.min(c1, c2);
            const maxC = Math.max(c1, c2);
            let colDist = maxC - minC;
            for (let c = minC; c < maxC; c++) {
                if (emptyCols.has(c)) {
                    colDist += expansionFactor - 1;
                }
            }

            total += rowDist + colDist;
        }
    }

    return total;
}

function part1(lines) {
    const galaxies = parseGrid(lines);
    const { emptyRows, emptyCols } = findEmptyRowsAndCols(lines);
    return calculateDistances(galaxies, emptyRows, emptyCols, 2);
}

function part2(lines) {
    const galaxies = parseGrid(lines);
    const { emptyRows, emptyCols } = findEmptyRowsAndCols(lines);
    return calculateDistances(galaxies, emptyRows, emptyCols, 1000000);
}

function main() {
    const inputFile = argv[2] || '../input.txt';
    const content = readFileSync(inputFile, 'utf-8');
    const lines = content.split('\n').filter(line => line.length > 0);

    console.log('Part 1:', part1(lines));
    console.log('Part 2:', part2(lines));
}

main();
