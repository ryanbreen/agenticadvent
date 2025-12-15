import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

function parseInput(text) {
    const parts = text.split('\n\n');
    const gridLines = parts[0].split('\n');
    const grid = gridLines.map(line => line.split(''));
    const moves = parts[1].replace(/\n/g, '');
    return { grid, moves };
}

function findRobot(grid) {
    for (let r = 0; r < grid.length; r++) {
        for (let c = 0; c < grid[r].length; c++) {
            if (grid[r][c] === '@') {
                return { r, c };
            }
        }
    }
    return null;
}

function moveRobot(grid, robotPos, direction) {
    const deltas = {
        '<': { dr: 0, dc: -1 },
        '>': { dr: 0, dc: 1 },
        '^': { dr: -1, dc: 0 },
        'v': { dr: 1, dc: 0 }
    };

    const { dr, dc } = deltas[direction];
    const { r, c } = robotPos;
    const nr = r + dr;
    const nc = c + dc;

    if (grid[nr][nc] === '#') {
        return robotPos;
    }

    if (grid[nr][nc] === '.') {
        grid[r][c] = '.';
        grid[nr][nc] = '@';
        return { r: nr, c: nc };
    }

    if (grid[nr][nc] === 'O') {
        let checkR = nr;
        let checkC = nc;
        while (grid[checkR][checkC] === 'O') {
            checkR += dr;
            checkC += dc;
        }

        if (grid[checkR][checkC] === '#') {
            return robotPos;
        }

        grid[checkR][checkC] = 'O';
        grid[r][c] = '.';
        grid[nr][nc] = '@';
        return { r: nr, c: nc };
    }

    return robotPos;
}

function calculateGPS(grid, boxChar = 'O') {
    let total = 0;
    for (let r = 0; r < grid.length; r++) {
        for (let c = 0; c < grid[r].length; c++) {
            if (grid[r][c] === boxChar) {
                total += 100 * r + c;
            }
        }
    }
    return total;
}

function part1() {
    const { grid, moves } = parseInput(input);
    let robotPos = findRobot(grid);

    for (const move of moves) {
        robotPos = moveRobot(grid, robotPos, move);
    }

    return calculateGPS(grid);
}

function scaleGrid(grid) {
    const newGrid = [];
    for (const row of grid) {
        const newRow = [];
        for (const cell of row) {
            if (cell === '#') {
                newRow.push('#', '#');
            } else if (cell === 'O') {
                newRow.push('[', ']');
            } else if (cell === '.') {
                newRow.push('.', '.');
            } else if (cell === '@') {
                newRow.push('@', '.');
            }
        }
        newGrid.push(newRow);
    }
    return newGrid;
}

function canMoveBoxVertical(grid, boxLeftC, r, dr) {
    const nr = r + dr;
    const leftC = boxLeftC;
    const rightC = boxLeftC + 1;

    const leftTarget = grid[nr][leftC];
    const rightTarget = grid[nr][rightC];

    if (leftTarget === '#' || rightTarget === '#') {
        return false;
    }

    const boxesToCheck = new Set();

    if (leftTarget === '[') {
        boxesToCheck.add(`${nr},${leftC}`);
    } else if (leftTarget === ']') {
        boxesToCheck.add(`${nr},${leftC - 1}`);
    }

    if (rightTarget === '[') {
        boxesToCheck.add(`${nr},${rightC}`);
    } else if (rightTarget === ']') {
        boxesToCheck.add(`${nr},${rightC - 1}`);
    }

    for (const boxKey of boxesToCheck) {
        const [boxR, boxC] = boxKey.split(',').map(Number);
        if (!canMoveBoxVertical(grid, boxC, boxR, dr)) {
            return false;
        }
    }

    return true;
}

function collectBoxesVertical(grid, boxLeftC, r, dr, collected) {
    const key = `${r},${boxLeftC}`;
    collected.add(key);

    const nr = r + dr;
    const leftC = boxLeftC;
    const rightC = boxLeftC + 1;

    const leftTarget = grid[nr][leftC];
    const rightTarget = grid[nr][rightC];

    const boxesToCheck = new Set();

    if (leftTarget === '[') {
        boxesToCheck.add(`${nr},${leftC}`);
    } else if (leftTarget === ']') {
        boxesToCheck.add(`${nr},${leftC - 1}`);
    }

    if (rightTarget === '[') {
        boxesToCheck.add(`${nr},${rightC}`);
    } else if (rightTarget === ']') {
        boxesToCheck.add(`${nr},${rightC - 1}`);
    }

    for (const boxKey of boxesToCheck) {
        if (!collected.has(boxKey)) {
            const [boxR, boxC] = boxKey.split(',').map(Number);
            collectBoxesVertical(grid, boxC, boxR, dr, collected);
        }
    }
}

function moveRobotWide(grid, robotPos, direction) {
    const deltas = {
        '<': { dr: 0, dc: -1 },
        '>': { dr: 0, dc: 1 },
        '^': { dr: -1, dc: 0 },
        'v': { dr: 1, dc: 0 }
    };

    const { dr, dc } = deltas[direction];
    const { r, c } = robotPos;
    const nr = r + dr;
    const nc = c + dc;

    const target = grid[nr][nc];

    if (target === '#') {
        return robotPos;
    }

    if (target === '.') {
        grid[r][c] = '.';
        grid[nr][nc] = '@';
        return { r: nr, c: nc };
    }

    if (target === '[' || target === ']') {
        if (dc !== 0) {
            // Horizontal movement
            let checkC = nc;
            while (grid[r][checkC] === '[' || grid[r][checkC] === ']') {
                checkC += dc;
            }

            if (grid[r][checkC] === '#') {
                return robotPos;
            }

            // Shift all boxes
            if (dc > 0) {
                // Moving right
                for (let col = checkC; col > nc; col--) {
                    grid[r][col] = grid[r][col - 1];
                }
            } else {
                // Moving left
                for (let col = checkC; col < nc; col++) {
                    grid[r][col] = grid[r][col + 1];
                }
            }

            grid[r][c] = '.';
            grid[nr][nc] = '@';
            return { r: nr, c: nc };
        } else {
            // Vertical movement
            const boxLeftC = target === '[' ? nc : nc - 1;

            if (!canMoveBoxVertical(grid, boxLeftC, nr, dr)) {
                return robotPos;
            }

            const boxesToMove = new Set();
            collectBoxesVertical(grid, boxLeftC, nr, dr, boxesToMove);

            // Convert to array and sort by row
            const sortedBoxes = Array.from(boxesToMove)
                .map(key => {
                    const [r, c] = key.split(',').map(Number);
                    return { r, c };
                })
                .sort((a, b) => dr > 0 ? b.r - a.r : a.r - b.r);

            // Move all boxes
            for (const box of sortedBoxes) {
                grid[box.r][box.c] = '.';
                grid[box.r][box.c + 1] = '.';
                grid[box.r + dr][box.c] = '[';
                grid[box.r + dr][box.c + 1] = ']';
            }

            // Move robot
            grid[r][c] = '.';
            grid[nr][nc] = '@';
            return { r: nr, c: nc };
        }
    }

    return robotPos;
}

function part2() {
    let { grid, moves } = parseInput(input);
    grid = scaleGrid(grid);
    let robotPos = findRobot(grid);

    for (const move of moves) {
        robotPos = moveRobotWide(grid, robotPos, move);
    }

    return calculateGPS(grid, '[');
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
