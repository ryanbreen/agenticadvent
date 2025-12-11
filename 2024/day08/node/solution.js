import { readFileSync } from 'fs';

function parseInput(filename) {
    const grid = readFileSync(filename, 'utf-8').trim().split('\n');
    const rows = grid.length;
    const cols = grid[0]?.length ?? 0;

    // Group antenna positions by frequency
    const antennas = new Map();
    grid.forEach((row, r) => {
        [...row].forEach((ch, c) => {
            if (ch !== '.') {
                if (!antennas.has(ch)) {
                    antennas.set(ch, []);
                }
                antennas.get(ch).push([r, c]);
            }
        });
    });

    return { rows, cols, antennas };
}

function part1() {
    const { rows, cols, antennas } = parseInput('../input.txt');
    const antinodes = new Set();

    for (const positions of antennas.values()) {
        // For each pair of antennas with same frequency
        for (let i = 0; i < positions.length; i++) {
            for (let j = i + 1; j < positions.length; j++) {
                const [r1, c1] = positions[i];
                const [r2, c2] = positions[j];

                // Calculate the two antinodes (2:1 distance ratio)
                // Antinode beyond antenna 1 (away from antenna 2)
                const ar1 = 2 * r1 - r2;
                const ac1 = 2 * c1 - c2;
                // Antinode beyond antenna 2 (away from antenna 1)
                const ar2 = 2 * r2 - r1;
                const ac2 = 2 * c2 - c1;

                // Add if within bounds
                if (ar1 >= 0 && ar1 < rows && ac1 >= 0 && ac1 < cols) {
                    antinodes.add(`${ar1},${ac1}`);
                }
                if (ar2 >= 0 && ar2 < rows && ac2 >= 0 && ac2 < cols) {
                    antinodes.add(`${ar2},${ac2}`);
                }
            }
        }
    }

    return antinodes.size;
}

function part2() {
    const { rows, cols, antennas } = parseInput('../input.txt');
    const antinodes = new Set();

    for (const positions of antennas.values()) {
        // For each pair of antennas with same frequency
        for (let i = 0; i < positions.length; i++) {
            for (let j = i + 1; j < positions.length; j++) {
                const [r1, c1] = positions[i];
                const [r2, c2] = positions[j];
                const dr = r2 - r1;
                const dc = c2 - c1;

                // Extend in both directions along the line
                // Direction 1: from antenna 1 towards and beyond antenna 2
                let r = r1, c = c1;
                while (r >= 0 && r < rows && c >= 0 && c < cols) {
                    antinodes.add(`${r},${c}`);
                    r += dr;
                    c += dc;
                }

                // Direction 2: from antenna 1 away from antenna 2
                r = r1 - dr;
                c = c1 - dc;
                while (r >= 0 && r < rows && c >= 0 && c < cols) {
                    antinodes.add(`${r},${c}`);
                    r -= dr;
                    c -= dc;
                }
            }
        }
    }

    return antinodes.size;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
