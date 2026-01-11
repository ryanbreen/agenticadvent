import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

function parseInput() {
  const sections = input.split('\n\n');
  const numbers = sections[0].split(',').map(Number);

  const boards = [];
  for (let i = 1; i < sections.length; i++) {
    const board = sections[i].trim().split('\n').map(line =>
      line.trim().split(/\s+/).map(Number)
    );
    boards.push(board);
  }

  return { numbers, boards };
}

function checkWinner(marked) {
  // Check rows
  for (let row = 0; row < 5; row++) {
    if (marked[row].every(v => v)) return true;
  }
  // Check columns
  for (let col = 0; col < 5; col++) {
    if (marked.every(row => row[col])) return true;
  }
  return false;
}

function calculateScore(board, marked, lastNumber) {
  let unmarkedSum = 0;
  for (let row = 0; row < 5; row++) {
    for (let col = 0; col < 5; col++) {
      if (!marked[row][col]) {
        unmarkedSum += board[row][col];
      }
    }
  }
  return unmarkedSum * lastNumber;
}

function markNumber(board, marked, number) {
  for (let row = 0; row < 5; row++) {
    for (let col = 0; col < 5; col++) {
      if (board[row][col] === number) {
        marked[row][col] = true;
      }
    }
  }
}

function part1() {
  const { numbers, boards } = parseInput();
  const marked = boards.map(() =>
    Array.from({ length: 5 }, () => Array(5).fill(false))
  );

  for (const number of numbers) {
    for (let i = 0; i < boards.length; i++) {
      markNumber(boards[i], marked[i], number);
      if (checkWinner(marked[i])) {
        return calculateScore(boards[i], marked[i], number);
      }
    }
  }

  return null;
}

function part2() {
  const { numbers, boards } = parseInput();
  const marked = boards.map(() =>
    Array.from({ length: 5 }, () => Array(5).fill(false))
  );
  const won = new Array(boards.length).fill(false);
  let lastScore = null;

  for (const number of numbers) {
    for (let i = 0; i < boards.length; i++) {
      if (won[i]) continue;
      markNumber(boards[i], marked[i], number);
      if (checkWinner(marked[i])) {
        won[i] = true;
        lastScore = calculateScore(boards[i], marked[i], number);
      }
    }
  }

  return lastScore;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
