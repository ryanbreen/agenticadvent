import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(filename) {
  return readFileSync(filename, 'utf-8')
    .trim()
    .split('\n')
    .map(line => line.split(' '));
}

function part1(rounds) {
  // X=Rock, Y=Paper, Z=Scissors
  const shapeScore = { X: 1, Y: 2, Z: 3 };

  // Outcome: 0=loss, 3=draw, 6=win
  const outcomes = {
    'A X': 3, 'A Y': 6, 'A Z': 0,  // Rock vs ...
    'B X': 0, 'B Y': 3, 'B Z': 6,  // Paper vs ...
    'C X': 6, 'C Y': 0, 'C Z': 3,  // Scissors vs ...
  };

  return rounds.reduce((total, [opp, me]) => {
    return total + shapeScore[me] + outcomes[`${opp} ${me}`];
  }, 0);
}

function part2(rounds) {
  // X=lose, Y=draw, Z=win
  // Returns shape score (1=Rock, 2=Paper, 3=Scissors)
  const choices = {
    'A X': 3, 'A Y': 1, 'A Z': 2,  // vs Rock: lose->Scissors, draw->Rock, win->Paper
    'B X': 1, 'B Y': 2, 'B Z': 3,  // vs Paper: lose->Rock, draw->Paper, win->Scissors
    'C X': 2, 'C Y': 3, 'C Z': 1,  // vs Scissors: lose->Paper, draw->Scissors, win->Rock
  };

  const outcomeScore = { X: 0, Y: 3, Z: 6 };

  return rounds.reduce((total, [opp, outcome]) => {
    return total + choices[`${opp} ${outcome}`] + outcomeScore[outcome];
  }, 0);
}

const inputFile = join(__dirname, '..', 'input.txt');
const rounds = parseInput(inputFile);

console.log('Part 1:', part1(rounds));
console.log('Part 2:', part2(rounds));
