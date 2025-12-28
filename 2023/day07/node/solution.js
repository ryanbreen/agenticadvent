import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf-8').trim();

// Named constants for hand types
const HAND_TYPE = {
  HIGH_CARD: 0,
  ONE_PAIR: 1,
  TWO_PAIR: 2,
  THREE_OF_A_KIND: 3,
  FULL_HOUSE: 4,
  FOUR_OF_A_KIND: 5,
  FIVE_OF_A_KIND: 6
};

// Card strength order (higher index = stronger)
const CARD_STRENGTH = '23456789TJQKA';
const CARD_STRENGTH_JOKER = 'J23456789TQKA'; // J is weakest in Part 2

// Parse input once, shared between part1 and part2
const hands = input.split('\n').map(line => {
  const [hand, bid] = line.split(' ');
  return { hand, bid: parseInt(bid) };
});

// Shared helper to classify hand type from sorted counts
function classifyFromCounts(sorted) {
  if (sorted[0] === 5) return HAND_TYPE.FIVE_OF_A_KIND;
  if (sorted[0] === 4) return HAND_TYPE.FOUR_OF_A_KIND;
  if (sorted[0] === 3 && sorted[1] === 2) return HAND_TYPE.FULL_HOUSE;
  if (sorted[0] === 3) return HAND_TYPE.THREE_OF_A_KIND;
  if (sorted[0] === 2 && sorted[1] === 2) return HAND_TYPE.TWO_PAIR;
  if (sorted[0] === 2) return HAND_TYPE.ONE_PAIR;
  return HAND_TYPE.HIGH_CARD;
}

function getHandType(hand) {
  const counts = {};
  for (const c of hand) {
    counts[c] = (counts[c] || 0) + 1;
  }
  const sorted = Object.values(counts).sort((a, b) => b - a);
  return classifyFromCounts(sorted);
}

function getHandTypeWithJokers(hand) {
  const jokerCount = [...hand].filter(c => c === 'J').length;
  if (jokerCount === 0) return getHandType(hand);
  if (jokerCount === 5) return HAND_TYPE.FIVE_OF_A_KIND;

  // Count non-joker cards
  const counts = {};
  for (const c of hand) {
    if (c !== 'J') counts[c] = (counts[c] || 0) + 1;
  }
  const sorted = Object.values(counts).sort((a, b) => b - a);

  // Add jokers to the highest count
  sorted[0] += jokerCount;

  return classifyFromCounts(sorted);
}

function handKey(hand) {
  const handType = getHandType(hand);
  const cardValues = [...hand].map(c => CARD_STRENGTH.indexOf(c));
  return [handType, ...cardValues];
}

function handKeyWithJokers(hand) {
  const handType = getHandTypeWithJokers(hand);
  const cardValues = [...hand].map(c => CARD_STRENGTH_JOKER.indexOf(c));
  return [handType, ...cardValues];
}

function compareHands(a, b) {
  const keyA = handKey(a);
  const keyB = handKey(b);
  for (let i = 0; i < keyA.length; i++) {
    if (keyA[i] !== keyB[i]) return keyA[i] - keyB[i];
  }
  return 0;
}

function compareHandsWithJokers(a, b) {
  const keyA = handKeyWithJokers(a);
  const keyB = handKeyWithJokers(b);
  for (let i = 0; i < keyA.length; i++) {
    if (keyA[i] !== keyB[i]) return keyA[i] - keyB[i];
  }
  return 0;
}

// Calculate total winnings using reduce
function calculateWinnings(sortedHands) {
  return sortedHands.reduce((total, hand, index) => total + (index + 1) * hand.bid, 0);
}

// Part 1
function part1() {
  const sortedHands = [...hands].sort((a, b) => compareHands(a.hand, b.hand));
  return calculateWinnings(sortedHands);
}

// Part 2
function part2() {
  const sortedHands = [...hands].sort((a, b) => compareHandsWithJokers(a.hand, b.hand));
  return calculateWinnings(sortedHands);
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
