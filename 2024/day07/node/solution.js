import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const input = readFileSync(join(__dirname, '..', 'input.txt'), 'utf8').trim();

function parseInput(text) {
  return text.split('\n').map(line => {
    const [target, nums] = line.split(': ');
    return {
      target: BigInt(target),
      nums: nums.split(' ').map(n => BigInt(n))
    };
  });
}

function evaluate(nums, ops) {
  let result = nums[0];
  for (let i = 0; i < ops.length; i++) {
    if (ops[i] === '+') {
      result += nums[i + 1];
    } else if (ops[i] === '*') {
      result *= nums[i + 1];
    } else if (ops[i] === '||') {
      result = BigInt(result.toString() + nums[i + 1].toString());
    }
  }
  return result;
}

function* generateCombinations(operators, length) {
  if (length === 0) {
    yield [];
    return;
  }
  for (const combo of generateCombinations(operators, length - 1)) {
    for (const op of operators) {
      yield [...combo, op];
    }
  }
}

function canMakeTarget(target, nums, operators) {
  const nOps = nums.length - 1;
  for (const ops of generateCombinations(operators, nOps)) {
    if (evaluate(nums, ops) === target) {
      return true;
    }
  }
  return false;
}

function part1(equations) {
  const operators = ['+', '*'];
  let total = 0n;
  for (const { target, nums } of equations) {
    if (canMakeTarget(target, nums, operators)) {
      total += target;
    }
  }
  return total;
}

function part2(equations) {
  const operators = ['+', '*', '||'];
  let total = 0n;
  for (const { target, nums } of equations) {
    if (canMakeTarget(target, nums, operators)) {
      total += target;
    }
  }
  return total;
}

const equations = parseInput(input);
console.log('Part 1:', part1(equations).toString());
console.log('Part 2:', part2(equations).toString());
