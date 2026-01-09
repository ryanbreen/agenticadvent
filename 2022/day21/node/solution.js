#!/usr/bin/env node
import { readFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

function parseInput(text) {
    const monkeys = new Map();
    for (const line of text.trim().split('\n')) {
        const [name, job] = line.split(': ');
        const parts = job.split(' ');
        if (parts.length === 1) {
            monkeys.set(name, BigInt(parts[0]));
        } else {
            monkeys.set(name, [parts[0], parts[1], parts[2]]);
        }
    }
    return monkeys;
}

function evaluate(monkeys, name, memo = new Map()) {
    if (memo.has(name)) return memo.get(name);
    
    const job = monkeys.get(name);
    if (typeof job === 'bigint') return job;
    
    const [left, op, right] = job;
    const leftVal = evaluate(monkeys, left, memo);
    const rightVal = evaluate(monkeys, right, memo);
    
    let result;
    if (op === '+') result = leftVal + rightVal;
    else if (op === '-') result = leftVal - rightVal;
    else if (op === '*') result = leftVal * rightVal;
    else if (op === '/') result = leftVal / rightVal;
    
    memo.set(name, result);
    return result;
}

function containsHumn(monkeys, name, memo = new Map()) {
    if (memo.has(name)) return memo.get(name);
    if (name === 'humn') return true;
    
    const job = monkeys.get(name);
    if (typeof job === 'bigint') {
        memo.set(name, false);
        return false;
    }
    
    const [left, , right] = job;
    const result = containsHumn(monkeys, left, memo) || containsHumn(monkeys, right, memo);
    memo.set(name, result);
    return result;
}

function solveForHumn(monkeys, name, target) {
    if (name === 'humn') return target;
    
    const job = monkeys.get(name);
    if (typeof job === 'bigint') return null;
    
    const [left, op, right] = job;
    const leftHasHumn = containsHumn(monkeys, left);
    
    if (leftHasHumn) {
        const rightVal = evaluate(monkeys, right);
        let newTarget;
        if (op === '+') newTarget = target - rightVal;
        else if (op === '-') newTarget = target + rightVal;
        else if (op === '*') newTarget = target / rightVal;
        else if (op === '/') newTarget = target * rightVal;
        return solveForHumn(monkeys, left, newTarget);
    } else {
        const leftVal = evaluate(monkeys, left);
        let newTarget;
        if (op === '+') newTarget = target - leftVal;
        else if (op === '-') newTarget = leftVal - target;
        else if (op === '*') newTarget = target / leftVal;
        else if (op === '/') newTarget = leftVal / target;
        return solveForHumn(monkeys, right, newTarget);
    }
}

function part1(text) {
    const monkeys = parseInput(text);
    return evaluate(monkeys, 'root');
}

function part2(text) {
    const monkeys = parseInput(text);
    
    const [left, , right] = monkeys.get('root');
    const leftHasHumn = containsHumn(monkeys, left);
    
    if (leftHasHumn) {
        const target = evaluate(monkeys, right);
        return solveForHumn(monkeys, left, target);
    } else {
        const target = evaluate(monkeys, left);
        return solveForHumn(monkeys, right, target);
    }
}

const inputFile = join(__dirname, '..', 'input.txt');
const text = readFileSync(inputFile, 'utf-8');

console.log('Part 1:', part1(text).toString());
console.log('Part 2:', part2(text).toString());
