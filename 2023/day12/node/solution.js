#!/usr/bin/env node
/**
 * Advent of Code 2023 Day 12: Hot Springs
 */

import { readFileSync } from 'fs';

/**
 * Count arrangements using memoized DP.
 * @param {string} pattern - The spring pattern
 * @param {number[]} groups - The contiguous group sizes
 * @returns {number} - Number of valid arrangements
 */
function countArrangements(pattern, groups) {
    const memo = new Map();

    function dp(pos, groupIdx, currentRun) {
        const key = `${pos},${groupIdx},${currentRun}`;
        if (memo.has(key)) {
            return memo.get(key);
        }

        // Base case: reached end of pattern
        if (pos === pattern.length) {
            // Valid if we've matched all groups and no partial run
            if (groupIdx === groups.length && currentRun === 0) {
                return 1;
            }
            // Or if we're on the last group and the run matches
            if (groupIdx === groups.length - 1 && groups[groupIdx] === currentRun) {
                return 1;
            }
            return 0;
        }

        let result = 0;
        const char = pattern[pos];

        // Option 1: Place operational spring (.)
        if (char === '.' || char === '?') {
            if (currentRun === 0) {
                // No active run, just move forward
                result += dp(pos + 1, groupIdx, 0);
            } else if (groupIdx < groups.length && groups[groupIdx] === currentRun) {
                // End current run if it matches expected group size
                result += dp(pos + 1, groupIdx + 1, 0);
            }
            // Otherwise invalid (run doesn't match group)
        }

        // Option 2: Place damaged spring (#)
        if (char === '#' || char === '?') {
            if (groupIdx < groups.length && currentRun < groups[groupIdx]) {
                // Can extend current run
                result += dp(pos + 1, groupIdx, currentRun + 1);
            }
            // Otherwise invalid (exceeds group size or no more groups)
        }

        memo.set(key, result);
        return result;
    }

    return dp(0, 0, 0);
}

/**
 * Parse a line into pattern and groups.
 * @param {string} line - Input line
 * @returns {{pattern: string, groups: number[]}}
 */
function parseLine(line) {
    const [pattern, groupsStr] = line.trim().split(' ');
    const groups = groupsStr.split(',').map(Number);
    return { pattern, groups };
}

/**
 * Part 1: Sum of arrangement counts for all rows.
 * @param {string[]} lines - Input lines
 * @returns {number}
 */
function part1(lines) {
    let total = 0;
    for (const line of lines) {
        if (!line.trim()) continue;
        const { pattern, groups } = parseLine(line);
        total += countArrangements(pattern, groups);
    }
    return total;
}

/**
 * Unfold pattern and groups by repeating them 5 times.
 * @param {string} pattern
 * @param {number[]} groups
 * @returns {{pattern: string, groups: number[]}}
 */
function unfold(pattern, groups) {
    const unfoldedPattern = Array(5).fill(pattern).join('?');
    const unfoldedGroups = [];
    for (let i = 0; i < 5; i++) {
        unfoldedGroups.push(...groups);
    }
    return { pattern: unfoldedPattern, groups: unfoldedGroups };
}

/**
 * Part 2: Sum of arrangement counts for all rows after unfolding.
 * @param {string[]} lines - Input lines
 * @returns {number}
 */
function part2(lines) {
    let total = 0;
    for (const line of lines) {
        if (!line.trim()) continue;
        const parsed = parseLine(line);
        const { pattern, groups } = unfold(parsed.pattern, parsed.groups);
        total += countArrangements(pattern, groups);
    }
    return total;
}

function main() {
    const input = readFileSync('../input.txt', 'utf-8');
    const lines = input.split('\n');

    console.log(`Part 1: ${part1(lines)}`);
    console.log(`Part 2: ${part2(lines)}`);
}

main();
