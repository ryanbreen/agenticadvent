#!/usr/bin/env bash

# Day 19: Not Enough Minerals
# Robot factory optimization - Uses AWK for heavy computation

cd "$(dirname "$0")"

# Parse input and run computation using AWK
awk '
BEGIN {
    bp_count = 0
}

/Blueprint/ {
    # Parse using field splitting - extract numbers from known positions
    gsub(/[^0-9 ]/, " ")  # Replace non-digits with spaces
    n = split($0, nums)
    bp_id[bp_count] = nums[1]
    oreOre[bp_count] = nums[2]
    clayOre[bp_count] = nums[3]
    obsOre[bp_count] = nums[4]
    obsClay[bp_count] = nums[5]
    geoOre[bp_count] = nums[6]
    geoObs[bp_count] = nums[7]
    bp_count++
}

function max(a, b) {
    return a > b ? a : b
}

function min(a, b) {
    return a < b ? a : b
}

# State encoding: pack values into a single string key
function makeKey(t, o, c, ob, oR, cR, obR, gR) {
    return t "," o "," c "," ob "," oR "," cR "," obR "," gR
}

function maxGeodes(idx, timeLimit,
    _oreOre, _clayOre, _obsOre, _obsClay, _geoOre, _geoObs,
    maxOre, maxClay, maxObs, best, stack, stack_size, seen, i,
    state, time, ore, clay, obs, geodes, oreR, clayR, obsR, geoR,
    remaining, upperBound, cappedOre, cappedClay, cappedObs, key,
    newOre, newClay, newObs, newGeodes, nextTime, s) {

    _oreOre = oreOre[idx]
    _clayOre = clayOre[idx]
    _obsOre = obsOre[idx]
    _obsClay = obsClay[idx]
    _geoOre = geoOre[idx]
    _geoObs = geoObs[idx]

    maxOre = max(max(max(_oreOre, _clayOre), _obsOre), _geoOre)
    maxClay = _obsClay
    maxObs = _geoObs

    best = 0
    delete seen
    delete stack

    # Initial state: time=0 ore=0 clay=0 obs=0 geodes=0 oreR=1 clayR=0 obsR=0 geoR=0
    stack[0] = "0 0 0 0 0 1 0 0 0"
    stack_size = 1

    while (stack_size > 0) {
        stack_size--
        state = stack[stack_size]
        split(state, s, " ")
        time = s[1]+0; ore = s[2]+0; clay = s[3]+0; obs = s[4]+0; geodes = s[5]+0
        oreR = s[6]+0; clayR = s[7]+0; obsR = s[8]+0; geoR = s[9]+0

        remaining = timeLimit - time

        # Upper bound pruning
        upperBound = geodes + geoR * remaining + int((remaining * (remaining - 1)) / 2)
        if (upperBound <= best) continue

        # Base case
        if (time == timeLimit) {
            if (geodes > best) best = geodes
            continue
        }

        # Cap resources
        cappedOre = min(ore, remaining * maxOre)
        cappedClay = min(clay, remaining * maxClay)
        cappedObs = min(obs, remaining * maxObs)

        # State key for memoization
        key = makeKey(time, cappedOre, cappedClay, cappedObs, oreR, clayR, obsR, geoR)
        if (key in seen && seen[key] >= geodes) continue
        seen[key] = geodes

        # Collect resources
        newOre = cappedOre + oreR
        newClay = cappedClay + clayR
        newObs = cappedObs + obsR
        newGeodes = geodes + geoR
        nextTime = time + 1

        # If we can build a geode robot, always do it
        if (cappedOre >= _geoOre && cappedObs >= _geoObs) {
            stack[stack_size++] = nextTime " " (newOre - _geoOre) " " newClay " " (newObs - _geoObs) " " newGeodes " " oreR " " clayR " " obsR " " (geoR + 1)
            continue
        }

        # Do nothing (wait)
        stack[stack_size++] = nextTime " " newOre " " newClay " " newObs " " newGeodes " " oreR " " clayR " " obsR " " geoR

        # Try building ore robot
        if (cappedOre >= _oreOre && oreR < maxOre) {
            stack[stack_size++] = nextTime " " (newOre - _oreOre) " " newClay " " newObs " " newGeodes " " (oreR + 1) " " clayR " " obsR " " geoR
        }

        # Try building clay robot
        if (cappedOre >= _clayOre && clayR < maxClay) {
            stack[stack_size++] = nextTime " " (newOre - _clayOre) " " newClay " " newObs " " newGeodes " " oreR " " (clayR + 1) " " obsR " " geoR
        }

        # Try building obsidian robot
        if (cappedOre >= _obsOre && cappedClay >= _obsClay && obsR < maxObs) {
            stack[stack_size++] = nextTime " " (newOre - _obsOre) " " (newClay - _obsClay) " " newObs " " newGeodes " " oreR " " clayR " " (obsR + 1) " " geoR
        }
    }

    return best
}

END {
    # Part 1: Sum of quality levels for all blueprints, 24 minutes
    total = 0
    for (i = 0; i < bp_count; i++) {
        geodes = maxGeodes(i, 24)
        total += bp_id[i] * geodes
    }
    print "Part 1:", total

    # Part 2: Product of max geodes for first 3 blueprints, 32 minutes
    limit = bp_count < 3 ? bp_count : 3
    result = 1
    for (i = 0; i < limit; i++) {
        geodes = maxGeodes(i, 32)
        result *= geodes
    }
    print "Part 2:", result
}
' ../input.txt
