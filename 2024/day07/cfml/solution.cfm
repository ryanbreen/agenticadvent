<cfscript>
// Day 7: Bridge Repair - ColdFusion Solution

function parseInput(text) {
    var equations = [];
    var lines = listToArray(trim(text), chr(10));

    for (var line in lines) {
        if (len(trim(line)) == 0) continue;

        // Split on colon
        var colonPos = find(':', line);
        var target = val(left(line, colonPos - 1));
        var numString = trim(mid(line, colonPos + 1, len(line)));

        var nums = [];
        var numParts = listToArray(numString, ' ');

        for (var num in numParts) {
            var numVal = val(trim(num));
            if (numVal != 0 || trim(num) == "0") {
                arrayAppend(nums, numVal);
            }
        }

        if (arrayLen(nums) > 0) {
            arrayAppend(equations, {target: target, nums: nums});
        }
    }

    return equations;
}

function evaluateExpression(nums, ops) {
    // Evaluate left-to-right with given operators
    var result = nums[1];

    for (var i = 1; i <= arrayLen(ops); i++) {
        var op = ops[i];
        if (op == '+') {
            result += nums[i + 1];
        } else if (op == '*') {
            result *= nums[i + 1];
        } else if (op == '||') {
            result = val(toString(result) & toString(nums[i + 1]));
        }
    }

    return result;
}

function generateOperatorCombinations(operators, count) {
    // Generate all combinations of operators
    if (count == 0) {
        return [[]];
    }

    if (count == 1) {
        var result = [];
        for (var op in operators) {
            arrayAppend(result, [op]);
        }
        return result;
    }

    var result = [];
    var smaller = generateOperatorCombinations(operators, count - 1);

    for (var combo in smaller) {
        for (var op in operators) {
            var newCombo = duplicate(combo);
            arrayAppend(newCombo, op);
            arrayAppend(result, newCombo);
        }
    }

    return result;
}

function canMakeTarget(target, nums, operators) {
    // Check if any combination of operators can produce target
    var nOps = arrayLen(nums) - 1;
    var allCombinations = generateOperatorCombinations(operators, nOps);

    for (var ops in allCombinations) {
        if (evaluateExpression(nums, ops) == target) {
            return true;
        }
    }

    return false;
}

function part1(equations) {
    var operators = ['+', '*'];
    var total = 0;

    for (var eq in equations) {
        if (canMakeTarget(eq.target, eq.nums, operators)) {
            total += eq.target;
        }
    }

    return total;
}

function part2(equations) {
    var operators = ['+', '*', '||'];
    var total = 0;

    for (var eq in equations) {
        if (canMakeTarget(eq.target, eq.nums, operators)) {
            total += eq.target;
        }
    }

    return total;
}

// Main execution
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputFile = scriptDir & '../input.txt';
var text = fileRead(inputFile);
var equations = parseInput(text);

writeOutput('Part 1: ' & part1(equations) & chr(10));
writeOutput('Part 2: ' & part2(equations) & chr(10));
</cfscript>
