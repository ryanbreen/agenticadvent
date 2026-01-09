component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Parse input into monkeys struct
        var monkeys = parseInput(inputText);

        // Solve both parts
        var part1Result = part1(monkeys);
        var part2Result = part2(monkeys);

        systemOutput("Part 1: " & part1Result, true);
        systemOutput("Part 2: " & part2Result, true);
    }

    function parseInput(text) {
        var monkeys = {};
        var lines = listToArray(text, chr(10));

        for (var line in lines) {
            line = trim(line);
            if (len(line) == 0) continue;

            var parts = listToArray(line, ":");
            var name = trim(parts[1]);
            var job = trim(parts[2]);

            var jobParts = listToArray(job, " ");

            if (arrayLen(jobParts) == 1) {
                // It's a number - store as simple string with N: prefix
                monkeys[name] = "N:" & jobParts[1];
            } else {
                // It's an operation: left op right - store as O:left:op:right
                monkeys[name] = "O:" & jobParts[1] & ":" & jobParts[2] & ":" & jobParts[3];
            }
        }

        return monkeys;
    }

    function evalMonkey(monkeys, name, memo) {
        if (structKeyExists(memo, name)) {
            return memo[name];
        }

        var monkey = monkeys[name];

        if (left(monkey, 2) == "N:") {
            // It's a number
            var result = val(mid(monkey, 3, len(monkey) - 2));
            memo[name] = result;
            return result;
        }

        // It's an operation: O:left:op:right
        var parts = listToArray(mid(monkey, 3, len(monkey) - 2), ":");
        var leftName = parts[1];
        var op = parts[2];
        var rightName = parts[3];

        var leftVal = evalMonkey(monkeys, leftName, memo);
        var rightVal = evalMonkey(monkeys, rightName, memo);
        var result = 0;

        if (op == "+") {
            result = leftVal + rightVal;
        } else if (op == "-") {
            result = leftVal - rightVal;
        } else if (op == "*") {
            result = leftVal * rightVal;
        } else if (op == "/") {
            result = int(leftVal / rightVal);
        }

        memo[name] = result;
        return result;
    }

    function containsHumn(monkeys, name, memo) {
        if (structKeyExists(memo, name)) {
            return memo[name];
        }

        if (name == "humn") {
            memo[name] = true;
            return true;
        }

        var monkey = monkeys[name];

        if (left(monkey, 2) == "N:") {
            memo[name] = false;
            return false;
        }

        // It's an operation: O:left:op:right
        var parts = listToArray(mid(monkey, 3, len(monkey) - 2), ":");
        var leftName = parts[1];
        var rightName = parts[3];

        var result = containsHumn(monkeys, leftName, memo) || containsHumn(monkeys, rightName, memo);
        memo[name] = result;
        return result;
    }

    function solveForHumn(monkeys, name, target) {
        if (name == "humn") {
            return target;
        }

        var monkey = monkeys[name];

        if (left(monkey, 2) == "N:") {
            return -1; // Should not happen
        }

        // It's an operation: O:left:op:right
        var parts = listToArray(mid(monkey, 3, len(monkey) - 2), ":");
        var leftName = parts[1];
        var op = parts[2];
        var rightName = parts[3];

        var humnMemo = {};
        var leftHasHumn = containsHumn(monkeys, leftName, humnMemo);
        var evalMemo = {};

        if (leftHasHumn) {
            // Evaluate right side
            var rightVal = evalMonkey(monkeys, rightName, evalMemo);
            var newTarget = 0;

            if (op == "+") {
                // left + right = target => left = target - right
                newTarget = target - rightVal;
            } else if (op == "-") {
                // left - right = target => left = target + right
                newTarget = target + rightVal;
            } else if (op == "*") {
                // left * right = target => left = target / right
                newTarget = int(target / rightVal);
            } else if (op == "/") {
                // left / right = target => left = target * right
                newTarget = target * rightVal;
            }

            return solveForHumn(monkeys, leftName, newTarget);
        } else {
            // Evaluate left side
            var leftVal = evalMonkey(monkeys, leftName, evalMemo);
            var newTarget = 0;

            if (op == "+") {
                // left + right = target => right = target - left
                newTarget = target - leftVal;
            } else if (op == "-") {
                // left - right = target => right = left - target
                newTarget = leftVal - target;
            } else if (op == "*") {
                // left * right = target => right = target / left
                newTarget = int(target / leftVal);
            } else if (op == "/") {
                // left / right = target => right = left / target
                newTarget = int(leftVal / target);
            }

            return solveForHumn(monkeys, rightName, newTarget);
        }
    }

    function part1(monkeys) {
        var memo = {};
        return evalMonkey(monkeys, "root", memo);
    }

    function part2(monkeys) {
        var monkey = monkeys["root"];

        // It's an operation: O:left:op:right
        var parts = listToArray(mid(monkey, 3, len(monkey) - 2), ":");
        var leftName = parts[1];
        var rightName = parts[3];

        var humnMemo = {};
        var leftHasHumn = containsHumn(monkeys, leftName, humnMemo);
        var evalMemo = {};

        if (leftHasHumn) {
            // Right side gives us the target value
            var target = evalMonkey(monkeys, rightName, evalMemo);
            return solveForHumn(monkeys, leftName, target);
        } else {
            // Left side gives us the target value
            var target = evalMonkey(monkeys, leftName, evalMemo);
            return solveForHumn(monkeys, rightName, target);
        }
    }
}
