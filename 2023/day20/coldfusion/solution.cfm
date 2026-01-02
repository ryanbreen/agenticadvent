<cfscript>
/**
 * Day 20: Pulse Propagation
 * Module communication simulation with flip-flops and conjunctions.
 */

// Read input file
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputLines = listToArray(fileRead(inputPath), chr(10));

/**
 * Parse module configuration from input
 */
function parseModules(lines) {
    var modules = {};

    for (var line in lines) {
        line = trim(line);
        if (len(line) == 0) continue;

        var parts = listToArray(line, "->");
        var namePart = trim(parts[1]);
        var destPart = trim(parts[2]);
        var destinations = listToArray(destPart, ",");

        // Trim each destination
        for (var i = 1; i <= arrayLen(destinations); i++) {
            destinations[i] = trim(destinations[i]);
        }

        if (namePart == "broadcaster") {
            modules["broadcaster"] = {
                "type": "broadcaster",
                "destinations": destinations
            };
        } else if (left(namePart, 1) == "%") {
            var name = mid(namePart, 2, len(namePart) - 1);
            modules[name] = {
                "type": "flip-flop",
                "destinations": destinations,
                "state": false
            };
        } else if (left(namePart, 1) == "&") {
            var name = mid(namePart, 2, len(namePart) - 1);
            modules[name] = {
                "type": "conjunction",
                "destinations": destinations,
                "memory": {}
            };
        }
    }

    // Initialize conjunction memory for all inputs
    for (var name in modules) {
        var module = modules[name];
        for (var dest in module.destinations) {
            if (structKeyExists(modules, dest) && modules[dest].type == "conjunction") {
                modules[dest].memory[name] = false;
            }
        }
    }

    return modules;
}

/**
 * Reset module states
 */
function resetModules(modules) {
    for (var name in modules) {
        var module = modules[name];
        if (module.type == "flip-flop") {
            module.state = false;
        } else if (module.type == "conjunction") {
            for (var key in module.memory) {
                module.memory[key] = false;
            }
        }
    }
}

/**
 * Simulate a single button press
 * Returns struct with lowCount, highCount, highSenders
 */
function simulateButtonPress(modules, watchNodes = []) {
    var lowCount = 0;
    var highCount = 0;
    var highSenders = {};

    // Simple array-based queue: [source, dest, pulse]
    var queue = [];
    arrayAppend(queue, ["button", "broadcaster", false]);
    var queueIndex = 1;

    while (queueIndex <= arrayLen(queue)) {
        var item = queue[queueIndex];
        queueIndex++;

        var source = item[1];
        var dest = item[2];
        var pulse = item[3];

        if (pulse) {
            highCount++;
        } else {
            lowCount++;
        }

        // Track if watched nodes send high pulses
        if (arrayLen(watchNodes) > 0 && arrayFind(watchNodes, source) > 0 && pulse) {
            highSenders[source] = true;
        }

        if (!structKeyExists(modules, dest)) {
            continue;
        }

        var module = modules[dest];

        if (module.type == "broadcaster") {
            for (var nextDest in module.destinations) {
                arrayAppend(queue, [dest, nextDest, pulse]);
            }
        } else if (module.type == "flip-flop") {
            if (!pulse) {
                module.state = !module.state;
                for (var nextDest in module.destinations) {
                    arrayAppend(queue, [dest, nextDest, module.state]);
                }
            }
        } else if (module.type == "conjunction") {
            module.memory[source] = pulse;
            // Send low if all inputs are high, otherwise send high
            var allHigh = true;
            for (var key in module.memory) {
                if (!module.memory[key]) {
                    allHigh = false;
                    break;
                }
            }
            var output = !allHigh;
            for (var nextDest in module.destinations) {
                arrayAppend(queue, [dest, nextDest, output]);
            }
        }
    }

    return {
        "lowCount": lowCount,
        "highCount": highCount,
        "highSenders": highSenders
    };
}

/**
 * Part 1: Count pulses after 1000 button presses
 */
function part1(modules) {
    resetModules(modules);

    var totalLow = 0;
    var totalHigh = 0;

    for (var i = 1; i <= 1000; i++) {
        var result = simulateButtonPress(modules);
        totalLow += result.lowCount;
        totalHigh += result.highCount;
    }

    return totalLow * totalHigh;
}

/**
 * Calculate GCD of two numbers
 */
function gcd(a, b) {
    while (b != 0) {
        var temp = b;
        b = a mod b;
        a = temp;
    }
    return a;
}

/**
 * Calculate LCM of two numbers using precisionEvaluate for large numbers
 */
function lcmTwo(a, b) {
    return precisionEvaluate(a / gcd(a, b) * b);
}

/**
 * Part 2: Find minimum button presses for rx to receive a low pulse
 */
function part2(modules) {
    resetModules(modules);

    // Find the module that feeds into rx
    var rxInput = "";
    for (var name in modules) {
        var module = modules[name];
        if (arrayFind(module.destinations, "rx") > 0) {
            rxInput = name;
            break;
        }
    }

    if (rxInput == "") {
        return 0;
    }

    // Find all modules that feed into rxInput
    var watchNodes = [];
    for (var key in modules[rxInput].memory) {
        arrayAppend(watchNodes, key);
    }

    var cycleLengths = {};
    var buttonPress = 0;

    while (structCount(cycleLengths) < arrayLen(watchNodes)) {
        buttonPress++;
        var result = simulateButtonPress(modules, watchNodes);

        for (var node in result.highSenders) {
            if (!structKeyExists(cycleLengths, node)) {
                cycleLengths[node] = buttonPress;
            }
        }
    }

    // Calculate LCM of all cycle lengths
    var lcmResult = 1;
    for (var node in cycleLengths) {
        lcmResult = lcmTwo(lcmResult, cycleLengths[node]);
    }

    return lcmResult;
}

// Main execution
modules = parseModules(inputLines);
writeOutput("Part 1: " & part1(modules) & chr(10));

// Re-parse for part 2 (fresh state)
modules = parseModules(inputLines);
writeOutput("Part 2: " & part2(modules) & chr(10));
</cfscript>
