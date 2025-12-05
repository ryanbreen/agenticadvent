<cfscript>
// Read input file
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputPath = scriptDir & "../input.txt";
var inputText = trim(fileRead(inputPath));

// Parse input - split into rules and updates sections
// Rules have "|", updates have ","
var lines = listToArray(inputText, chr(10));
var rulesSection = [];
var updatesSection = [];

for (var line in lines) {
    line = trim(line);
    if (len(line) == 0) continue;
    if (find("|", line) > 0) {
        arrayAppend(rulesSection, line);
    } else if (find(",", line) > 0) {
        arrayAppend(updatesSection, line);
    }
}

// Parse rules: X|Y means X must come before Y
// Store as: rules[X] = array of pages that must come AFTER X
var rules = {};
for (var rule in rulesSection) {
    rule = trim(rule);
    if (len(rule) == 0 || find("|", rule) == 0) continue;
    var parts = listToArray(rule, "|");
    if (arrayLen(parts) < 2) continue;
    var before = val(trim(parts[1]));
    var after = val(trim(parts[2]));

    if (!structKeyExists(rules, before)) {
        rules[before] = [];
    }
    arrayAppend(rules[before], after);
}

// Parse updates
var updates = [];
for (var line in updatesSection) {
    line = trim(line);
    var pages = listToArray(line, ",");
    var update = [];
    for (var page in pages) {
        arrayAppend(update, val(trim(page)));
    }
    arrayAppend(updates, update);
}

function isValidOrder(required array update, required struct rules) {
    // Create position map
    var pagePositions = {};
    for (var i = 1; i <= arrayLen(update); i++) {
        pagePositions[update[i]] = i;
    }

    // Check if order is valid
    for (var i = 1; i <= arrayLen(update); i++) {
        var page = update[i];

        // Check all pages that must come after this page
        if (structKeyExists(rules, page)) {
            for (var mustBeAfter in rules[page]) {
                if (structKeyExists(pagePositions, mustBeAfter)) {
                    if (pagePositions[mustBeAfter] < i) {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

function part1(required array updates, required struct rules) {
    var total = 0;
    for (var update in updates) {
        if (isValidOrder(update, rules)) {
            var middleIdx = int(arrayLen(update) / 2) + 1;
            total += update[middleIdx];
        }
    }
    return total;
}

function fixOrder(required array update, required struct rules) {
    // Sort using bubble sort with custom comparator
    var sorted = duplicate(update);

    var n = arrayLen(sorted);
    for (var i = 1; i <= n; i++) {
        for (var j = 1; j < n; j++) {
            var a = sorted[j];
            var b = sorted[j + 1];

            // If b must come before a, swap them
            var shouldSwap = false;

            if (structKeyExists(rules, b) && arrayFind(rules[b], a)) {
                shouldSwap = true;
            }

            if (shouldSwap) {
                sorted[j] = b;
                sorted[j + 1] = a;
            }
        }
    }

    return sorted;
}

function part2(required array updates, required struct rules) {
    var total = 0;
    for (var update in updates) {
        if (!isValidOrder(update, rules)) {
            var fixed = fixOrder(update, rules);
            var middleIdx = int(arrayLen(fixed) / 2) + 1;
            total += fixed[middleIdx];
        }
    }
    return total;
}

// Solve both parts
writeOutput("Part 1: " & part1(updates, rules) & chr(10));
writeOutput("Part 2: " & part2(updates, rules) & chr(10));
</cfscript>
