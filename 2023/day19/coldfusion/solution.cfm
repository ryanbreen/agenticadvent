<cfscript>
/**
 * Day 19: Aplenty - Workflow processing and range analysis
 *
 * Part 1: Parse workflows and parts, process each part through workflows
 *         starting from "in", sum x+m+a+s for accepted parts.
 * Part 2: Count all combinations of x,m,a,s values (1-4000) leading to acceptance
 *         using range splitting.
 */

// Read input file
// Get the directory of the current script
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputText = fileRead(inputPath).trim();

// Split into workflows and parts sections by finding double newline
// Use includeEmptyFields=true to preserve empty lines
blankPos = find(chr(10) & chr(10), inputText);
workflowSection = left(inputText, blankPos - 1);
partsSection = mid(inputText, blankPos + 2, len(inputText) - blankPos - 1);

// Parse workflows
workflows = {};
for (line in listToArray(workflowSection, chr(10))) {
    if (trim(line) == "") continue;

    // Format: name{rules...}
    bracePos = find("{", line);
    name = left(line, bracePos - 1);
    rulesStr = mid(line, bracePos + 1, len(line) - bracePos - 1);

    rules = [];
    for (ruleStr in listToArray(rulesStr, ",")) {
        if (find(":", ruleStr)) {
            // Conditional rule: attr<|>value:destination
            colonPos = find(":", ruleStr);
            condition = left(ruleStr, colonPos - 1);
            destination = mid(ruleStr, colonPos + 1, len(ruleStr) - colonPos);

            attr = left(condition, 1);
            op = mid(condition, 2, 1);
            value = int(mid(condition, 3, len(condition) - 2));

            arrayAppend(rules, {
                attr: attr,
                op: op,
                value: value,
                destination: destination
            });
        } else {
            // Default rule: just destination
            arrayAppend(rules, {
                attr: "",
                op: "",
                value: 0,
                destination: ruleStr
            });
        }
    }
    workflows[name] = rules;
}

// Parse parts
parts = [];
for (line in listToArray(partsSection, chr(10))) {
    if (trim(line) == "") continue;

    // Format: {x=...,m=...,a=...,s=...}
    part = {};
    // Remove braces
    innerStr = mid(line, 2, len(line) - 2);
    for (assignment in listToArray(innerStr, ",")) {
        eqPos = find("=", assignment);
        attrName = left(assignment, eqPos - 1);
        attrValue = int(mid(assignment, eqPos + 1, len(assignment) - eqPos));
        part[attrName] = attrValue;
    }
    arrayAppend(parts, part);
}

/**
 * Process a part through workflows and return true if accepted
 */
function processPart(required struct workflows, required struct part) {
    var current = "in";

    while (current != "A" && current != "R") {
        var rules = workflows[current];
        for (var rule in rules) {
            if (rule.attr == "") {
                // Default rule
                current = rule.destination;
                break;
            } else {
                var partValue = part[rule.attr];
                var matched = false;

                if (rule.op == "<" && partValue < rule.value) {
                    matched = true;
                } else if (rule.op == ">" && partValue > rule.value) {
                    matched = true;
                }

                if (matched) {
                    current = rule.destination;
                    break;
                }
            }
        }
    }

    return current == "A";
}

/**
 * Part 1: Sum ratings of accepted parts
 */
function part1(required struct workflows, required array parts) {
    var total = 0;

    for (var part in parts) {
        if (processPart(workflows, part)) {
            total += part.x + part.m + part.a + part.s;
        }
    }

    return total;
}

/**
 * Count combinations of xmas values that lead to acceptance.
 * Uses range splitting to process all possible paths through workflows.
 * ranges: struct mapping x, m, a, s to {lo, hi} inclusive ranges
 */
function countAccepted(required struct workflows, required string workflow, required struct ranges) {
    if (workflow == "R") {
        return 0;
    }

    if (workflow == "A") {
        // Count all combinations in current ranges
        // Use precisionEvaluate for large numbers
        var result = precisionEvaluate(
            (ranges.x.hi - ranges.x.lo + 1) *
            (ranges.m.hi - ranges.m.lo + 1) *
            (ranges.a.hi - ranges.a.lo + 1) *
            (ranges.s.hi - ranges.s.lo + 1)
        );
        return result;
    }

    var total = 0;
    // Make a copy of ranges
    var currentRanges = {
        x: {lo: ranges.x.lo, hi: ranges.x.hi},
        m: {lo: ranges.m.lo, hi: ranges.m.hi},
        a: {lo: ranges.a.lo, hi: ranges.a.hi},
        s: {lo: ranges.s.lo, hi: ranges.s.hi}
    };

    var rules = workflows[workflow];

    for (var rule in rules) {
        if (rule.attr == "") {
            // Default rule
            total = precisionEvaluate(total + countAccepted(workflows, rule.destination, currentRanges));
        } else {
            var lo = currentRanges[rule.attr].lo;
            var hi = currentRanges[rule.attr].hi;

            if (rule.op == "<") {
                // Split: [lo, value-1] goes to destination, [value, hi] continues
                if (lo < rule.value) {
                    // Part that matches the condition
                    var newRanges = {
                        x: {lo: currentRanges.x.lo, hi: currentRanges.x.hi},
                        m: {lo: currentRanges.m.lo, hi: currentRanges.m.hi},
                        a: {lo: currentRanges.a.lo, hi: currentRanges.a.hi},
                        s: {lo: currentRanges.s.lo, hi: currentRanges.s.hi}
                    };
                    newRanges[rule.attr].hi = min(hi, rule.value - 1);
                    total = precisionEvaluate(total + countAccepted(workflows, rule.destination, newRanges));
                }
                // Remaining part continues to next rule
                if (hi >= rule.value) {
                    currentRanges[rule.attr].lo = max(lo, rule.value);
                } else {
                    break; // No remaining range
                }
            } else {
                // op == ">"
                // Split: [value+1, hi] goes to destination, [lo, value] continues
                if (hi > rule.value) {
                    // Part that matches the condition
                    var newRanges = {
                        x: {lo: currentRanges.x.lo, hi: currentRanges.x.hi},
                        m: {lo: currentRanges.m.lo, hi: currentRanges.m.hi},
                        a: {lo: currentRanges.a.lo, hi: currentRanges.a.hi},
                        s: {lo: currentRanges.s.lo, hi: currentRanges.s.hi}
                    };
                    newRanges[rule.attr].lo = max(lo, rule.value + 1);
                    total = precisionEvaluate(total + countAccepted(workflows, rule.destination, newRanges));
                }
                // Remaining part continues to next rule
                if (lo <= rule.value) {
                    currentRanges[rule.attr].hi = min(hi, rule.value);
                } else {
                    break; // No remaining range
                }
            }
        }
    }

    return total;
}

/**
 * Part 2: Count all possible accepted combinations (1-4000 for each rating)
 */
function part2(required struct workflows) {
    var initialRanges = {
        x: {lo: 1, hi: 4000},
        m: {lo: 1, hi: 4000},
        a: {lo: 1, hi: 4000},
        s: {lo: 1, hi: 4000}
    };
    return countAccepted(workflows, "in", initialRanges);
}

// Run solutions
writeOutput("Part 1: " & part1(workflows, parts) & chr(10));
writeOutput("Part 2: " & part2(workflows) & chr(10));
</cfscript>
