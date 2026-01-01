// Day 19: Aplenty - Workflow processing and range analysis
//
// This solution processes workflows that route parts through a series of rules.
// Part 1: Evaluate individual parts against workflow rules
// Part 2: Count all valid combinations using range-based analysis
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

struct Rule {
    char attr;       // 'x', 'm', 'a', 's' or '\0' for default
    char op;         // '<' or '>' or '\0' for default
    int value;
    std::string destination;
};

struct Part {
    int x, m, a, s;

    int get(char attr) const {
        switch (attr) {
            case 'x': return x;
            case 'm': return m;
            case 'a': return a;
            case 's': return s;
            default:
                throw std::invalid_argument(
                    std::string("Invalid part attribute: '") + attr + "'");
        }
    }

    int sum() const { return x + m + a + s; }
};

// Represents an inclusive range [lo, hi] for a single attribute
struct Range {
    int lo, hi;

    long long size() const { return std::max(0, hi - lo + 1); }
};

// Represents ranges for all four attributes (x, m, a, s)
// Used in Part 2 to track which combinations of values are still valid
// as we traverse the workflow rules
struct Ranges {
    Range x{1, 4000}, m{1, 4000}, a{1, 4000}, s{1, 4000};

    Range& get(char attr) {
        switch (attr) {
            case 'x': return x;
            case 'm': return m;
            case 'a': return a;
            case 's': return s;
            default:
                throw std::invalid_argument(
                    std::string("Invalid range attribute: '") + attr + "'");
        }
    }

    const Range& get(char attr) const {
        switch (attr) {
            case 'x': return x;
            case 'm': return m;
            case 'a': return a;
            case 's': return s;
            default:
                throw std::invalid_argument(
                    std::string("Invalid range attribute: '") + attr + "'");
        }
    }

    long long combinations() const {
        return x.size() * m.size() * a.size() * s.size();
    }
};

using Workflows = std::map<std::string, std::vector<Rule>>;

// Validates that a character is a valid attribute (x, m, a, or s)
bool isValidAttribute(char c) {
    return c == 'x' || c == 'm' || c == 'a' || c == 's';
}

// Validates that a character is a valid operator (< or >)
bool isValidOperator(char c) {
    return c == '<' || c == '>';
}

std::pair<Workflows, std::vector<Part>> parseInput(const std::string& filename) {
    std::ifstream file(filename);
    if (!file) {
        throw std::runtime_error("Cannot open input file: " + filename);
    }

    std::string line;
    Workflows workflows;
    std::vector<Part> parts;

    // Parse workflows
    while (std::getline(file, line) && !line.empty()) {
        size_t bracePos = line.find('{');
        if (bracePos == std::string::npos || line.back() != '}') {
            throw std::runtime_error("Invalid workflow format: " + line);
        }

        std::string name = line.substr(0, bracePos);
        if (name.empty()) {
            throw std::runtime_error("Empty workflow name in: " + line);
        }

        std::string rulesStr = line.substr(bracePos + 1, line.size() - bracePos - 2);

        std::vector<Rule> rules;
        std::stringstream ss(rulesStr);
        std::string ruleStr;

        while (std::getline(ss, ruleStr, ',')) {
            Rule rule{};
            size_t colonPos = ruleStr.find(':');

            if (colonPos != std::string::npos) {
                // Conditional rule: format is "a<123:dest" or "a>123:dest"
                if (ruleStr.size() < 4) {
                    throw std::runtime_error("Invalid conditional rule (too short): " + ruleStr);
                }

                rule.attr = ruleStr[0];
                if (!isValidAttribute(rule.attr)) {
                    throw std::runtime_error(
                        std::string("Invalid attribute '") + rule.attr + "' in rule: " + ruleStr);
                }

                rule.op = ruleStr[1];
                if (!isValidOperator(rule.op)) {
                    throw std::runtime_error(
                        std::string("Invalid operator '") + rule.op + "' in rule: " + ruleStr);
                }

                rule.value = std::stoi(ruleStr.substr(2, colonPos - 2));
                rule.destination = ruleStr.substr(colonPos + 1);

                if (rule.destination.empty()) {
                    throw std::runtime_error("Empty destination in rule: " + ruleStr);
                }
            } else {
                // Default rule (just a destination)
                if (ruleStr.empty()) {
                    throw std::runtime_error("Empty rule in workflow: " + name);
                }
                rule.attr = '\0';
                rule.op = '\0';
                rule.value = 0;
                rule.destination = ruleStr;
            }
            rules.push_back(rule);
        }

        if (rules.empty()) {
            throw std::runtime_error("Workflow has no rules: " + name);
        }

        workflows[name] = rules;
    }

    // Parse parts
    std::regex partRegex(R"(\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\})");
    while (std::getline(file, line)) {
        std::smatch match;
        if (std::regex_match(line, match, partRegex)) {
            Part part{
                std::stoi(match[1]),
                std::stoi(match[2]),
                std::stoi(match[3]),
                std::stoi(match[4])
            };
            parts.push_back(part);
        }
    }

    return {workflows, parts};
}

bool processPart(const Workflows& workflows, const Part& part) {
    std::string current = "in";

    while (current != "A" && current != "R") {
        for (const auto& rule : workflows.at(current)) {
            if (rule.attr == '\0') {
                // Default rule
                current = rule.destination;
                break;
            }

            int partValue = part.get(rule.attr);
            bool matches = (rule.op == '<') ? (partValue < rule.value)
                                            : (partValue > rule.value);
            if (matches) {
                current = rule.destination;
                break;
            }
        }
    }

    return current == "A";
}

long long part1(const Workflows& workflows, const std::vector<Part>& parts) {
    long long total = 0;
    for (const auto& part : parts) {
        if (processPart(workflows, part)) {
            total += part.sum();
        }
    }
    return total;
}

/**
 * Recursively counts all valid part combinations that reach the "Accept" state.
 *
 * Range-Splitting Algorithm:
 * --------------------------
 * Instead of testing each of the 4000^4 possible part combinations individually,
 * we track ranges of valid values for each attribute (x, m, a, s).
 *
 * When we encounter a conditional rule like "a<2006:qkq":
 *
 * For operator '<' (e.g., a < 2006):
 *   - Values in [lo, 2005] satisfy the condition -> follow the rule's destination
 *   - Values in [2006, hi] do NOT satisfy -> continue to the next rule
 *   We "split" the range and recurse on both branches.
 *
 * For operator '>' (e.g., a > 2006):
 *   - Values in [2007, hi] satisfy the condition -> follow the rule's destination
 *   - Values in [lo, 2006] do NOT satisfy -> continue to the next rule
 *
 * This is essentially a tree traversal where:
 *   - Each node is a (workflow, ranges) pair
 *   - Leaves are "A" (accepted) or "R" (rejected)
 *   - We sum up combinations() for all paths reaching "A"
 *
 * Time complexity: O(W * R) where W = number of workflows, R = rules per workflow
 * Space complexity: O(W) for recursion depth
 *
 * @param workflows The parsed workflow rules
 * @param workflow Current workflow name being processed
 * @param ranges Current valid ranges for each attribute
 * @return Number of valid part combinations that reach "Accept" from this state
 */
long long countAccepted(const Workflows& workflows, const std::string& workflow, Ranges ranges) {
    // Base cases: reached a terminal state
    if (workflow == "R") return 0;
    if (workflow == "A") return ranges.combinations();

    long long total = 0;

    for (const auto& rule : workflows.at(workflow)) {
        if (rule.attr == '\0') {
            // Default rule: all remaining values follow this path
            total += countAccepted(workflows, rule.destination, ranges);
        } else {
            Range& r = ranges.get(rule.attr);

            if (rule.op == '<') {
                // Rule: attr < value
                // Matching range: [lo, value-1] -> follows rule destination
                // Remaining range: [value, hi] -> continues to next rule
                if (r.lo < rule.value) {
                    Ranges matchingRanges = ranges;
                    matchingRanges.get(rule.attr) = {r.lo, std::min(r.hi, rule.value - 1)};
                    total += countAccepted(workflows, rule.destination, matchingRanges);
                }
                // Update range for remaining values that continue to next rule
                if (r.hi >= rule.value) {
                    r = {std::max(r.lo, rule.value), r.hi};
                } else {
                    break; // Entire range matched; no values continue to next rule
                }
            } else { // op == '>'
                // Rule: attr > value
                // Matching range: [value+1, hi] -> follows rule destination
                // Remaining range: [lo, value] -> continues to next rule
                if (r.hi > rule.value) {
                    Ranges matchingRanges = ranges;
                    matchingRanges.get(rule.attr) = {std::max(r.lo, rule.value + 1), r.hi};
                    total += countAccepted(workflows, rule.destination, matchingRanges);
                }
                // Update range for remaining values that continue to next rule
                if (r.lo <= rule.value) {
                    r = {r.lo, std::min(r.hi, rule.value)};
                } else {
                    break; // Entire range matched; no values continue to next rule
                }
            }
        }
    }

    return total;
}

long long part2(const Workflows& workflows) {
    Ranges initialRanges;
    return countAccepted(workflows, "in", initialRanges);
}

int main() {
    auto [workflows, parts] = parseInput("../input.txt");

    std::cout << "Part 1: " << part1(workflows, parts) << std::endl;
    std::cout << "Part 2: " << part2(workflows) << std::endl;

    return 0;
}
