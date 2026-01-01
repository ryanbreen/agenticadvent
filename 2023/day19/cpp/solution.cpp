// Day 19: Aplenty - Workflow processing and range analysis
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <sstream>
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
            default: return 0;
        }
    }

    int sum() const { return x + m + a + s; }
};

struct Range {
    int lo, hi;

    long long size() const { return std::max(0, hi - lo + 1); }
};

struct Ranges {
    Range x{1, 4000}, m{1, 4000}, a{1, 4000}, s{1, 4000};

    Range& get(char attr) {
        switch (attr) {
            case 'x': return x;
            case 'm': return m;
            case 'a': return a;
            case 's': return s;
            default: return x; // Should never happen
        }
    }

    const Range& get(char attr) const {
        switch (attr) {
            case 'x': return x;
            case 'm': return m;
            case 'a': return a;
            case 's': return s;
            default: return x;
        }
    }

    long long combinations() const {
        return x.size() * m.size() * a.size() * s.size();
    }
};

using Workflows = std::map<std::string, std::vector<Rule>>;

std::pair<Workflows, std::vector<Part>> parseInput(const std::string& filename) {
    std::ifstream file(filename);
    std::string line;
    Workflows workflows;
    std::vector<Part> parts;

    // Parse workflows
    while (std::getline(file, line) && !line.empty()) {
        size_t bracePos = line.find('{');
        std::string name = line.substr(0, bracePos);
        std::string rulesStr = line.substr(bracePos + 1, line.size() - bracePos - 2);

        std::vector<Rule> rules;
        std::stringstream ss(rulesStr);
        std::string ruleStr;

        while (std::getline(ss, ruleStr, ',')) {
            Rule rule{};
            size_t colonPos = ruleStr.find(':');

            if (colonPos != std::string::npos) {
                // Conditional rule
                rule.attr = ruleStr[0];
                rule.op = ruleStr[1];
                rule.value = std::stoi(ruleStr.substr(2, colonPos - 2));
                rule.destination = ruleStr.substr(colonPos + 1);
            } else {
                // Default rule
                rule.attr = '\0';
                rule.op = '\0';
                rule.value = 0;
                rule.destination = ruleStr;
            }
            rules.push_back(rule);
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

long long countAccepted(const Workflows& workflows, const std::string& workflow, Ranges ranges) {
    if (workflow == "R") return 0;
    if (workflow == "A") return ranges.combinations();

    long long total = 0;

    for (const auto& rule : workflows.at(workflow)) {
        if (rule.attr == '\0') {
            // Default rule
            total += countAccepted(workflows, rule.destination, ranges);
        } else {
            Range& r = ranges.get(rule.attr);

            if (rule.op == '<') {
                // Split: [lo, value-1] goes to destination, [value, hi] continues
                if (r.lo < rule.value) {
                    Ranges newRanges = ranges;
                    newRanges.get(rule.attr) = {r.lo, std::min(r.hi, rule.value - 1)};
                    total += countAccepted(workflows, rule.destination, newRanges);
                }
                if (r.hi >= rule.value) {
                    r = {std::max(r.lo, rule.value), r.hi};
                } else {
                    break; // No remaining range
                }
            } else { // op == '>'
                // Split: [value+1, hi] goes to destination, [lo, value] continues
                if (r.hi > rule.value) {
                    Ranges newRanges = ranges;
                    newRanges.get(rule.attr) = {std::max(r.lo, rule.value + 1), r.hi};
                    total += countAccepted(workflows, rule.destination, newRanges);
                }
                if (r.lo <= rule.value) {
                    r = {r.lo, std::min(r.hi, rule.value)};
                } else {
                    break; // No remaining range
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
