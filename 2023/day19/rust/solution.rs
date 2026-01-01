// Day 19: Aplenty - Workflow processing and range analysis

use std::collections::HashMap;
use std::fs;

#[derive(Clone, Debug)]
enum Rule {
    Conditional {
        attr: char,
        op: char,
        value: i64,
        destination: String,
    },
    Default {
        destination: String,
    },
}

type Workflows = HashMap<String, Vec<Rule>>;

#[derive(Clone, Debug)]
struct Part {
    x: i64,
    m: i64,
    a: i64,
    s: i64,
}

impl Part {
    fn get(&self, attr: char) -> i64 {
        match attr {
            'x' => self.x,
            'm' => self.m,
            'a' => self.a,
            's' => self.s,
            _ => panic!("Invalid attribute: {}", attr),
        }
    }

    fn sum(&self) -> i64 {
        self.x + self.m + self.a + self.s
    }
}

#[derive(Clone, Debug)]
struct Ranges {
    x: (i64, i64),
    m: (i64, i64),
    a: (i64, i64),
    s: (i64, i64),
}

impl Ranges {
    fn new() -> Self {
        Ranges {
            x: (1, 4000),
            m: (1, 4000),
            a: (1, 4000),
            s: (1, 4000),
        }
    }

    fn get(&self, attr: char) -> (i64, i64) {
        match attr {
            'x' => self.x,
            'm' => self.m,
            'a' => self.a,
            's' => self.s,
            _ => panic!("Invalid attribute: {}", attr),
        }
    }

    fn set(&mut self, attr: char, range: (i64, i64)) {
        match attr {
            'x' => self.x = range,
            'm' => self.m = range,
            'a' => self.a = range,
            's' => self.s = range,
            _ => panic!("Invalid attribute: {}", attr),
        }
    }

    fn combinations(&self) -> i64 {
        let count = |r: (i64, i64)| -> i64 { (r.1 - r.0 + 1).max(0) };
        count(self.x) * count(self.m) * count(self.a) * count(self.s)
    }
}

fn parse_input(content: &str) -> (Workflows, Vec<Part>) {
    let sections: Vec<&str> = content.trim().split("\n\n").collect();
    let workflow_section = sections[0];
    let parts_section = sections[1];

    // Parse workflows
    let mut workflows = HashMap::new();
    for line in workflow_section.lines() {
        let brace_pos = line.find('{').unwrap();
        let name = line[..brace_pos].to_string();
        let rules_str = &line[brace_pos + 1..line.len() - 1]; // Remove { and }

        let mut rules = Vec::new();
        for rule_str in rules_str.split(',') {
            if let Some(colon_pos) = rule_str.find(':') {
                let condition = &rule_str[..colon_pos];
                let destination = rule_str[colon_pos + 1..].to_string();

                let attr = condition.chars().next().unwrap();
                let op = condition.chars().nth(1).unwrap();
                let value: i64 = condition[2..].parse().unwrap();

                rules.push(Rule::Conditional {
                    attr,
                    op,
                    value,
                    destination,
                });
            } else {
                rules.push(Rule::Default {
                    destination: rule_str.to_string(),
                });
            }
        }
        workflows.insert(name, rules);
    }

    // Parse parts
    let mut parts = Vec::new();
    for line in parts_section.lines() {
        let inner = &line[1..line.len() - 1]; // Remove { and }
        let mut part = Part {
            x: 0,
            m: 0,
            a: 0,
            s: 0,
        };

        for assignment in inner.split(',') {
            let mut parts_iter = assignment.split('=');
            let attr = parts_iter.next().unwrap();
            let value: i64 = parts_iter.next().unwrap().parse().unwrap();
            match attr {
                "x" => part.x = value,
                "m" => part.m = value,
                "a" => part.a = value,
                "s" => part.s = value,
                _ => panic!("Unknown attribute: {}", attr),
            }
        }
        parts.push(part);
    }

    (workflows, parts)
}

fn process_part(workflows: &Workflows, part: &Part) -> bool {
    let mut current = "in".to_string();

    while current != "A" && current != "R" {
        let rules = workflows.get(&current).unwrap();

        for rule in rules {
            match rule {
                Rule::Default { destination } => {
                    current = destination.clone();
                    break;
                }
                Rule::Conditional {
                    attr,
                    op,
                    value,
                    destination,
                } => {
                    let part_value = part.get(*attr);
                    let matches = match op {
                        '<' => part_value < *value,
                        '>' => part_value > *value,
                        _ => panic!("Unknown operator: {}", op),
                    };
                    if matches {
                        current = destination.clone();
                        break;
                    }
                }
            }
        }
    }

    current == "A"
}

fn part1(workflows: &Workflows, parts: &[Part]) -> i64 {
    parts
        .iter()
        .filter(|p| process_part(workflows, p))
        .map(|p| p.sum())
        .sum()
}

fn count_accepted(workflows: &Workflows, workflow: &str, ranges: Ranges) -> i64 {
    if workflow == "R" {
        return 0;
    }
    if workflow == "A" {
        return ranges.combinations();
    }

    let mut total = 0;
    let mut ranges = ranges;
    let rules = workflows.get(workflow).unwrap();

    for rule in rules {
        match rule {
            Rule::Default { destination } => {
                total += count_accepted(workflows, destination, ranges.clone());
            }
            Rule::Conditional {
                attr,
                op,
                value,
                destination,
            } => {
                let (lo, hi) = ranges.get(*attr);

                match op {
                    '<' => {
                        // Split: [lo, value-1] goes to destination, [value, hi] continues
                        if lo < *value {
                            let mut new_ranges = ranges.clone();
                            new_ranges.set(*attr, (lo, hi.min(*value - 1)));
                            total += count_accepted(workflows, destination, new_ranges);
                        }
                        if hi >= *value {
                            ranges.set(*attr, (lo.max(*value), hi));
                        } else {
                            break;
                        }
                    }
                    '>' => {
                        // Split: [value+1, hi] goes to destination, [lo, value] continues
                        if hi > *value {
                            let mut new_ranges = ranges.clone();
                            new_ranges.set(*attr, (lo.max(*value + 1), hi));
                            total += count_accepted(workflows, destination, new_ranges);
                        }
                        if lo <= *value {
                            ranges.set(*attr, (lo, hi.min(*value)));
                        } else {
                            break;
                        }
                    }
                    _ => panic!("Unknown operator: {}", op),
                }
            }
        }
    }

    total
}

fn part2(workflows: &Workflows) -> i64 {
    count_accepted(workflows, "in", Ranges::new())
}

fn main() {
    let content = fs::read_to_string("../input.txt").expect("Failed to read input file");
    let (workflows, parts) = parse_input(&content);

    println!("Part 1: {}", part1(&workflows, &parts));
    println!("Part 2: {}", part2(&workflows));
}
