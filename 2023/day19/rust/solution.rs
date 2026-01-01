// Day 19: Aplenty - Workflow processing and range analysis

use std::collections::HashMap;
use std::fs;

/// Represents the four part attributes (x, m, a, s)
#[derive(Clone, Copy, Debug)]
enum Attr {
    X,
    M,
    A,
    S,
}

impl Attr {
    fn from_char(c: char) -> Option<Attr> {
        match c {
            'x' => Some(Attr::X),
            'm' => Some(Attr::M),
            'a' => Some(Attr::A),
            's' => Some(Attr::S),
            _ => None,
        }
    }
}

/// Comparison operators for rules
#[derive(Clone, Copy, Debug)]
enum Op {
    LessThan,
    GreaterThan,
}

impl Op {
    fn from_char(c: char) -> Option<Op> {
        match c {
            '<' => Some(Op::LessThan),
            '>' => Some(Op::GreaterThan),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
enum Rule {
    Conditional {
        attr: Attr,
        op: Op,
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
    fn get(&self, attr: Attr) -> i64 {
        match attr {
            Attr::X => self.x,
            Attr::M => self.m,
            Attr::A => self.a,
            Attr::S => self.s,
        }
    }

    fn set(&mut self, attr: Attr, value: i64) {
        match attr {
            Attr::X => self.x = value,
            Attr::M => self.m = value,
            Attr::A => self.a = value,
            Attr::S => self.s = value,
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

    fn get(&self, attr: Attr) -> (i64, i64) {
        match attr {
            Attr::X => self.x,
            Attr::M => self.m,
            Attr::A => self.a,
            Attr::S => self.s,
        }
    }

    fn set(&mut self, attr: Attr, range: (i64, i64)) {
        match attr {
            Attr::X => self.x = range,
            Attr::M => self.m = range,
            Attr::A => self.a = range,
            Attr::S => self.s = range,
        }
    }

    fn with_attr(&self, attr: Attr, range: (i64, i64)) -> Ranges {
        let mut new_ranges = self.clone();
        new_ranges.set(attr, range);
        new_ranges
    }

    fn combinations(&self) -> i64 {
        let count = |r: (i64, i64)| -> i64 { (r.1 - r.0 + 1).max(0) };
        count(self.x) * count(self.m) * count(self.a) * count(self.s)
    }
}

fn parse_input(content: &str) -> Option<(Workflows, Vec<Part>)> {
    let sections: Vec<&str> = content.trim().split("\n\n").collect();
    if sections.len() < 2 {
        return None;
    }
    let workflow_section = sections[0];
    let parts_section = sections[1];

    // Parse workflows
    let mut workflows = HashMap::new();
    for line in workflow_section.lines() {
        let brace_pos = line.find('{')?;
        let name = line[..brace_pos].to_string();
        let rules_str = &line[brace_pos + 1..line.len() - 1]; // Remove { and }

        let mut rules = Vec::new();
        for rule_str in rules_str.split(',') {
            if let Some(colon_pos) = rule_str.find(':') {
                let condition = &rule_str[..colon_pos];
                let destination = rule_str[colon_pos + 1..].to_string();

                let attr = Attr::from_char(condition.chars().next()?)?;
                let op = Op::from_char(condition.chars().nth(1)?)?;
                let value: i64 = condition[2..].parse().ok()?;

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
            let attr_str = parts_iter.next()?;
            let value: i64 = parts_iter.next()?.parse().ok()?;
            let attr = Attr::from_char(attr_str.chars().next()?)?;
            part.set(attr, value);
        }
        parts.push(part);
    }

    Some((workflows, parts))
}

fn process_part(workflows: &Workflows, part: &Part) -> Option<bool> {
    let mut current = "in".to_string();

    while current != "A" && current != "R" {
        let rules = workflows.get(&current)?;

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
                        Op::LessThan => part_value < *value,
                        Op::GreaterThan => part_value > *value,
                    };
                    if matches {
                        current = destination.clone();
                        break;
                    }
                }
            }
        }
    }

    Some(current == "A")
}

fn part1(workflows: &Workflows, parts: &[Part]) -> i64 {
    parts
        .iter()
        .filter(|p| process_part(workflows, p).unwrap_or(false))
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

    let rules = match workflows.get(workflow) {
        Some(r) => r,
        None => return 0,
    };

    let mut total = 0;
    let mut ranges = ranges;

    for rule in rules {
        match rule {
            Rule::Default { destination } => {
                // Last rule - can move ranges instead of cloning
                total += count_accepted(workflows, destination, ranges);
                break;
            }
            Rule::Conditional {
                attr,
                op,
                value,
                destination,
            } => {
                let (lo, hi) = ranges.get(*attr);

                match op {
                    Op::LessThan => {
                        // Split: [lo, value-1] goes to destination, [value, hi] continues
                        if lo < *value {
                            let matching_range = (lo, hi.min(*value - 1));
                            total += count_accepted(workflows, destination, ranges.with_attr(*attr, matching_range));
                        }
                        if hi >= *value {
                            ranges.set(*attr, (lo.max(*value), hi));
                        } else {
                            break;
                        }
                    }
                    Op::GreaterThan => {
                        // Split: [value+1, hi] goes to destination, [lo, value] continues
                        if hi > *value {
                            let matching_range = (lo.max(*value + 1), hi);
                            total += count_accepted(workflows, destination, ranges.with_attr(*attr, matching_range));
                        }
                        if lo <= *value {
                            ranges.set(*attr, (lo, hi.min(*value)));
                        } else {
                            break;
                        }
                    }
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
    let (workflows, parts) = parse_input(&content).expect("Failed to parse input");

    println!("Part 1: {}", part1(&workflows, &parts));
    println!("Part 2: {}", part2(&workflows));
}
