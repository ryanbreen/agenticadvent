#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <variant>
#include <cstdint>

struct Operation {
    std::string left;
    char op;
    std::string right;
};

using Job = std::variant<int64_t, Operation>;
using Monkeys = std::unordered_map<std::string, Job>;

Monkeys parse_input(const std::string& filename) {
    Monkeys monkeys;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        size_t colon_pos = line.find(':');
        std::string name = line.substr(0, colon_pos);
        std::string job_str = line.substr(colon_pos + 2);

        std::istringstream iss(job_str);
        std::string part1;
        iss >> part1;

        std::string op_str;
        if (iss >> op_str) {
            std::string part3;
            iss >> part3;
            monkeys[name] = Operation{part1, op_str[0], part3};
        } else {
            monkeys[name] = std::stoll(part1);
        }
    }

    return monkeys;
}

int64_t evaluate(const Monkeys& monkeys, const std::string& name,
                 std::unordered_map<std::string, int64_t>& memo) {
    if (memo.count(name)) {
        return memo[name];
    }

    const Job& job = monkeys.at(name);

    if (std::holds_alternative<int64_t>(job)) {
        return std::get<int64_t>(job);
    }

    const Operation& op = std::get<Operation>(job);
    int64_t left_val = evaluate(monkeys, op.left, memo);
    int64_t right_val = evaluate(monkeys, op.right, memo);

    int64_t result;
    switch (op.op) {
        case '+': result = left_val + right_val; break;
        case '-': result = left_val - right_val; break;
        case '*': result = left_val * right_val; break;
        case '/': result = left_val / right_val; break;
        default: result = 0;
    }

    memo[name] = result;
    return result;
}

int64_t evaluate(const Monkeys& monkeys, const std::string& name) {
    std::unordered_map<std::string, int64_t> memo;
    return evaluate(monkeys, name, memo);
}

bool contains_humn(const Monkeys& monkeys, const std::string& name,
                   std::unordered_map<std::string, bool>& memo) {
    if (memo.count(name)) {
        return memo[name];
    }
    if (name == "humn") {
        return true;
    }

    const Job& job = monkeys.at(name);

    if (std::holds_alternative<int64_t>(job)) {
        memo[name] = false;
        return false;
    }

    const Operation& op = std::get<Operation>(job);
    bool result = contains_humn(monkeys, op.left, memo) ||
                  contains_humn(monkeys, op.right, memo);
    memo[name] = result;
    return result;
}

bool contains_humn(const Monkeys& monkeys, const std::string& name) {
    std::unordered_map<std::string, bool> memo;
    return contains_humn(monkeys, name, memo);
}

int64_t solve_for_humn(const Monkeys& monkeys, const std::string& name, int64_t target) {
    if (name == "humn") {
        return target;
    }

    const Job& job = monkeys.at(name);

    if (std::holds_alternative<int64_t>(job)) {
        return -1;  // Can't solve if it's just a number
    }

    const Operation& op = std::get<Operation>(job);
    bool left_has_humn = contains_humn(monkeys, op.left);

    if (left_has_humn) {
        int64_t right_val = evaluate(monkeys, op.right);
        int64_t new_target;
        switch (op.op) {
            case '+': new_target = target - right_val; break;
            case '-': new_target = target + right_val; break;
            case '*': new_target = target / right_val; break;
            case '/': new_target = target * right_val; break;
            default: new_target = 0;
        }
        return solve_for_humn(monkeys, op.left, new_target);
    } else {
        int64_t left_val = evaluate(monkeys, op.left);
        int64_t new_target;
        switch (op.op) {
            case '+': new_target = target - left_val; break;
            case '-': new_target = left_val - target; break;
            case '*': new_target = target / left_val; break;
            case '/': new_target = left_val / target; break;
            default: new_target = 0;
        }
        return solve_for_humn(monkeys, op.right, new_target);
    }
}

int64_t part1(const Monkeys& monkeys) {
    return evaluate(monkeys, "root");
}

int64_t part2(const Monkeys& monkeys) {
    const Operation& root_op = std::get<Operation>(monkeys.at("root"));

    bool left_has_humn = contains_humn(monkeys, root_op.left);

    if (left_has_humn) {
        int64_t target = evaluate(monkeys, root_op.right);
        return solve_for_humn(monkeys, root_op.left, target);
    } else {
        int64_t target = evaluate(monkeys, root_op.left);
        return solve_for_humn(monkeys, root_op.right, target);
    }
}

int main() {
    std::string filename = "../input.txt";
    Monkeys monkeys = parse_input(filename);

    std::cout << "Part 1: " << part1(monkeys) << std::endl;
    std::cout << "Part 2: " << part2(monkeys) << std::endl;

    return 0;
}
