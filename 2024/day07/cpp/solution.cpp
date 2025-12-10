#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <cmath>

using namespace std;

struct Equation {
    long long target;
    vector<long long> nums;
};

vector<Equation> parse_input(const string& filename) {
    vector<Equation> equations;
    ifstream file(filename);
    string line;

    while (getline(file, line)) {
        Equation eq;
        size_t colon = line.find(':');
        eq.target = stoll(line.substr(0, colon));

        istringstream iss(line.substr(colon + 2));
        long long num;
        while (iss >> num) {
            eq.nums.push_back(num);
        }

        equations.push_back(eq);
    }

    return equations;
}

long long concatenate(long long left, long long right) {
    // Count digits in right number
    long long temp = right;
    int digits = 0;
    if (temp == 0) {
        digits = 1;
    } else {
        while (temp > 0) {
            digits++;
            temp /= 10;
        }
    }

    // Shift left by number of digits and add right
    return left * (long long)pow(10, digits) + right;
}

long long evaluate(const vector<long long>& nums, const vector<int>& ops) {
    long long result = nums[0];

    for (size_t i = 0; i < ops.size(); i++) {
        if (ops[i] == 0) {  // Addition
            result += nums[i + 1];
        } else if (ops[i] == 1) {  // Multiplication
            result *= nums[i + 1];
        } else if (ops[i] == 2) {  // Concatenation
            result = concatenate(result, nums[i + 1]);
        }
    }

    return result;
}

bool can_make_target(long long target, const vector<long long>& nums, int num_operators) {
    int n_ops = nums.size() - 1;

    // Generate all combinations of operators
    // num_operators: 2 for part1 (+, *), 3 for part2 (+, *, ||)
    long long total_combinations = 1;
    for (int i = 0; i < n_ops; i++) {
        total_combinations *= num_operators;
    }

    for (long long comb = 0; comb < total_combinations; comb++) {
        vector<int> ops(n_ops);
        long long temp = comb;

        // Convert combination number to operator sequence
        for (int i = 0; i < n_ops; i++) {
            ops[i] = temp % num_operators;
            temp /= num_operators;
        }

        if (evaluate(nums, ops) == target) {
            return true;
        }
    }

    return false;
}

long long part1(const vector<Equation>& equations) {
    long long total = 0;

    for (const auto& eq : equations) {
        if (can_make_target(eq.target, eq.nums, 2)) {
            total += eq.target;
        }
    }

    return total;
}

long long part2(const vector<Equation>& equations) {
    long long total = 0;

    for (const auto& eq : equations) {
        if (can_make_target(eq.target, eq.nums, 3)) {
            total += eq.target;
        }
    }

    return total;
}

int main() {
    vector<Equation> equations = parse_input("../input.txt");

    cout << "Part 1: " << part1(equations) << endl;
    cout << "Part 2: " << part2(equations) << endl;

    return 0;
}
