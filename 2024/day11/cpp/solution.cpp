#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <cmath>

using namespace std;

// Memoization cache: (value, blinks) -> count
map<pair<long long, int>, long long> memo;

// Count digits in a number
int count_digits(long long n) {
    if (n == 0) return 1;
    int count = 0;
    while (n > 0) {
        count++;
        n /= 10;
    }
    return count;
}

// Split a number with even digits into left and right halves
pair<long long, long long> split_number(long long value, int num_digits) {
    int mid = num_digits / 2;
    long long divisor = 1;
    for (int i = 0; i < mid; i++) {
        divisor *= 10;
    }
    long long left = value / divisor;
    long long right = value % divisor;
    return {left, right};
}

// Count how many stones result from a single stone after N blinks
long long count_stones(long long value, int blinks) {
    // Base case
    if (blinks == 0) {
        return 1;
    }

    // Check memoization cache
    auto key = make_pair(value, blinks);
    if (memo.find(key) != memo.end()) {
        return memo[key];
    }

    long long result;

    // Rule 1: 0 becomes 1
    if (value == 0) {
        result = count_stones(1, blinks - 1);
    }
    // Rule 2: Even number of digits -> split
    else {
        int num_digits = count_digits(value);
        if (num_digits % 2 == 0) {
            auto [left, right] = split_number(value, num_digits);
            result = count_stones(left, blinks - 1) + count_stones(right, blinks - 1);
        }
        // Rule 3: Multiply by 2024
        else {
            result = count_stones(value * 2024, blinks - 1);
        }
    }

    // Store in cache
    memo[key] = result;
    return result;
}

long long part1(const vector<long long>& stones) {
    long long total = 0;
    for (long long stone : stones) {
        total += count_stones(stone, 25);
    }
    return total;
}

long long part2(const vector<long long>& stones) {
    long long total = 0;
    for (long long stone : stones) {
        total += count_stones(stone, 75);
    }
    return total;
}

int main() {
    // Read input file
    ifstream infile("../input.txt");
    if (!infile) {
        cerr << "Error: Could not open input.txt" << endl;
        return 1;
    }

    string line;
    getline(infile, line);
    infile.close();

    // Parse space-separated numbers
    vector<long long> stones;
    stringstream ss(line);
    long long num;
    while (ss >> num) {
        stones.push_back(num);
    }

    // Solve both parts
    cout << "Part 1: " << part1(stones) << endl;

    // Clear memo between parts to avoid any issues
    // (though it shouldn't matter since we're using different blink counts)

    cout << "Part 2: " << part2(stones) << endl;

    return 0;
}
