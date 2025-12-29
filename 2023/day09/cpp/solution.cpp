#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using Sequence = std::vector<long long>;
using Histories = std::vector<Sequence>;

Histories parse_input(const std::string& filename) {
    Histories histories;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        Sequence seq;
        std::istringstream iss(line);
        long long num;
        while (iss >> num) {
            seq.push_back(num);
        }
        histories.push_back(std::move(seq));
    }

    return histories;
}

Sequence get_differences(const Sequence& seq) {
    Sequence diff;
    diff.reserve(seq.size() - 1);

    for (size_t i = 0; i < seq.size() - 1; ++i) {
        diff.push_back(seq[i + 1] - seq[i]);
    }

    return diff;
}

bool all_zeros(const Sequence& seq) {
    return std::all_of(seq.begin(), seq.end(), [](long long x) { return x == 0; });
}

long long extrapolate_next(Sequence seq) {
    std::vector<Sequence> sequences;
    sequences.push_back(seq);

    while (!all_zeros(sequences.back())) {
        sequences.push_back(get_differences(sequences.back()));
    }

    // Work backwards, adding the last element of each level
    for (int i = static_cast<int>(sequences.size()) - 2; i >= 0; --i) {
        long long next_val = sequences[i].back() + sequences[i + 1].back();
        sequences[i].push_back(next_val);
    }

    return sequences[0].back();
}

long long extrapolate_prev(Sequence seq) {
    std::vector<Sequence> sequences;
    sequences.push_back(seq);

    while (!all_zeros(sequences.back())) {
        sequences.push_back(get_differences(sequences.back()));
    }

    // Work backwards, computing the previous value at each level
    for (int i = static_cast<int>(sequences.size()) - 2; i >= 0; --i) {
        long long prev_val = sequences[i].front() - sequences[i + 1].front();
        sequences[i].insert(sequences[i].begin(), prev_val);
    }

    return sequences[0].front();
}

long long part1(const Histories& histories) {
    long long sum = 0;
    for (const auto& history : histories) {
        sum += extrapolate_next(history);
    }
    return sum;
}

long long part2(const Histories& histories) {
    long long sum = 0;
    for (const auto& history : histories) {
        sum += extrapolate_prev(history);
    }
    return sum;
}

int main() {
    auto histories = parse_input("../input.txt");

    std::cout << "Part 1: " << part1(histories) << std::endl;
    std::cout << "Part 2: " << part2(histories) << std::endl;

    return 0;
}
