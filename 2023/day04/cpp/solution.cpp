#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <sstream>
#include <string>
#include <unordered_set>
#include <vector>

struct Card {
    std::unordered_set<int> winning;
    std::vector<int> have;
};

Card parseCard(const std::string& line) {
    Card card;

    const std::size_t colonPos = line.find(':');
    const std::size_t pipePos = line.find('|');

    std::istringstream winningStream(line.substr(colonPos + 1, pipePos - colonPos - 1));
    int num;
    while (winningStream >> num) {
        card.winning.insert(num);
    }

    std::istringstream haveStream(line.substr(pipePos + 1));
    while (haveStream >> num) {
        card.have.push_back(num);
    }

    return card;
}

std::vector<Card> parseCards(std::ifstream& file) {
    std::vector<Card> cards;
    std::string line;

    while (std::getline(file, line)) {
        if (!line.empty()) {
            cards.push_back(parseCard(line));
        }
    }

    return cards;
}

int countMatches(const Card& card) {
    return static_cast<int>(std::count_if(
        card.have.begin(),
        card.have.end(),
        [&card](int num) { return card.winning.count(num) > 0; }
    ));
}

int part1(const std::vector<Card>& cards) {
    return std::accumulate(
        cards.begin(),
        cards.end(),
        0,
        [](int total, const Card& card) {
            const int matches = countMatches(card);
            return total + (matches > 0 ? (1 << (matches - 1)) : 0);
        }
    );
}

long long part2(const std::vector<Card>& cards) {
    const std::size_t n = cards.size();

    std::vector<int> matches(n);
    std::transform(
        cards.begin(),
        cards.end(),
        matches.begin(),
        countMatches
    );

    std::vector<long long> copies(n, 1);

    for (std::size_t i = 0; i < n; ++i) {
        const std::size_t limit = std::min(i + 1 + static_cast<std::size_t>(matches[i]), n);
        for (std::size_t j = i + 1; j < limit; ++j) {
            copies[j] += copies[i];
        }
    }

    return std::accumulate(copies.begin(), copies.end(), 0LL);
}

int main() {
    std::ifstream inputFile("../input.txt");
    if (!inputFile) {
        std::cerr << "Error opening input file\n";
        return 1;
    }

    const std::vector<Card> cards = parseCards(inputFile);

    std::cout << "Part 1: " << part1(cards) << '\n';
    std::cout << "Part 2: " << part2(cards) << '\n';

    return 0;
}
