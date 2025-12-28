#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm>
#include <array>

// Named constants
constexpr size_t HAND_SIZE = 5;
constexpr size_t ASCII_TABLE_SIZE = 256;

// Card strength order (higher index = stronger)
constexpr std::string_view CARD_STRENGTH = "23456789TJQKA";
constexpr std::string_view CARD_STRENGTH_JOKER = "J23456789TQKA";  // J is weakest in Part 2

// Hand type values
constexpr int FIVE_OF_A_KIND = 6;
constexpr int FOUR_OF_A_KIND = 5;
constexpr int FULL_HOUSE = 4;
constexpr int THREE_OF_A_KIND = 3;
constexpr int TWO_PAIR = 2;
constexpr int ONE_PAIR = 1;
constexpr int HIGH_CARD = 0;

struct Hand {
    std::string cards;
    int bid;
};

int getCardStrength(char c, std::string_view order) {
    return static_cast<int>(order.find(c));
}

// Shared helper to classify hand type from sorted counts
int classifyHandType(const std::vector<int>& sortedCounts) {
    if (sortedCounts == std::vector<int>{5}) {
        return FIVE_OF_A_KIND;
    } else if (sortedCounts == std::vector<int>{4, 1}) {
        return FOUR_OF_A_KIND;
    } else if (sortedCounts == std::vector<int>{3, 2}) {
        return FULL_HOUSE;
    } else if (sortedCounts == std::vector<int>{3, 1, 1}) {
        return THREE_OF_A_KIND;
    } else if (sortedCounts == std::vector<int>{2, 2, 1}) {
        return TWO_PAIR;
    } else if (sortedCounts == std::vector<int>{2, 1, 1, 1}) {
        return ONE_PAIR;
    } else {
        return HIGH_CARD;
    }
}

int getHandType(const std::string& hand) {
    // Count occurrences of each card
    std::array<int, ASCII_TABLE_SIZE> counts{};
    for (char c : hand) {
        counts[static_cast<unsigned char>(c)]++;
    }

    // Collect non-zero counts and sort descending
    std::vector<int> sortedCounts;
    for (int count : counts) {
        if (count > 0) {
            sortedCounts.push_back(count);
        }
    }
    std::sort(sortedCounts.begin(), sortedCounts.end(), std::greater<int>());

    return classifyHandType(sortedCounts);
}

int getHandTypeWithJokers(const std::string& hand) {
    int jokerCount = std::count(hand.begin(), hand.end(), 'J');

    if (jokerCount == 0) {
        return getHandType(hand);
    }
    if (jokerCount == static_cast<int>(HAND_SIZE)) {
        return FIVE_OF_A_KIND;
    }

    // Count non-joker cards
    std::array<int, ASCII_TABLE_SIZE> counts{};
    for (char c : hand) {
        if (c != 'J') {
            counts[static_cast<unsigned char>(c)]++;
        }
    }

    // Collect non-zero counts and sort descending
    std::vector<int> sortedCounts;
    for (int count : counts) {
        if (count > 0) {
            sortedCounts.push_back(count);
        }
    }
    std::sort(sortedCounts.begin(), sortedCounts.end(), std::greater<int>());

    // Add jokers to the highest count
    sortedCounts[0] += jokerCount;

    return classifyHandType(sortedCounts);
}

long long part1(const std::vector<Hand>& hands) {
    // Create sorted copy using lambda comparator
    std::vector<Hand> sortedHands = hands;
    std::sort(sortedHands.begin(), sortedHands.end(), [](const Hand& a, const Hand& b) {
        int typeA = getHandType(a.cards);
        int typeB = getHandType(b.cards);

        if (typeA != typeB) {
            return typeA < typeB;
        }

        // Compare card by card
        for (size_t i = 0; i < HAND_SIZE; ++i) {
            int strengthA = getCardStrength(a.cards[i], CARD_STRENGTH);
            int strengthB = getCardStrength(b.cards[i], CARD_STRENGTH);
            if (strengthA != strengthB) {
                return strengthA < strengthB;
            }
        }
        return false;
    });

    long long total = 0;
    for (size_t rank = 0; rank < sortedHands.size(); ++rank) {
        total += static_cast<long long>(rank + 1) * sortedHands[rank].bid;
    }
    return total;
}

long long part2(const std::vector<Hand>& hands) {
    // Create sorted copy using lambda comparator
    std::vector<Hand> sortedHands = hands;
    std::sort(sortedHands.begin(), sortedHands.end(), [](const Hand& a, const Hand& b) {
        int typeA = getHandTypeWithJokers(a.cards);
        int typeB = getHandTypeWithJokers(b.cards);

        if (typeA != typeB) {
            return typeA < typeB;
        }

        // Compare card by card (with J as weakest)
        for (size_t i = 0; i < HAND_SIZE; ++i) {
            int strengthA = getCardStrength(a.cards[i], CARD_STRENGTH_JOKER);
            int strengthB = getCardStrength(b.cards[i], CARD_STRENGTH_JOKER);
            if (strengthA != strengthB) {
                return strengthA < strengthB;
            }
        }
        return false;
    });

    long long total = 0;
    for (size_t rank = 0; rank < sortedHands.size(); ++rank) {
        total += static_cast<long long>(rank + 1) * sortedHands[rank].bid;
    }
    return total;
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error: Could not open input.txt" << std::endl;
        return 1;
    }

    std::vector<Hand> hands;
    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) continue;

        std::istringstream iss(line);
        Hand hand;
        iss >> hand.cards >> hand.bid;
        hands.push_back(hand);
    }

    std::cout << "Part 1: " << part1(hands) << std::endl;
    std::cout << "Part 2: " << part2(hands) << std::endl;

    return 0;
}
