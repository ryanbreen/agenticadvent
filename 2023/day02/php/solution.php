<?php

// Read input file
$input = trim(file_get_contents(__DIR__ . '/../input.txt'));
$lines = explode("\n", $input);

function parseGame($line) {
    // Parse "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    preg_match('/Game (\d+): (.+)/', $line, $matches);
    $gameId = (int)$matches[1];
    $setsStr = $matches[2];

    $sets = [];
    $setStrs = explode(';', $setsStr);

    foreach ($setStrs as $setStr) {
        $cubes = ['red' => 0, 'green' => 0, 'blue' => 0];
        $cubeStrs = explode(',', $setStr);

        foreach ($cubeStrs as $cubeStr) {
            $cubeStr = trim($cubeStr);
            preg_match('/(\d+) (red|green|blue)/', $cubeStr, $cubeMatches);
            $count = (int)$cubeMatches[1];
            $color = $cubeMatches[2];
            $cubes[$color] = $count;
        }

        $sets[] = $cubes;
    }

    return ['id' => $gameId, 'sets' => $sets];
}

function part1($lines) {
    $maxRed = 12;
    $maxGreen = 13;
    $maxBlue = 14;

    $sumIds = 0;

    foreach ($lines as $line) {
        $game = parseGame($line);
        $possible = true;

        foreach ($game['sets'] as $set) {
            if ($set['red'] > $maxRed || $set['green'] > $maxGreen || $set['blue'] > $maxBlue) {
                $possible = false;
                break;
            }
        }

        if ($possible) {
            $sumIds += $game['id'];
        }
    }

    return $sumIds;
}

function part2($lines) {
    $sumPowers = 0;

    foreach ($lines as $line) {
        $game = parseGame($line);

        // Find minimum required cubes for each color
        $minRed = 0;
        $minGreen = 0;
        $minBlue = 0;

        foreach ($game['sets'] as $set) {
            $minRed = max($minRed, $set['red']);
            $minGreen = max($minGreen, $set['green']);
            $minBlue = max($minBlue, $set['blue']);
        }

        // Calculate power (product of minimums)
        $power = $minRed * $minGreen * $minBlue;
        $sumPowers += $power;
    }

    return $sumPowers;
}

echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";
