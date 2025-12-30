<?php

function parseInput($text) {
    $blocks = array_filter(explode("\n\n", trim($text)));
    $patterns = [];
    foreach ($blocks as $block) {
        $patterns[] = explode("\n", $block);
    }
    return $patterns;
}

function findVerticalReflection($pattern) {
    if (empty($pattern)) {
        return 0;
    }
    $width = strlen($pattern[0]);

    for ($col = 1; $col < $width; $col++) {
        $isReflection = true;
        foreach ($pattern as $row) {
            // Split at column position
            $left = substr($row, 0, $col);
            $right = substr($row, $col);

            // Reverse left side for comparison
            $left = strrev($left);

            // Compare overlapping parts
            $minLen = min(strlen($left), strlen($right));
            if (substr($left, 0, $minLen) !== substr($right, 0, $minLen)) {
                $isReflection = false;
                break;
            }
        }
        if ($isReflection) {
            return $col;
        }
    }
    return 0;
}

function findHorizontalReflection($pattern) {
    if (empty($pattern)) {
        return 0;
    }
    $height = count($pattern);

    for ($row = 1; $row < $height; $row++) {
        $isReflection = true;

        // Split at row position
        $top = array_slice($pattern, 0, $row);
        $bottom = array_slice($pattern, $row);

        // Reverse top for comparison
        $top = array_reverse($top);

        // Compare overlapping parts
        $minLen = min(count($top), count($bottom));
        for ($i = 0; $i < $minLen; $i++) {
            if ($top[$i] !== $bottom[$i]) {
                $isReflection = false;
                break;
            }
        }
        if ($isReflection) {
            return $row;
        }
    }
    return 0;
}

function summarizePattern($pattern) {
    $v = findVerticalReflection($pattern);
    if ($v > 0) {
        return $v;
    }
    $h = findHorizontalReflection($pattern);
    return $h * 100;
}

function part1($patterns) {
    $sum = 0;
    foreach ($patterns as $pattern) {
        $sum += summarizePattern($pattern);
    }
    return $sum;
}

function countDifferences($s1, $s2) {
    $len = min(strlen($s1), strlen($s2));
    $diff = 0;
    for ($i = 0; $i < $len; $i++) {
        if ($s1[$i] !== $s2[$i]) {
            $diff++;
        }
    }
    return $diff;
}

function findVerticalReflectionWithSmudge($pattern) {
    if (empty($pattern)) {
        return 0;
    }
    $width = strlen($pattern[0]);

    for ($col = 1; $col < $width; $col++) {
        $totalDiff = 0;
        foreach ($pattern as $row) {
            $left = substr($row, 0, $col);
            $right = substr($row, $col);

            // Reverse left side for comparison
            $left = strrev($left);

            // Count differences in overlapping parts
            $minLen = min(strlen($left), strlen($right));
            $totalDiff += countDifferences(substr($left, 0, $minLen), substr($right, 0, $minLen));

            if ($totalDiff > 1) {
                break;
            }
        }
        if ($totalDiff === 1) {
            return $col;
        }
    }
    return 0;
}

function findHorizontalReflectionWithSmudge($pattern) {
    if (empty($pattern)) {
        return 0;
    }
    $height = count($pattern);

    for ($row = 1; $row < $height; $row++) {
        $totalDiff = 0;

        $top = array_slice($pattern, 0, $row);
        $bottom = array_slice($pattern, $row);

        // Reverse top for comparison
        $top = array_reverse($top);

        // Compare overlapping parts and count differences
        $minLen = min(count($top), count($bottom));
        for ($i = 0; $i < $minLen; $i++) {
            $totalDiff += countDifferences($top[$i], $bottom[$i]);
            if ($totalDiff > 1) {
                break;
            }
        }
        if ($totalDiff === 1) {
            return $row;
        }
    }
    return 0;
}

function summarizePatternWithSmudge($pattern) {
    $v = findVerticalReflectionWithSmudge($pattern);
    if ($v > 0) {
        return $v;
    }
    $h = findHorizontalReflectionWithSmudge($pattern);
    return $h * 100;
}

function part2($patterns) {
    $sum = 0;
    foreach ($patterns as $pattern) {
        $sum += summarizePatternWithSmudge($pattern);
    }
    return $sum;
}

// Main execution
$inputFile = __DIR__ . '/../input.txt';
$text = file_get_contents($inputFile);
$patterns = parseInput($text);

echo "Part 1: " . part1($patterns) . "\n";
echo "Part 2: " . part2($patterns) . "\n";
