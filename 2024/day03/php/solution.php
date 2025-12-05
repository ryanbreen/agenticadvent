#!/usr/bin/env php
<?php

function part1(string $data): int {
    /**
     * Find all valid mul(X,Y) instructions and sum their products.
     */
    $pattern = '/mul\((\d{1,3}),(\d{1,3})\)/';
    preg_match_all($pattern, $data, $matches, PREG_SET_ORDER);

    $total = 0;
    foreach ($matches as $match) {
        $x = (int)$match[1];
        $y = (int)$match[2];
        $total += $x * $y;
    }

    return $total;
}

function part2(string $data): int {
    /**
     * Like part1, but do() enables and don't() disables mul instructions.
     */
    $mul_pattern = '/mul\((\d{1,3}),(\d{1,3})\)/';
    $do_pattern = '/do\(\)/';
    $dont_pattern = '/don\'t\(\)/';

    $total = 0;
    $enabled = true;

    // Build a list of all events with positions
    $events = [];

    // Find all mul instructions
    preg_match_all($mul_pattern, $data, $mul_matches, PREG_OFFSET_CAPTURE);
    for ($i = 0; $i < count($mul_matches[0]); $i++) {
        $pos = $mul_matches[0][$i][1];
        $x = (int)$mul_matches[1][$i][0];
        $y = (int)$mul_matches[2][$i][0];
        $events[] = [$pos, 'mul', $x, $y];
    }

    // Find all do() instructions
    preg_match_all($do_pattern, $data, $do_matches, PREG_OFFSET_CAPTURE);
    foreach ($do_matches[0] as $match) {
        $pos = $match[1];
        $events[] = [$pos, 'do', 0, 0];
    }

    // Find all don't() instructions
    preg_match_all($dont_pattern, $data, $dont_matches, PREG_OFFSET_CAPTURE);
    foreach ($dont_matches[0] as $match) {
        $pos = $match[1];
        $events[] = [$pos, 'dont', 0, 0];
    }

    // Sort by position
    usort($events, function($a, $b) {
        return $a[0] <=> $b[0];
    });

    // Process events in order
    foreach ($events as $event) {
        list($pos, $event_type, $x, $y) = $event;

        if ($event_type === 'do') {
            $enabled = true;
        } elseif ($event_type === 'dont') {
            $enabled = false;
        } elseif ($event_type === 'mul' && $enabled) {
            $total += $x * $y;
        }
    }

    return $total;
}

function main() {
    $input_path = __DIR__ . '/../input.txt';
    $data = file_get_contents($input_path);

    echo 'Part 1: ' . part1($data) . "\n";
    echo 'Part 2: ' . part2($data) . "\n";
}

main();
