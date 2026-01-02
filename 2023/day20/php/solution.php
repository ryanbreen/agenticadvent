<?php
/**
 * Day 20: Pulse Propagation - Module communication simulation.
 */

/**
 * Parse module configuration from input.
 */
function parseInput(string $filename): array {
    $modules = [];
    $lines = explode("\n", trim(file_get_contents($filename)));

    foreach ($lines as $line) {
        list($namePart, $destPart) = explode(' -> ', $line);
        $destinations = array_map('trim', explode(',', $destPart));

        if ($namePart === 'broadcaster') {
            $modules['broadcaster'] = [
                'type' => 'broadcaster',
                'destinations' => $destinations
            ];
        } elseif ($namePart[0] === '%') {
            $name = substr($namePart, 1);
            $modules[$name] = [
                'type' => 'flip-flop',
                'destinations' => $destinations,
                'state' => false
            ];
        } elseif ($namePart[0] === '&') {
            $name = substr($namePart, 1);
            $modules[$name] = [
                'type' => 'conjunction',
                'destinations' => $destinations,
                'memory' => []
            ];
        }
    }

    // Initialize conjunction memory for all inputs
    foreach ($modules as $name => $module) {
        foreach ($module['destinations'] as $dest) {
            if (isset($modules[$dest]) && $modules[$dest]['type'] === 'conjunction') {
                $modules[$dest]['memory'][$name] = false;
            }
        }
    }

    return $modules;
}

/**
 * Reset all module states.
 */
function resetModules(array &$modules): void {
    foreach ($modules as &$module) {
        if ($module['type'] === 'flip-flop') {
            $module['state'] = false;
        } elseif ($module['type'] === 'conjunction') {
            foreach ($module['memory'] as $key => $val) {
                $module['memory'][$key] = false;
            }
        }
    }
}

/**
 * Simulate a single button press.
 * Returns [low_count, high_count, high_senders_set]
 */
function simulateButtonPress(array &$modules, array $watchNodes = []): array {
    $lowCount = 0;
    $highCount = 0;
    $highSenders = [];

    // Queue: [source, destination, pulse] where pulse is true for high, false for low
    $queue = [['button', 'broadcaster', false]];

    while (!empty($queue)) {
        $item = array_shift($queue);
        list($source, $dest, $pulse) = $item;

        if ($pulse) {
            $highCount++;
        } else {
            $lowCount++;
        }

        // Track if watched nodes send high pulses
        if (!empty($watchNodes) && in_array($source, $watchNodes, true) && $pulse) {
            $highSenders[$source] = true;
        }

        if (!isset($modules[$dest])) {
            continue;
        }

        $module = &$modules[$dest];

        if ($module['type'] === 'broadcaster') {
            foreach ($module['destinations'] as $nextDest) {
                $queue[] = [$dest, $nextDest, $pulse];
            }
        } elseif ($module['type'] === 'flip-flop') {
            if (!$pulse) {  // Only react to low pulses
                $module['state'] = !$module['state'];
                foreach ($module['destinations'] as $nextDest) {
                    $queue[] = [$dest, $nextDest, $module['state']];
                }
            }
        } elseif ($module['type'] === 'conjunction') {
            $module['memory'][$source] = $pulse;
            // Send low if all inputs are high, otherwise send high
            $allHigh = true;
            foreach ($module['memory'] as $val) {
                if (!$val) {
                    $allHigh = false;
                    break;
                }
            }
            $output = !$allHigh;
            foreach ($module['destinations'] as $nextDest) {
                $queue[] = [$dest, $nextDest, $output];
            }
        }
    }

    return [$lowCount, $highCount, array_keys($highSenders)];
}

/**
 * Part 1: Count pulses after 1000 button presses.
 */
function part1(array $modules): int {
    resetModules($modules);

    $totalLow = 0;
    $totalHigh = 0;

    for ($i = 0; $i < 1000; $i++) {
        list($low, $high, $_) = simulateButtonPress($modules);
        $totalLow += $low;
        $totalHigh += $high;
    }

    return $totalLow * $totalHigh;
}

/**
 * GCD using GMP for large numbers.
 */
function gmpGcd($a, $b) {
    return gmp_gcd($a, $b);
}

/**
 * LCM using GMP for large numbers.
 */
function gmpLcm($a, $b) {
    return gmp_div_q(gmp_mul($a, $b), gmpGcd($a, $b));
}

/**
 * Part 2: Find minimum button presses for rx to receive a low pulse.
 */
function part2(array $modules): string {
    resetModules($modules);

    // Find the module that feeds into rx
    $rxInput = null;
    foreach ($modules as $name => $module) {
        if (in_array('rx', $module['destinations'], true)) {
            $rxInput = $name;
            break;
        }
    }

    if ($rxInput === null) {
        return "0";
    }

    // Find all modules that feed into rx_input
    $watchNodes = array_keys($modules[$rxInput]['memory']);
    $cycleLengths = [];

    $buttonPress = 0;
    while (count($cycleLengths) < count($watchNodes)) {
        $buttonPress++;
        list($_, $_, $highSenders) = simulateButtonPress($modules, $watchNodes);

        foreach ($highSenders as $node) {
            if (!isset($cycleLengths[$node])) {
                $cycleLengths[$node] = $buttonPress;
            }
        }
    }

    // LCM of all cycle lengths using GMP
    $result = gmp_init(1);
    foreach ($cycleLengths as $length) {
        $result = gmpLcm($result, gmp_init($length));
    }

    return gmp_strval($result);
}

// Main execution
$inputFile = __DIR__ . '/../input.txt';
$modules = parseInput($inputFile);
echo "Part 1: " . part1($modules) . "\n";

// Re-parse for part 2 (fresh state)
$modules = parseInput($inputFile);
echo "Part 2: " . part2($modules) . "\n";
