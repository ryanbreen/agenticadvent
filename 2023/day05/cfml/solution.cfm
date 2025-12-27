<cfscript>
/**
 * Advent of Code 2023 - Day 5: If You Give A Seed A Fertilizer
 * ColdFusion (CFML) Solution
 */

// Read input file
inputPath = getDirectoryFromPath(getCurrentTemplatePath()) & "../input.txt";
inputText = fileRead(inputPath);

// Parse the input
function parseInput(text) {
    // Normalize line endings and split by double newline
    var normalizedText = replace(text, chr(13), "", "all");
    var sections = [];
    var currentSection = "";
    var lines = listToArray(normalizedText, chr(10), true);

    for (var i = 1; i <= arrayLen(lines); i++) {
        var line = lines[i];
        if (len(trim(line)) == 0) {
            if (len(trim(currentSection)) > 0) {
                arrayAppend(sections, trim(currentSection));
                currentSection = "";
            }
        } else {
            if (len(currentSection) > 0) {
                currentSection = currentSection & chr(10) & line;
            } else {
                currentSection = line;
            }
        }
    }
    if (len(trim(currentSection)) > 0) {
        arrayAppend(sections, trim(currentSection));
    }

    var result = {};

    // Parse seeds
    var seedLine = sections[1];
    var seedPart = listRest(seedLine, ": ");
    result.seeds = [];
    var seedNumbers = listToArray(trim(seedPart), " ");
    for (var s in seedNumbers) {
        if (len(trim(s)) > 0) {
            arrayAppend(result.seeds, javaCast("long", trim(s)));
        }
    }

    // Parse maps
    result.maps = [];
    for (var i = 2; i <= arrayLen(sections); i++) {
        var section = sections[i];
        var sectionLines = listToArray(trim(section), chr(10));
        var ranges = [];

        // Skip header line (index 1), process range lines
        for (var j = 2; j <= arrayLen(sectionLines); j++) {
            var parts = listToArray(trim(sectionLines[j]), " ");
            if (arrayLen(parts) == 3) {
                arrayAppend(ranges, {
                    dstStart: javaCast("long", trim(parts[1])),
                    srcStart: javaCast("long", trim(parts[2])),
                    length: javaCast("long", trim(parts[3]))
                });
            }
        }
        arrayAppend(result.maps, ranges);
    }

    return result;
}

// Apply a single map to transform a value
function applyMap(value, ranges) {
    for (var range in ranges) {
        if (value >= range.srcStart && value < range.srcStart + range.length) {
            return range.dstStart + (value - range.srcStart);
        }
    }
    return value;
}

// Convert seed to location through all maps
function seedToLocation(seed, maps) {
    var value = seed;
    for (var mapRanges in maps) {
        value = applyMap(value, mapRanges);
    }
    return value;
}

// Part 1: Find lowest location for individual seeds
function part1(seeds, maps) {
    var minLocation = seedToLocation(seeds[1], maps);
    for (var i = 2; i <= arrayLen(seeds); i++) {
        var loc = seedToLocation(seeds[i], maps);
        if (loc < minLocation) {
            minLocation = loc;
        }
    }
    return minLocation;
}

// Apply map to ranges for Part 2
function applyMapToRanges(inputRanges, mapRanges) {
    var result = [];

    for (var inputRange in inputRanges) {
        var remaining = [{ start: inputRange.start, end: inputRange.end }];

        for (var mapRange in mapRanges) {
            var srcEnd = mapRange.srcStart + mapRange.length;
            var newRemaining = [];

            for (var r in remaining) {
                // Part before the map range (unmapped)
                if (r.start < mapRange.srcStart) {
                    arrayAppend(newRemaining, {
                        start: r.start,
                        end: min(r.end, mapRange.srcStart)
                    });
                }

                // Part within the map range (mapped)
                var overlapStart = max(r.start, mapRange.srcStart);
                var overlapEnd = min(r.end, srcEnd);
                if (overlapStart < overlapEnd) {
                    var offset = mapRange.dstStart - mapRange.srcStart;
                    arrayAppend(result, {
                        start: overlapStart + offset,
                        end: overlapEnd + offset
                    });
                }

                // Part after the map range (unmapped)
                if (r.end > srcEnd) {
                    arrayAppend(newRemaining, {
                        start: max(r.start, srcEnd),
                        end: r.end
                    });
                }
            }

            remaining = newRemaining;
        }

        // Any remaining parts are unmapped (identity)
        for (var rem in remaining) {
            arrayAppend(result, rem);
        }
    }

    return result;
}

// Part 2: Find lowest location for seed ranges
function part2(seeds, maps) {
    // Convert seeds to ranges: pairs of (start, start + length)
    var ranges = [];
    for (var i = 1; i <= arrayLen(seeds); i += 2) {
        var rangeStart = seeds[i];
        var rangeLength = seeds[i + 1];
        arrayAppend(ranges, {
            start: rangeStart,
            end: rangeStart + rangeLength
        });
    }

    // Apply each map to the ranges
    for (var mapRanges in maps) {
        ranges = applyMapToRanges(ranges, mapRanges);
    }

    // Find minimum start of any range
    var minStart = ranges[1].start;
    for (var i = 2; i <= arrayLen(ranges); i++) {
        if (ranges[i].start < minStart) {
            minStart = ranges[i].start;
        }
    }
    return minStart;
}

// Main execution
parsed = parseInput(inputText);

result1 = part1(parsed.seeds, parsed.maps);
result2 = part2(parsed.seeds, parsed.maps);

writeOutput("Part 1: " & result1 & chr(10));
writeOutput("Part 2: " & result2 & chr(10));
</cfscript>
