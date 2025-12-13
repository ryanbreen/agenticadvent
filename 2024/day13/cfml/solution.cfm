<cfscript>
// Day 13: Claw Contraption - ColdFusion Solution

function parseMachines(text) {
    var machines = [];
    // Split by double newline (blank line separator)
    var blocks = reMatch("(?s)Button A:.*?Prize:.*?(?=\n\n|\Z)", text);

    for (var block in blocks) {
        // Extract all numbers from each line
        var lines = listToArray(trim(block), chr(10));

        // Button A: X+ax, Y+ay
        var aNums = reMatch("\d+", lines[1]);
        var ax = val(aNums[1]);
        var ay = val(aNums[2]);

        // Button B: X+bx, Y+by
        var bNums = reMatch("\d+", lines[2]);
        var bx = val(bNums[1]);
        var by = val(bNums[2]);

        // Prize: X=px, Y=py
        var pNums = reMatch("\d+", lines[3]);
        var px = val(pNums[1]);
        var py = val(pNums[2]);

        arrayAppend(machines, {
            ax: ax,
            ay: ay,
            bx: bx,
            by: by,
            px: px,
            py: py
        });
    }

    return machines;
}

function solveMachine(ax, ay, bx, by, px, py, maxPresses = javaCast("null", "")) {
    /*
     * Solve for button presses using Cramer's rule.
     *
     * System of equations:
     *   a*ax + b*bx = px
     *   a*ay + b*by = py
     *
     * Solution:
     *   det = ax*by - ay*bx
     *   a = (px*by - py*bx) / det
     *   b = (ax*py - ay*px) / det
     *
     * Returns token cost (3*a + b) or null if no valid solution.
     */

    // Use BigInteger for large number arithmetic
    var BigInteger = createObject("java", "java.math.BigInteger");

    var axBig = BigInteger.valueOf(javaCast("long", ax));
    var ayBig = BigInteger.valueOf(javaCast("long", ay));
    var bxBig = BigInteger.valueOf(javaCast("long", bx));
    var byBig = BigInteger.valueOf(javaCast("long", by));
    var pxBig = BigInteger.init(javaCast("string", px));
    var pyBig = BigInteger.init(javaCast("string", py));

    // det = ax*by - ay*bx
    var det = axBig.multiply(byBig).subtract(ayBig.multiply(bxBig));

    if (det.equals(BigInteger.ZERO)) {
        return javaCast("null", "");  // No unique solution
    }

    // aNum = px*by - py*bx
    var aNum = pxBig.multiply(byBig).subtract(pyBig.multiply(bxBig));
    // bNum = ax*py - ay*px
    var bNum = axBig.multiply(pyBig).subtract(ayBig.multiply(pxBig));

    // Check if solutions are integers (use remainder method)
    var remainder1 = aNum.remainder(det);
    var remainder2 = bNum.remainder(det);
    if (!remainder1.equals(BigInteger.ZERO) || !remainder2.equals(BigInteger.ZERO)) {
        return javaCast("null", "");
    }

    // a = aNum / det, b = bNum / det
    var a = aNum.divide(det);
    var b = bNum.divide(det);

    // Check non-negative
    if (a.compareTo(BigInteger.ZERO) < 0 || b.compareTo(BigInteger.ZERO) < 0) {
        return javaCast("null", "");
    }

    // Check max presses constraint (Part 1)
    if (!isNull(maxPresses)) {
        var maxPressesBig = BigInteger.valueOf(javaCast("long", maxPresses));
        if (a.compareTo(maxPressesBig) > 0 || b.compareTo(maxPressesBig) > 0) {
            return javaCast("null", "");
        }
    }

    // cost = 3*a + b
    var three = BigInteger.valueOf(3);
    var cost = three.multiply(a).add(b);

    return cost.toString();
}

function part1(inputText) {
    // Part 1: Max 100 presses per button
    var machines = parseMachines(inputText);
    var BigInteger = createObject("java", "java.math.BigInteger");
    var total = BigInteger.ZERO;

    for (var machine in machines) {
        var cost = solveMachine(
            machine.ax, machine.ay,
            machine.bx, machine.by,
            machine.px, machine.py,
            100
        );

        if (!isNull(cost)) {
            total = total.add(BigInteger.init(cost));
        }
    }

    return total.toString();
}

function part2(inputText) {
    // Part 2: Prize coordinates shifted by 10^13, no press limit
    var machines = parseMachines(inputText);
    var BigInteger = createObject("java", "java.math.BigInteger");
    var offset = BigInteger.init("10000000000000");  // 10^13
    var total = BigInteger.ZERO;

    for (var machine in machines) {
        // Shift prize coordinates
        var pxShifted = BigInteger.valueOf(javaCast("long", machine.px)).add(offset);
        var pyShifted = BigInteger.valueOf(javaCast("long", machine.py)).add(offset);

        var cost = solveMachine(
            machine.ax, machine.ay,
            machine.bx, machine.by,
            pxShifted.toString(), pyShifted.toString(),
            javaCast("null", "")
        );

        if (!isNull(cost)) {
            total = total.add(BigInteger.init(cost));
        }
    }

    return total.toString();
}

// Main execution
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputText = fileRead(inputPath);

writeOutput("Part 1: " & part1(inputText) & chr(10));
writeOutput("Part 2: " & part2(inputText) & chr(10));
</cfscript>
