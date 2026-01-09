component {

    public function init() {
        variables.blueprints = parseInput();
        return this;
    }

    private array function parseInput() {
        var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
        var inputPath = scriptDir & "../input.txt";
        var content = fileRead(inputPath);
        var lines = listToArray(content, chr(10));
        var blueprints = [];

        for (var line in lines) {
            line = trim(line);
            if (len(line) == 0) continue;

            // Extract all numbers from the line
            var nums = [];
            var matcher = createObject("java", "java.util.regex.Pattern")
                .compile("\d+")
                .matcher(line);

            while (matcher.find()) {
                arrayAppend(nums, int(matcher.group()));
            }

            if (arrayLen(nums) >= 7) {
                arrayAppend(blueprints, {
                    id: nums[1],
                    oreOre: nums[2],
                    clayOre: nums[3],
                    obsOre: nums[4],
                    obsClay: nums[5],
                    geoOre: nums[6],
                    geoObs: nums[7]
                });
            }
        }

        return blueprints;
    }

    private numeric function maxGeodes(required struct bp, required numeric timeLimit) {
        var maxOre = max(bp.oreOre, max(bp.clayOre, max(bp.obsOre, bp.geoOre)));
        var maxClay = bp.obsClay;
        var maxObs = bp.geoObs;

        // Use a struct to hold best value (mutable reference)
        var state = { best: 0 };
        var seen = {};

        dfs(
            bp = bp,
            timeLimit = timeLimit,
            maxOre = maxOre,
            maxClay = maxClay,
            maxObs = maxObs,
            time = 0,
            ore = 0,
            clay = 0,
            obs = 0,
            geodes = 0,
            oreR = 1,
            clayR = 0,
            obsR = 0,
            geoR = 0,
            seen = seen,
            state = state
        );

        return state.best;
    }

    private void function dfs(
        required struct bp,
        required numeric timeLimit,
        required numeric maxOre,
        required numeric maxClay,
        required numeric maxObs,
        required numeric time,
        required numeric ore,
        required numeric clay,
        required numeric obs,
        required numeric geodes,
        required numeric oreR,
        required numeric clayR,
        required numeric obsR,
        required numeric geoR,
        required struct seen,
        required struct state
    ) {
        var remaining = timeLimit - time;

        // Upper bound pruning
        var upperBound = geodes + geoR * remaining + int((remaining * (remaining - 1)) / 2);
        if (upperBound <= state.best) return;

        // Base case: time's up
        if (time == timeLimit) {
            if (geodes > state.best) {
                state.best = geodes;
            }
            return;
        }

        // Cap resources to avoid redundant states
        var cappedOre = min(ore, remaining * maxOre);
        var cappedClay = min(clay, remaining * maxClay);
        var cappedObs = min(obs, remaining * maxObs);

        // State deduplication
        var key = "#time#,#cappedOre#,#cappedClay#,#cappedObs#,#oreR#,#clayR#,#obsR#,#geoR#";
        if (structKeyExists(seen, key) && seen[key] >= geodes) return;
        seen[key] = geodes;

        // Collect resources
        var newOre = cappedOre + oreR;
        var newClay = cappedClay + clayR;
        var newObs = cappedObs + obsR;
        var newGeodes = geodes + geoR;

        // Try building geode robot (always do if possible - and return early)
        if (cappedOre >= bp.geoOre && cappedObs >= bp.geoObs) {
            dfs(
                bp = bp,
                timeLimit = timeLimit,
                maxOre = maxOre,
                maxClay = maxClay,
                maxObs = maxObs,
                time = time + 1,
                ore = newOre - bp.geoOre,
                clay = newClay,
                obs = newObs - bp.geoObs,
                geodes = newGeodes,
                oreR = oreR,
                clayR = clayR,
                obsR = obsR,
                geoR = geoR + 1,
                seen = seen,
                state = state
            );
            return; // If we can build geode, always do it
        }

        // Try building obsidian robot
        if (cappedOre >= bp.obsOre && cappedClay >= bp.obsClay && obsR < maxObs) {
            dfs(
                bp = bp,
                timeLimit = timeLimit,
                maxOre = maxOre,
                maxClay = maxClay,
                maxObs = maxObs,
                time = time + 1,
                ore = newOre - bp.obsOre,
                clay = newClay - bp.obsClay,
                obs = newObs,
                geodes = newGeodes,
                oreR = oreR,
                clayR = clayR,
                obsR = obsR + 1,
                geoR = geoR,
                seen = seen,
                state = state
            );
        }

        // Try building clay robot
        if (cappedOre >= bp.clayOre && clayR < maxClay) {
            dfs(
                bp = bp,
                timeLimit = timeLimit,
                maxOre = maxOre,
                maxClay = maxClay,
                maxObs = maxObs,
                time = time + 1,
                ore = newOre - bp.clayOre,
                clay = newClay,
                obs = newObs,
                geodes = newGeodes,
                oreR = oreR,
                clayR = clayR + 1,
                obsR = obsR,
                geoR = geoR,
                seen = seen,
                state = state
            );
        }

        // Try building ore robot
        if (cappedOre >= bp.oreOre && oreR < maxOre) {
            dfs(
                bp = bp,
                timeLimit = timeLimit,
                maxOre = maxOre,
                maxClay = maxClay,
                maxObs = maxObs,
                time = time + 1,
                ore = newOre - bp.oreOre,
                clay = newClay,
                obs = newObs,
                geodes = newGeodes,
                oreR = oreR + 1,
                clayR = clayR,
                obsR = obsR,
                geoR = geoR,
                seen = seen,
                state = state
            );
        }

        // Do nothing (wait)
        dfs(
            bp = bp,
            timeLimit = timeLimit,
            maxOre = maxOre,
            maxClay = maxClay,
            maxObs = maxObs,
            time = time + 1,
            ore = newOre,
            clay = newClay,
            obs = newObs,
            geodes = newGeodes,
            oreR = oreR,
            clayR = clayR,
            obsR = obsR,
            geoR = geoR,
            seen = seen,
            state = state
        );
    }

    public numeric function part1() {
        var total = 0;
        for (var bp in variables.blueprints) {
            var geodes = maxGeodes(bp, 24);
            total += bp.id * geodes;
        }
        return total;
    }

    public numeric function part2() {
        var result = 1;
        var count = min(3, arrayLen(variables.blueprints));
        for (var i = 1; i <= count; i++) {
            var geodes = maxGeodes(variables.blueprints[i], 32);
            result *= geodes;
        }
        return result;
    }
}
