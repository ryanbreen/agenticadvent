<cfscript>
try {
    sol = new Solution();
    sol.run();
} catch (any e) {
    systemOutput("Error: " & e.message, true);
    systemOutput("Detail: " & e.detail, true);
}
</cfscript>
