Verify contracts in: $ARGUMENTS

## Instructions

Run `slop verify` on the specified SLOP file(s) to check contract consistency using Z3.

### Process

1. Run the verifier:
   ```bash
   slop verify <file.slop>
   ```

2. Analyze the output:
   - **VERIFIED**: Contract is provably consistent
   - **FAILED**: Contract violation found - analyze the counterexample
   - **UNKNOWN**: Z3 couldn't determine (may need simplification)
   - **WARNING**: Issue detected that the verifier can't fully check yet (future capability)

3. For each failure or warning, provide:
   - The specific contract that failed (`@pre`, `@post`, or range type)
   - The counterexample values if provided
   - A suggested fix

### Common Failure Patterns

**Postcondition not satisfied by implementation:**
```slop
;; Problem: @post claims result > 0, but implementation can return 0
(@post {$result > 0})
(if (> x 0) x 0)  ; Returns 0 when x <= 0

;; Fix: Adjust postcondition or implementation
(@post {$result >= 0})  ; Weaker postcondition
;; OR
(if (> x 0) x 1)  ; Ensure result > 0
```

**Precondition too weak:**
```slop
;; Problem: Division by zero possible
(@pre {y >= 0})  ; Allows y = 0
(/ x y)

;; Fix: Strengthen precondition
(@pre {y > 0})
```

**Range type violation:**
```slop
;; Problem: Result can exceed range bounds
(type Percentage (Int 0 .. 100))
(fn calc ((in x Int))
  (@spec ((Int) -> Percentage))
  (* x 2))  ; Can exceed 100

;; Fix: Clamp or validate
(if (> (* x 2) 100) 100 (* x 2))
```

### Handling Warnings

Warnings indicate issues the verifier detected but can't fully verify yet:
- Review the warning message carefully
- Consider if the code could violate the indicated property
- Fix proactively even if not a hard failure

### Suggesting Fixes

When suggesting fixes:
1. Prefer strengthening `@pre` over weakening `@post` (fail fast)
2. If postcondition is wrong, fix the implementation to match intent
3. For range violations, add clamping or validation logic
4. Consider if the `@intent` matches the contract - maybe the contract is wrong

### Output

For each file:
1. Report verification status (verified/failed/warning counts)
2. For failures: explain what failed and suggest a fix
3. For warnings: explain the concern and recommend action
4. Offer to apply fixes if straightforward
