Verify contracts in: $ARGUMENTS

## Instructions

Run `slop verify` on the specified SLOP file(s) to check contract consistency using Z3.

### Process

1. Run the verifier:
   ```bash
   slop verify <file.slop>
   slop verify <file.slop> --mode warn    # Warn instead of error
   slop verify <file.slop> --timeout 10   # Z3 timeout in seconds
   ```

2. Analyze the output:
   - **VERIFIED**: Contract is provably consistent
   - **FAILED**: Contract violation found - analyze the counterexample
   - **UNKNOWN**: Z3 couldn't determine (may need simplification)
   - **Verified via @assume (trusted)**: Axiom accepted without proof

3. For each failure, provide:
   - The specific contract that failed (`@pre`, `@post`, `@property`, `@invariant`, or range type)
   - The counterexample values if provided
   - A suggested fix

### What the Verifier Proves

The verifier checks each function independently with path-sensitive reasoning:

- **@post** — postconditions hold given preconditions
- **@property** — named properties hold universally
- **@invariant** — type invariants maintained
- **Range types** — bounds propagated through arithmetic
- **Record field axioms** — `(record-new Type (field value))` implies `(. $result field) == value`
- **Imported function postconditions** — called functions' `@post` become axioms for callers
- **Equality function semantics** — `*-eq` functions with `(@post (== $result (== a b)))` get Z3 axioms
- **Containment congruence** — nested loops over query results bridge element containment to constructor containment

### Automatic Loop Analysis

The verifier recognizes six loop patterns without needing `@loop-invariant`:

1. **Filter** — conditional push → `(list-len result) <= (list-len items)`
2. **Map/Transform** — unconditional push of constructed element → field correspondence axioms
3. **Count** — conditional increment → `0 <= count <= (list-len items)`
4. **Fold** — accumulation with operator → bounds from operator (e.g., max: `result >= init`)
5. **Find-First** — conditional assign when nil → result came from source
6. **Nested/Join** — inner loop over derived collection → field provenance (OUTER/INNER/CONSTANT)

Functions with `@property` but no explicit `@loop-invariant` automatically use the property body as the loop invariant (property auto-propagation).

### Escape Hatches

When automatic verification fails:

- **`@assume condition`** — Trust an assertion without proof (for FFI, complex invariants)
- **`@loop-invariant condition`** — Provide invariant for unrecognized loop patterns
- **`@callback-assume param expr`** — Declare properties of callback arguments (`$callback-arg` for each arg)
- **`@trusted`** — Skip verification entirely for a function

**Verifier-only predicates:**
- `(list-contains lst elem)` — membership test usable in `@post`, `@property`, `@loop-invariant`, `@assume` annotations only (no C codegen)

Use `@assume` sparingly — each is a potential source of unsoundness.

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

**Timeout:**
- Quantifier-heavy properties or non-linear arithmetic
- Fix: add `@loop-invariant`, simplify postconditions, break function into smaller pieces

**Unrecognized loop:**
- The verifier prints suggestions when it can't analyze a loop
- Fix: add `@loop-invariant` inside the loop body, or use `@assume`

### Suggesting Fixes

When suggesting fixes:
1. Prefer strengthening `@pre` over weakening `@post` (fail fast)
2. If postcondition is wrong, fix the implementation to match intent
3. For range violations, add clamping or validation logic
4. Consider if the `@intent` matches the contract - maybe the contract is wrong
5. For complex loops, consider adding `@loop-invariant` before resorting to `@assume`

### Output

For each file:
1. Report verification status (verified/failed/warning counts)
2. For failures: explain what failed and suggest a fix
3. For timeouts/unknowns: suggest simplification strategies
4. Offer to apply fixes if straightforward
