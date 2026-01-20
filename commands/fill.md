Fill the holes in: $ARGUMENTS

## Instructions

Read the specified SLOP file and fill each `(hole ...)` expression with a valid implementation.

**CRITICAL: Use exact SLOP syntax. SLOP is NOT Scheme/Lisp.**

## Reference Resources

If `SLOP_HOME` is set, consult the standard library and examples for patterns:

```bash
# Standard library for implementation patterns
slop doc $SLOP_HOME/lib/std/io/file.slop         # File I/O
slop doc $SLOP_HOME/lib/std/strlib/strlib.slop   # String utilities
slop doc $SLOP_HOME/lib/std/thread/thread.slop   # Concurrency

# Examples and spec
ls $SLOP_HOME/examples/
cat $SLOP_HOME/spec/REFERENCE.md                 # Quick reference
```

## Parallel Hole Filling with Tiered Models

Fill holes in parallel using the Task tool, with model selection based on complexity tier:

| Tier | Model | Description |
|------|-------|-------------|
| tier-1 | haiku | Trivial implementations (simple arithmetic, field access) |
| tier-2 | sonnet | Moderate complexity (loops, conditionals) |
| tier-3 | sonnet | Higher complexity (nested logic, multiple steps) |
| tier-4 | opus | Complex algorithms (fallback to sonnet if opus unavailable) |

### Execution Strategy

1. **Parse all holes** from the file first
2. **Group by tier** and launch parallel Task agents for each hole
3. **Each agent must**:
   - Generate an implementation respecting :context and :required
   - Run `slop check-hole` to validate
   - Return the validated implementation or error status

### Escalation on Failure

If a model fails to produce a valid implementation after **2 attempts**, escalate to the next higher model:

- haiku (tier-1) → sonnet → opus
- sonnet (tier-2/3) → opus
- opus (tier-4) → report failure (no higher model available)

### Task Agent Prompt Template

For each hole, spawn a Task agent with:

```
Fill this SLOP hole:

Hole: (hole <Type> "<prompt>" :context (<ids>) :required (<ids>))
File: <filename.slop>
Function context: <enclosing function with annotations>

Generate a valid implementation using ONLY:
- Identifiers in :context
- Built-in operators
- New bindings you create (let, for, match)

ALL identifiers in :required MUST appear in your implementation.

After generating, validate with:
slop check-hole '<your-implementation>' -t '<Type>' -c <filename.slop>

If validation fails, fix and retry. Return ONLY the validated implementation or "FAILED" if unable after 2 attempts.
```

## MANDATORY: Validate Every Hole

You MUST run `slop check-hole` for EVERY hole implementation before writing the file.

```bash
slop check-hole '<implementation>' -t '<HoleType>' -c <original-file.slop>
```

- Do NOT skip this step
- Do NOT write the file until ALL holes pass validation
- If validation fails, fix the implementation and re-validate
- Skipping validation will result in type errors and broken code

## Understanding :context and :required

**These are STRICT constraints, not suggestions.**

### :context is an EXHAUSTIVE WHITELIST

The `:context` attribute lists the ONLY identifiers you may use (besides built-ins).

```slop
(hole Int "compute result"
  :context (x y helper-fn))
```

This means you can ONLY use:
- `x` - a variable in scope
- `y` - a variable in scope
- `helper-fn` - a function defined elsewhere
- Built-in operators (`+`, `-`, `if`, `let`, etc.)

**You CANNOT use:**
- Variables not listed (even if they seem like they should exist)
- Functions not listed (even standard library functions)
- Names you invent (no `temp`, `result`, `i`, `acc` unless in context)

### :required means MUST APPEAR

The `:required` attribute lists identifiers that MUST appear in your implementation.

```slop
(hole Unit "save the data"
  :context (state data save-to-db)
  :required (save-to-db))
```

Your implementation MUST call `save-to-db`. This is not optional.

### Example: Correct vs Wrong

Given this hole:
```slop
(fn calculate ((in x Int) (in y Int))
  (@spec ((Int Int) -> Int))
  (hole Int "compute using x and y"
    :context (x y helper)
    :required (helper)))
```

**CORRECT** - uses only context identifiers + built-ins:
```slop
(+ (helper x) y)
```
- `x` ✓ in :context
- `y` ✓ in :context
- `helper` ✓ in :context (and in :required, so must appear)
- `+` ✓ built-in

**WRONG** - references things not in context:
```slop
(+ (compute x) (* y scale))
```
- `compute` ✗ NOT in :context (you invented this function)
- `scale` ✗ NOT in :context (you invented this variable)

**WRONG** - missing required identifier:
```slop
(+ x y)
```
- Missing `helper` which is in :required

### What counts as "in :context"?

**Must be in :context to use:**
- Function parameters from outer scope
- Other functions defined in the module
- Global constants
- Any identifier that exists outside this hole

**Does NOT need to be in :context (you create these):**
- Variables you bind with `let`: `(let ((total 0)) ...)`
- Loop variables: `(for (i 0 n) ...)`, `(for-each (item list) ...)`
- Match bindings: `(match x ((ok val) val))`

### Pre-Fill Checklist

Before writing each implementation, explicitly verify:

1. **List the :context identifiers** - these are your ONLY allowed external references
2. **List the :required identifiers** - your implementation MUST use all of these
3. **Check each identifier you use** - is it in :context, a built-in, or a new binding you create?

## Constraints Summary

1. **Type**: Implementation must return the hole's declared type
2. **:context**: ONLY use listed identifiers + built-ins + new bindings you create
3. **:required**: ALL listed identifiers MUST appear in your output
4. **Contracts**: Must satisfy `@pre`, `@post`, and `@example` annotations

## Structural Editing Awareness

When filling holes, maintain balanced structure and follow PAREDIT MODE rules.

### Hole Replacement is Atomic

Treat the `(hole ...)` expression and its replacement as an atomic swap:

```
BEFORE: →(hole Int "compute sum" :context (items))←
AFTER:  →(let ((mut sum 0))
           (for-each (x items)
             (set! sum (+ sum x)))
           sum)←
```

The replacement expression must be syntactically complete and balanced.

### Paren Audit After Generation

After generating each implementation, perform a paren audit:

```
<slop-paren-audit>
Implementation for "compute sum":
(let ((mut sum 0))         →  ( = 2, ) = 1
  (for-each (x items)      →  ( = 1, ) = 1
    (set! sum (+ sum x)))  →  ( = 2, ) = 3
  sum)                     →  ( = 0, ) = 1
Total: ( = 5, ) = 5 ✓
</slop-paren-audit>
```

### Preserve Surrounding Structure

When replacing a hole:
- Keep all `@`-annotations intact
- Maintain the function's overall structure
- Ensure the replacement integrates cleanly with surrounding code

## SLOP Implementation Syntax

### Conditionals
```slop
;; if-then-else
(if (> x 0) (+ x 1) (- x 1))

;; multi-branch
(cond
  ((< x 0) "negative")
  ((== x 0) "zero")
  (else "positive"))

;; pattern matching
(match result
  ((ok val) (use val))
  ((error e) (handle e)))

;; simple enum matching - bare variant names
(match status
  (Active (do-active))
  (Pending (do-pending)))
```

### Let Bindings
```slop
;; Immutable bindings (default)
(let ((x 10)) (+ x 1))

;; Multiple immutable bindings
(let ((x 10)
      (y 20))
  (+ x y))

;; Mutable binding - required for set!
(let ((mut counter 0))
  (set! counter (+ counter 1))
  counter)

;; Mutable with explicit type
(let ((mut result Int 0))
  (set! result (compute))
  result)
```

**Important**: `set!` on a variable requires `mut` in its binding.

### Loops
```slop
;; for range with accumulator (needs mut)
(let ((mut sum 0))
  (for (i 0 n)
    (set! sum (+ sum i)))
  sum)

;; for-each over collection
(for-each (item items)
  (process item))

;; while loop (loop var needs mut)
(let ((mut i 0))
  (while (< i limit)
    (do
      (process i)
      (set! i (+ i 1)))))

;; early exit
(for (i 0 n)
  (when (found i)
    (return i)))
```

### Result and Option
```slop
;; Creating results - QUOTE the error variant!
(ok value)
(error 'not-found)      ; CORRECT: quoted
(error 'invalid-input)  ; CORRECT: quoted

;; Creating options
(some value)
(none)

;; Early return on error
(let ((val (? (might-fail x))))  ; returns early if error
  (use val))
```

### Field Access and Mutation
```slop
;; field access (works for records and pointers)
(. user name)
(. user email)

;; mutation
(set! user name "new-name")

;; array/list indexing
(@ items 0)
(set! (@ items i) new-value)
```

### List Operations
```slop
;; build a mutable list (mut required for list-push)
(let ((mut result (list-new arena Int)))
  (for (i 0 n)
    (list-push result (compute i)))
  result)

;; immutable list literal
(list Int 1 2 3 4 5)
```

### Sequence of Statements
```slop
;; use (do ...) to sequence multiple expressions
(do
  (step-one)
  (step-two)
  (final-result))
```

## Built-ins Reference

Run `slop ref` for the complete language reference.

- Arithmetic: `+`, `-`, `*`, `/`, `%`, `min`, `max`
- Comparison: `<`, `>`, `<=`, `>=`, `==`, `!=`
- Logic: `and`, `or`, `not`
- Control: `if`, `cond`, `match`, `let`, `let*`, `do`, `when`, `unless`
- Loops: `for`, `for-each`, `while`, `break`, `continue`, `return`
- Lists: `list-new`, `list-push`, `list-get`, `list-len`, `list`
- Maps: `map-new`, `map-put`, `map-get`, `map-has`, `map`
- Strings: `string-new`, `string-len`, `string-concat`, `string-eq`
- Results: `ok`, `error`, `is-ok`, `unwrap`, `?`
- Options: `some`, `none`, `is-some`, `is-none`
- Fields: `.`, `set!`, `@`

## What You Cannot Do

- Reference variables, functions, or types not in `:context`
- Use unquoted error variants: `(error not-found)` is WRONG
- Call user-defined functions not listed in `:context`

## Process

1. **Read and parse the file**
   - Run `slop parse <file> --holes` to list all holes with their tiers
   - Extract each hole's Type, prompt, :context, :required, and :complexity

2. **Launch parallel Task agents by tier**
   - Group holes by complexity tier
   - For each hole, spawn a Task agent with the appropriate model:
     - `model: "haiku"` for tier-1
     - `model: "sonnet"` for tier-2 and tier-3
     - `model: "opus"` for tier-4 (use sonnet if opus unavailable)
   - Include the enclosing function context (@intent, @spec, @pre, @post, @example)
   - Set `run_in_background: true` to run all holes in parallel

3. **Collect results and handle escalation**
   - Use TaskOutput to retrieve each agent's result
   - If an agent returns "FAILED" after 2 attempts:
     - Escalate to next model tier (haiku→sonnet→opus)
     - Spawn a new Task agent with the higher model
   - Track which holes succeeded and which need escalation

4. **Validate all implementations**
   - For each returned implementation, verify it passed `slop check-hole`
   - If any validation is missing or failed, re-run validation

5. **Write the filled file**
   - **ONLY after ALL holes have validated implementations**, write the file
   - Replace each `(hole ...)` with its validated implementation
   - Keep all annotations, types, and structure intact

6. **Final verification**
   - Run `slop check` on the completed file to confirm type correctness

### Example: Parallel Execution

Given a file with 3 holes:
```slop
(hole Int "trivial add" :complexity tier-1 ...)      ; → haiku
(hole Bool "moderate check" :complexity tier-2 ...)  ; → sonnet
(hole Result "complex algo" :complexity tier-4 ...)  ; → opus
```

Launch 3 Task agents in parallel (single message, multiple tool calls):
- Task 1: model=haiku, hole 1
- Task 2: model=sonnet, hole 2
- Task 3: model=opus, hole 3

If Task 1 (haiku) fails twice, spawn Task 4 with model=sonnet for that hole.

### Validation Example

For a hole like:
```slop
(hole Int "add one to x" :context (x))
```

With implementation `(+ x 1)`, validate with:
```bash
slop check-hole '(+ x 1)' -t Int -c original.slop
```

### Output

Replace each `(hole ...)` with its implementation. Keep all annotations, types, and structure intact. Only write the file after all holes pass validation.
