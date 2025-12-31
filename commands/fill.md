Fill the holes in: $ARGUMENTS

## Instructions

Read the specified SLOP file and fill each `(hole ...)` expression with a valid implementation.

**CRITICAL: Use exact SLOP syntax. SLOP is NOT Scheme/Lisp.**

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

- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `<`, `>`, `<=`, `>=`, `==`, `!=`
- Logic: `and`, `or`, `not`
- Control: `if`, `cond`, `match`, `let`, `do`, `when`, `unless`
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

1. Read the file
2. For each hole:
   - Parse the hole's Type, prompt, :context, and :required
   - Review the enclosing function's @intent, @spec, @pre, @post, @example
   - Generate an implementation using ONLY :context identifiers and built-ins
   - Ensure all :required identifiers appear in the output
   - **Validate the implementation** by running:
     ```bash
     slop check-hole '<implementation>' -t '<HoleType>' -c <original-file.slop>
     ```
   - If validation fails, fix the implementation and re-validate
3. Only after ALL holes pass validation, write the filled file
4. Run `slop check` on the final file to confirm

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
