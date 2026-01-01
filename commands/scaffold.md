Generate a SLOP scaffold for: $ARGUMENTS

## Instructions

Create SLOP code where implementations are stubbed out using `(hole ...)` expressions.

**CRITICAL: Use exact SLOP syntax shown below. SLOP is NOT Scheme/Lisp.**

Run `slop ref` for the complete language reference if needed.

## Required Syntax

### Module Structure

**CRITICAL: ALL definitions (types, functions, constants) must be INSIDE the module form.**

```slop
;; file: example.slop
(module example
  (export fn1 fn2)

  (type MyType (Int 0 ..))

  (fn my-function ((in x MyType))
    (@intent "Description")
    (@spec ((MyType) -> Int))
    (hole Int "implement this")))  ; <-- module closes AFTER all definitions
```

```slop
;; WRONG - definitions outside module:
(module example
  (export fn1))

(fn fn1 ...)  ; ERROR: outside module form
```

### Function Syntax (REQUIRED FORMAT)
```slop
;; CORRECT - use (fn name ((mode param Type)...) ...)
(fn add ((in a Int) (in b Int))
  (@intent "Add two numbers")
  (@spec ((Int Int) -> Int))
  (@pure)
  (@example (2 3) -> 5)
  (hole Int "add a and b"))

;; WRONG - do NOT use define, lambda, or other Lisp forms
;; (define (add a b) ...)     ; WRONG
;; (defun add (a b) ...)      ; WRONG
```

### Parameter Modes
```slop
(in x Type)    ; Read-only input (default)
(out x Type)   ; Write-only output
(mut x Type)   ; Read-write mutable
```

### Annotation Syntax (S-expressions, NOT comments)
```slop
(fn example ((in n Int))
  ;; Annotations are S-expressions INSIDE the function, not comments
  (@intent "Human-readable purpose")           ; REQUIRED
  (@spec ((Int) -> Int))                       ; REQUIRED - note double parens
  (@pre {n > 0})                               ; Precondition (infix preferred)
  (@post {$result >= 0})                       ; Postcondition ($result = return)
  (@pure)                                      ; No side effects
  (@example (5) -> 10)                         ; Use -> not =>
  (@example (0) -> 0)
  (hole Int "implementation"))
```

### @spec Format
```slop
;; Format: (@spec ((ParamType1 ParamType2...) -> ReturnType))
(@spec ((Int) -> Int))                    ; One param
(@spec ((Int Int) -> Bool))               ; Two params
(@spec ((String) -> (Result Int Error)))  ; Result type
(@spec (() -> Unit))                      ; No params
```

### @example Format
```slop
;; Format: (@example (arg1 arg2...) -> result)
(@example (5) -> 10)                      ; Single arg
(@example (2 3) -> 5)                     ; Multiple args
(@example ("hello") -> 5)                 ; String arg
(@example () -> 0)                        ; No args

;; WRONG:
;; (@example (5) => 10)    ; Wrong arrow
;; @example (5) -> 10      ; Missing parens
```

### Hole Syntax
```slop
(hole ReturnType "description"
  :complexity tier-N        ; tier-1 (trivial) to tier-4 (complex)
  :context (var1 var2 fn1)  ; identifiers the implementation may use
  :required (fn1))          ; identifiers that MUST appear
```

## Complete Example

```slop
;; factorial.slop
(module factorial
  (export factorial main)

  (type Natural (Int 0 ..))

  (fn factorial ((in n Natural))
    (@intent "Calculate factorial of n")
    (@spec ((Natural) -> Natural))
    (@pre {n >= 0})
    (@post {$result >= 1})
    (@pure)
    (@example (0) -> 1)
    (@example (1) -> 1)
    (@example (5) -> 120)
    (hole Natural "calculate n factorial recursively or iteratively"
      :complexity tier-2
      :context (n factorial)
      :required ()))

  (fn main ()
    (@intent "Print factorial of 10")
    (@spec (() -> Int))
    (hole Int "print factorial of 10 and return 0"
      :complexity tier-1
      :context (factorial println)
      :required (factorial))))
```

## Guidelines

- **ALL definitions INSIDE module**: Every `(type)`, `(fn)`, `(const)` must be inside `(module ... )`
- Module name MUST match the filename (e.g., `foo.slop` contains `(module foo ...)`)
- Define named types, then use them in signatures: `(type Age (Int 0 .. 150))` then `(in age Age)`
- Quote enum variants in code: `'active` not `active`
- Use `:context` to whitelist available identifiers
- Use `:required` only for functions that MUST be called
- Entry point `main` must return `Int` (exit code), not `Unit`
- **File placement**: If a `src/` directory exists in the project, place generated module files there (e.g., `src/mymodule.slop`). Otherwise, place files in the project root.

## Output

Generate the complete `.slop` file following the exact syntax above.
