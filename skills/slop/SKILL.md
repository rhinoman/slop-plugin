---
name: slop
description: |
  Generate code in SLOP (Symbolic LLM-Optimized Programming), a language designed for
  minimal human involvement in coding. Use when: (1) User asks for SLOP code, (2) Need
  strong contracts and range types, (3) Creating code with typed holes for incremental
  generation, (4) Generating efficient C code. SLOP uses S-expression syntax and transpiles to C.
---

# SLOP Language Skill

SLOP (Symbolic LLM-Optimized Programming) is a language designed for hybrid human-machine code generation. It uses S-expression syntax and transpiles to C.

## Resources

The `SLOP_HOME` environment variable points to the SLOP distribution. Resolution order: (1) `SLOP_HOME` if set, (2) package-relative paths as fallback.

| Location | Contents |
|----------|----------|
| `$SLOP_HOME/spec/LANGUAGE.md` | Complete language specification |
| `$SLOP_HOME/spec/REFERENCE.md` | Quick reference for code generation |
| `$SLOP_HOME/spec/VERIFICATION.md` | Verification guide |
| `$SLOP_HOME/lib/std/` | Standard library modules |
| `$SLOP_HOME/lib/compiler/` | Self-hosted compiler sources |
| `$SLOP_HOME/bin/` | Native compiler binaries |
| `$SLOP_HOME/examples/` | Code samples and reference implementations |

**Standard library modules:**

| Module | Path | Description |
|--------|------|-------------|
| io | `lib/std/io/file.slop` | File I/O operations |
| os | `lib/std/os/env.slop` | Environment variables |
| strlib | `lib/std/strlib/strlib.slop` | String utilities |
| math | `lib/std/math/mathlib.slop` | Math functions (FFI to math.h) |
| thread | `lib/std/thread/thread.slop` | Concurrency primitives |
| path | `lib/std/path/path.slop` | Path manipulation (dirname, join, basename, extension) |

## Tooling

SLOP is self-hosted: native compiler binaries are used by default. Use `--python` on most commands to fall back to the Python toolchain.

```bash
# Inspection and documentation
slop parse file.slop              # Parse and inspect AST
slop parse file.slop --holes      # Show holes in file
slop doc file.slop                # Generate documentation from annotations
slop ref                          # Full language reference (for AI)
slop ref types                    # Type system reference
slop ref --list                   # List available topics
slop paths                        # Show resolved SLOP paths

# Verification
slop check file.slop              # Type check
slop verify file.slop             # Verify contracts with Z3
slop verify file.slop --mode warn # Warn instead of error on failures
slop test file.slop               # Run @example and @property tests

# Building
slop transpile file.slop -o out.c # Transpile to C
slop build file.slop -o binary    # Full build (requires cc)
slop build file.slop --library static  # Build as static library
slop build file.slop --skip-check     # Skip type checking

# Code generation and formatting
slop fill file.slop -o filled.slop  # Fill holes with LLM
slop derive schema.json -o types.slop  # Generate types from schema
slop format file.slop               # Format SLOP source code
slop format file.slop --check       # Check formatting (CI)
```

## Philosophy

```
Humans: Specify WHAT (intent, contracts, types, examples)
Machines: Handle HOW (implementation, verification, compilation)
Type system: Catch errors early (range bounds, exhaustive matching, contract violations)
```

## SLOP PAREDIT MODE

When generating or editing SLOP code, follow these structural editing rules to ensure syntactically correct output.

### Rules

1. **Balanced parentheses** - ALL code must have perfectly balanced `(` and `)`. Count obsessively.

2. **Structural editing terminology** - Describe changes using structural operations BEFORE showing code:
   - **slurp**: Expand sexp to include next sibling
   - **barf**: Expel last child from sexp
   - **splice**: Remove surrounding parens, keeping children
   - **raise**: Replace parent with this sexp
   - **wrap**: Surround expression with new parens
   - **unwrap**: Remove one level of parens
   - **kill-sexp**: Delete entire balanced expression

3. **`<slop-paren-audit>` requirement** - Before ANY SLOP code block, insert an audit:
   ```
   <slop-paren-audit>
   Line 1: ( = 3, ) = 3
   Line 2: ( = 2, ) = 2
   Total: ( = 5, ) = 5 ✓
   </slop-paren-audit>
   ```

4. **BEFORE/AFTER structure** - When editing existing code, show markers:
   ```
   BEFORE: →( target-sexp )←
   AFTER:  →( new-sexp )←
   ```

5. **Atomic units** - Treat these as indivisible:
   - `@`-forms: `(@intent ...)`, `(@spec ...)`, `(@pre ...)`, etc.
   - `(hole ...)` expressions
   - `(fn ...)`, `(type ...)`, `(module ...)`, `(ffi ...)` declarations

6. **Infix `{}` as unit** - Treat entire `{infix conditions}` as one balanced sexp unit:
   ```slop
   (@pre {x > 0 and x < 100})  ; {} is atomic
   ```

7. **Refuse risky edits** - If a requested edit risks breaking structure, refuse and suggest the structural equivalent. Never produce unbalanced code.

8. **SLOP idioms**:
   - Keep `@`-contracts together at top of function body
   - Indent generously (2 spaces per level)
   - Align `let` bindings vertically
   - One form per line for readability

## Quick Reference

### Syntax

```
; Comment
;; Documentation comment
(module name (export ...) forms...)
(import module-name name*)
(type Name type-expr)
(const NAME Type const-expr)
(fn name ((param Type)...) annotations... body)
(fn name ((param Type)...) annotations... body :c-name "c_name") ; C name override
(fn ((param Type)...) body)           ; Lambda (anonymous function)
```

### Constants

```
(const NAME Type const-expr)

;; const-expr: literals, other constants, arithmetic/bitwise, sizeof
;; NOT allowed: function calls, variables, runtime expressions

(const MAX_CONN Int 128)
(const BUF_SIZE U64 (* 4 1024))
(const FLAGS Int (| FLAG_A FLAG_B))
```

C Mapping: integers → `#define`, others → `static const`.

### Parameter Modes

```
(fn name ((in x Type)        ; Read-only (default) - pass by value
          (out result Type)  ; Write-only - pointer to uninitialized
          (mut state Type))  ; Mutable - value or pointer depending on Type
  ...)
```

### Variables and Mutability

```
;; Immutable bindings (default) - set! is NOT allowed
(let ((x 10)
      (y 20))
  (+ x y))

;; Sequential bindings - later bindings can reference earlier ones
(let* ((x 10)
       (y (+ x 5)))    ; y can use x
  (+ x y))             ; → 25

;; Mutable bindings - set! IS allowed
(let ((mut counter 0))
  (set! counter (+ counter 1))
  counter)

;; Mutable with explicit type
(let ((mut total Int 0))
  (for (i 0 10)
    (set! total (+ total i)))
  total)
```

**Important**: `set!` on a variable requires the `mut` keyword in its binding.

### Types with Ranges

```
(Int)                  ; Any integer (int64_t)
(Int 0 ..)             ; Non-negative
(Int 1 .. 100)         ; Bounded range
(I8) (I16) (I32) (I64) ; Explicit width signed
(U8) (U16) (U32) (U64) ; Explicit width unsigned
(Float)                ; 64-bit float (double)
(F32)                  ; 32-bit float
(Bool)                 ; true or false
(String)               ; Record: { data: (Ptr U8), len: U64 }
(String 1 .. 255)      ; Length-bounded string
(Bytes)                ; Record: { data: (Ptr U8), len: U64, cap: U64 }
(List T 1 ..)          ; Non-empty list
(Array T 10)           ; Fixed-size array (stack allocated)
(Slice T)              ; View into array/list
(Ptr T)                ; Pointer to T
(ScopedPtr T)          ; Scoped pointer (freed when scope ends)
(OptPtr T)             ; Nullable pointer
(Option T)             ; T or none
(Result T E)           ; Success or error
(Map K V)              ; Hash map
(Set T)                ; Hash set
(Chan T)               ; Typed channel (concurrency)
(Thread T)             ; Thread handle returning T
(enum a b c)           ; Enumeration
(record (x T) (y U))   ; Struct
(union (a T) (b U))    ; Tagged union

;; Collection literals - IMMUTABLE values
(list Int 1 2 3)                ; Immutable list with explicit type
(list 1 2 3)                    ; Inferred from first element
(map String Int ("a" 1) ("b" 2)) ; Immutable map with explicit types
(map ("a" 1) ("b" 2))           ; Inferred from first pair

;; For mutable collections, use list-new/map-new with mut binding:
(let ((mut items (list-new arena Int)))
  (list-push items 1)           ; OK: items is mutable
  items)

;; See references/types.md for complete type reference
```

### Generic Functions

```
(@generic (T))                   ; Single type parameter
(@generic (T U))                 ; Multiple type parameters

;; Type variables appear in @spec param/return types
(fn identity ((in x T))
  (@generic (T))
  (@intent "Return the value unchanged")
  (@spec ((T) -> T))
  (@pure)
  x)
```

At call sites, the type checker unifies argument types to bind type variables and substitutes into the return type.

### Required Annotations

```
(fn name ((params...))
  (@intent "Human-readable purpose")     ; REQUIRED
  (@spec ((ParamTypes) -> ReturnType))   ; REQUIRED
  body)
```

### Contracts (For Verification)

Contracts enable automated testing and future SMT verification.
Always specify when possible - they are essential to SLOP's verification story.

```
(@pre condition)           ; Precondition - REQUIRED for non-trivial functions
(@post condition)          ; Postcondition ($result = return value)
(@assume condition)        ; Trusted axiom for verification (e.g., FFI behavior)
(@pure)                    ; No side effects, deterministic
(@trusted)                 ; Skip verification entirely (for unverifiable functions)
(@doc "Extended documentation")  ; Detailed docs, emits as C comment
(@example (args) -> result) ; Executable test case - include multiple!
(@example :eq eq-fn (args) -> result) ; Test with custom equality function
(@alloc arena)             ; Function allocates in arena
(@alloc static)            ; Returns static/global data
(@alloc none)              ; Does not allocate
```

#### Infix Notation for Contracts (Preferred)

Contracts support infix notation using curly braces. **Prefer infix for scaffolds** as it's more readable:

```
;; Infix notation - PREFERRED for scaffolds
(@pre {x > 0})
(@pre {x >= 0 and x <= 100})
(@post {$result == a + b})

;; Prefix notation (also valid)
(@pre (> x 0))
(@post (== $result (+ a b)))
```

**Infix precedence** (high to low): `*, /, %` → `+, -` → comparisons → `and` → `or`

**Grouping and function calls**:
```
(@pre {(a + b) * c > 0})           ; Use () for grouping
(@pre {(len arr) > 0})             ; Use prefix for function calls
(@post {$result >= (min a b)})     ; Mix infix operators with prefix calls
```

Examples are especially important - they serve as:
- Documentation by example
- Test cases for automated testing
- Guidance for hole filling

### Advanced Annotations

```
(@property (forall (x T) expr))   ; Property assertion
(@deprecated "message")           ; Deprecation notice
(@generation-mode mode)           ; deterministic|template|llm
(@derived-from "path")            ; Source tracking
(@generated-by src :version v)    ; Generation metadata
(@requires category :prompt "..." (fn-sigs...))  ; Requirements
(@loop-invariant condition)       ; Loop invariant (verification escape hatch)
(@invariant condition)            ; Type invariant (on type definitions)
(@callback-assume param expr)    ; Property of callback args ($callback-arg for each arg)
```

#### Type Invariants

Use `@invariant` on type definitions to specify relational constraints between fields.
Invariant expressions use field names directly (not prefixed):

```slop
(type Graph (record (triples (List Triple)) (size GraphSize))
  (@invariant (== size (list-len triples))))

(type Counter (record (count (Int 0 ..)) (max-count (Int 1 ..)))
  (@invariant (<= count max-count)))
```

#### Loop Invariants

Use `@loop-invariant` inside loops for verification:

```slop
(let ((mut sum 0))
  (for (i 0 n)
    (@loop-invariant {sum >= 0})
    (set! sum (+ sum i)))
  sum)
```

#### Callback Assumptions

Use `@callback-assume` to declare properties of callback arguments in higher-order functions:

```slop
(fn for-each-item ((g Graph) (callback (Fn (Item) Unit)))
  (@intent "Apply callback to each item in graph")
  (@spec ((Graph (Fn (Item) Unit)) -> Unit))
  (@callback-assume callback (graph-contains g $callback-arg))
  ...)
;; $callback-arg refers to each argument passed to the callback
```

The verifier desugars callback-taking function calls into `for-each` loops and transforms `@callback-assume` into `(forall (t $result) ...)` axioms for Z3 reasoning.

### Requirements (Scaffold Dependencies)

The `@requires` annotation declares dependencies that must be provided before code can be filled:

```
(@requires storage
  :prompt "Which storage approach?"
  :options (
    ("In-memory Map" map)
    ("Database via FFI" db))
  ;; Required function signatures
  (state-get-pet ((state (Ptr State)) (id PetId)) -> (Option (Ptr Pet)))
  (state-insert-pet ((state (Ptr State)) (pet (Ptr NewPet))) -> Pet))
```

### Memory Model

```
;; Arena allocation (primary pattern)
(with-arena 4096
  (let ((data (arena-alloc arena (sizeof Data))))
    ...))  ; Arena freed at end

;; Named arenas (binds custom name instead of 'arena')
(with-arena :as scratch 4096
  (arena-alloc scratch 256))  ; Uses 'scratch' instead of 'arena'

;; Nested named arenas for different lifetimes
(with-arena :as output 8192
  (with-arena :as scratch 4096
    (let ((temp (parse scratch input)))
      (build-result output temp))))
;; scratch freed first, then output

;; For allocating functions, pass arena as first param
(fn create-user ((arena Arena) (name String))
  (@alloc arena)
  ...)

;; Mutable pointer for in-place modification
(let ((mut conn (create-connection arena)))
  (set! conn state 'connected)
  conn)

;; Slices (borrowed views)
(fn process ((data (Slice U8)))
  (for-each (byte data) ...))
```

### Holes (For LLM Generation)

Two modes: **generation** (create new code) and **refactoring** (improve existing code).

```
;; Generation mode - create new implementation
(hole Type "prompt")
(hole Type "prompt"
  :complexity tier-2          ; tier-1 to tier-4
  :context (var1 var2 fn1)    ; Whitelist of available identifiers
  :required (var1 fn1)        ; Identifiers that must appear in output
  :examples ((in) -> out))

;; Refactoring mode - improve existing code
;; Pass existing code as the 3rd positional argument (after Type and prompt)
(hole Type "prompt"
  existing-code-expression    ; The code to refactor/improve
  :complexity tier-3)

;; Example: Refactor for performance
(hole Int "optimize this loop"
  (let ((mut sum 0))
    (for (i 0 n)
      (set! sum (+ sum (* i i))))
    sum)
  :complexity tier-3
  :context (n))
```

**Complexity Tiers:**
- `tier-1`: 1-3B models (trivial: simple arithmetic, field access)
- `tier-2`: 7-8B models (simple conditionals, result construction)
- `tier-3`: 13-34B models (loops, multiple conditions, state)
- `tier-4`: 70B+ models (algorithms, complex state machines)

### Pattern Syntax

```
;; In match expressions:
_                           ; Wildcard
identifier                  ; Binding (captures value)
'symbol                     ; Quoted value (enum variant match)
literal                     ; Literal match
(array p1 p2...)           ; Array pattern
(list p1 p2... | rest)     ; List with rest
(record Type (f1 p1)...)   ; Struct destructure
(union Tag pattern)        ; Union variant
(guard pattern when expr)  ; Guarded pattern
```

**Important**: Use quoted symbols `'Fizz` for enum value matches. Bare identifiers are bindings.

### Common Patterns

```
;; Error handling (Result type)
(match (might-fail x)
  ((ok val) (use val))
  ((error e) (handle e)))

;; Simple enum matching - bare variant names
(match status
  (Active (do-active))
  (Inactive (do-inactive)))

;; Field access (auto -> for pointers)
(. record field)           ; record.field or record->field
(set! record field value)  ; Mutation
(@ array index)            ; Array indexing

;; Loops
(for (i 0 10) body)
(for-each (x collection) body)       ; Iterate List, Set, or Map (keys only)
(for-each ((key value) map) body)    ; Iterate Map key-value pairs
(while cond body)
(break) (continue) (return expr)

;; for-each collection types:
;; - List: elements in order
;; - Set: elements (order undefined)
;; - Map with (var map): iterates keys only
;; - Map with ((k v) map): iterates key-value pairs

;; Data construction
(record-new Type (f1 v1) (f2 v2))  ; Named fields
(TypeName v1 v2 ...)               ; Positional (order matches definition)
(union-new Type Tag value)
(ok val) (error 'variant)  ; QUOTE the error variant!
(some val) (none)

;; Address-of and dereference
(addr expr)                ; &expr
(deref ptr)                ; *ptr

;; Error handling shorthand
(? fallible-expr)          ; Early return on error
```

### FFI (Foreign Function Interface)

```
;; Declare C functions and constants
(ffi "header.h"
  (func-name ((param Type)...) ReturnType)  ; Function
  (func-name ((param Type)...) ReturnType :variadic) ; Variadic function
  (CONST_NAME Type)                          ; Constant (no params = constant)
  ...)

;; Declare C structs for field access
(ffi-struct "header.h" struct_name
  (field1 Type1)
  (field2 Type2))

;; When SLOP name differs from C name
(ffi-struct "sys/stat.h" stat_buf :c-name "stat"
  (st_size I64)
  (st_mode U32))

;; Type casting
(cast Type expr)

;; C inline escape hatch
(c-inline "SOME_C_CONSTANT")

;; FFI-only type: Char
;; Use Char for C char* parameters (distinct from I8/U8)
(ffi "string.h"
  (strcpy ((dest (Ptr Char)) (src (Ptr Char))) (Ptr Char)))
```

Example:
```
(ffi "stdio.h"
  (SEEK_SET Int)                             ; Constant
  (SEEK_CUR Int)
  (EOF Int)
  (printf ((fmt (Ptr Char))) Int :variadic)  ; Variadic C function
  (fclose ((file (Ptr Void))) Int))          ; Function

(ffi "unistd.h"
  (read ((fd Int) (buf (Ptr U8)) (n U64)) I64)
  (write ((fd Int) (buf (Ptr U8)) (n U64)) I64)
  (close ((fd Int)) Int))

(ffi-struct "netinet/in.h" sockaddr_in
  (sin_family U16)
  (sin_port U16)
  (sin_addr U32))
```

## C Mapping

```
SLOP                    C
────                    ─
(Int 0 .. 255)     →    uint8_t + range check
(Ptr User)         →    User*
(. user name)      →    user->name (auto-detects pointer)
(arena-alloc ...)  →    slop_arena_alloc(...)
(@pre {x > 0})     →    SLOP_PRE(x > 0, "x > 0")
(@post {$result > 0}) → SLOP_POST(result > 0, "result > 0")
```

## Standard Library

```
;; Arithmetic
(+ a b) (- a b) (* a b) (/ a b) (% a b)
(min a b) (max a b)      ; Minimum/maximum of two values

;; Memory
(arena-new size) (arena-alloc arena size) (arena-free arena)
(with-arena size body)              ; Scoped arena, implicit 'arena' var
(with-arena :as name size body)     ; Named scoped arena, binds 'name'
(sizeof Type) (addr expr) (deref ptr)

;; I/O (String, Int, Bool, or Float - type inferred)
(print val) (println val)

;; Strings (builtins)
(int-to-string arena n)  ; Int -> String
(string-new arena str) (string-len s) (string-concat arena a b)
(string-eq a b) (string-slice s start end) (string-split arena s delim)
(string-push-char arena s c)  ; Append U8 char to string

;; Strings (strlib - requires import)
;; (import strlib (starts-with ends-with contains trim parse-int float-to-string))
;; float-to-string: (float-to-string arena f) ; Float -> String

;; Lists
(list-new arena Type)           ; Create empty mutable list
(list Type e1 e2...)            ; Immutable literal
(list-push list item)           ; Mutates list (requires mut binding)
(list-pop list)                 ; Remove and return last element -> (Option T)
(list-get list idx) (list-len list)

;; Maps
(map-new arena KeyType ValType) ; Create empty mutable map
(map KeyType ValType (k1 v1)...)  ; Immutable literal
(map-put m k v)                 ; Mutates map (requires mut binding)
(map-get m k) (map-has m k)
(map-keys m)                    ; Return list of all keys
(map-remove m k)                ; Remove key from mutable map

;; Sets
(set-new arena ElementType)     ; Create empty mutable set
(set Type e1 e2...)             ; Immutable set literal
(set-put set elem)              ; Add element to set
(set-has set elem)              ; Check if element exists -> Bool
(set-remove set elem)           ; Remove element from set
(set-elements set)              ; Get all elements as list

;; Result
(ok val) (error 'variant) (is-ok r) (unwrap r)

;; Option
(some val) (none) (is-some o) (is-none o)

;; Time
(now-ms) (sleep-ms ms)

;; Concurrency (thread library)
(chan Type arena)            ; Create unbuffered channel → (Ptr (Chan T))
(chan-buffered Type arena cap) ; Create buffered channel → (Ptr (Chan T))
(chan-close ch)             ; Close channel
(send ch value)             ; Send value (blocks if full) → (Result Unit ChanError)
(recv ch)                   ; Receive value (blocks if empty) → (Result T ChanError)
(try-recv ch)               ; Non-blocking receive → (Result T ChanError)
(spawn arena func)          ; Spawn thread running func → (Ptr (Thread T))
(join thread)               ; Wait for thread, get result
```

## Generation Guidelines

1. Always include @intent and @spec
2. Use @pre/@post to specify contracts for all non-trivial functions
3. **Prefer infix notation for contracts**: `(@pre {x > 0})` not `(@pre (> x 0))`
4. Include @example annotations - at least 2-3 per function
5. Mark pure functions with @pure for optimization and testing
6. Use range types to constrain values
7. Pass Arena as first param for allocating functions
8. Use (Result T E) for fallible operations
9. Mark hole complexity for optimal model routing
10. Quote error variants: `(error 'not-found)` not `(error not-found)`
11. Use `mut` for mutable bindings: `(let ((mut x 0)) (set! x 1))`
12. Use `list-new`/`map-new` with `mut` binding for mutable collections

## Scaffold Generation Guidelines

When generating SLOP scaffolds (files with holes for LLM filling):

### Use Named Types Consistently
```lisp
;; GOOD: Use the named type in signatures
(type PetId (Int 1 ..))
(fn get-pet ((id PetId)) ...)

;; BAD: Inline range (LLM won't connect it to PetId)
(fn get-pet ((id (Int 1 ..))) ...)
```

### Variant Names Must Be Globally Unique

**Compile-time requirement**: Variant names must be globally unique across all enum and union types in a module. Using the same variant name in different types will result in a compile error.

```lisp
;; COMPILE ERROR: 'bad-request' and 'not-found' used in multiple types
(type ApiError (enum bad-request not-found))
(type HttpStatus (enum ok bad-request not-found))

;; CORRECT: Unique variant names across all types
(type ApiError (enum api-bad-request api-not-found))
(type HttpStatus (enum http-ok http-bad-request http-not-found))
```

### Quote Enum Variants in Code
```lisp
;; GOOD
(error 'not-found)

;; BAD - "Undefined variable: not-found"
(error not-found)
```

### Match Constant Types to Usage
```lisp
;; If function expects (Int 0 ..):
(const HANDLER_ID Int 0)        ;; BAD - type mismatch
(const HANDLER_ID (Int 0 ..) 0) ;; GOOD
```

### Use :context for Available Scope, :required for Mandates
```lisp
;; :context = whitelist of what CAN be used
;; :required = what MUST appear in output

(hole Unit "delete"
  :context (state id)          ; Can use these
  :required (state-delete-pet)) ; Must call this
```

### Module Names Must Match Filenames
```lisp
;; File: petstore.slop
(module petstore ...)  ;; GOOD

;; File: petstore-api.slop
(module petstore ...)  ;; BAD - resolver won't find it
```

## Verification Levels

SLOP provides multiple levels of verification:

1. **Parse-time**: S-expression well-formed
2. **Generation-time**: LLM self-check against constraints
3. **Compile-time**: Type checking, range analysis, contract consistency, exhaustiveness
4. **Test-time**: `@example` execution, `@property` testing
5. **Runtime (debug)**: Assertion checks, bounds checking

## Validation

After generating SLOP files, run the type checker:

```bash
slop check path/to/file.slop
```

**Expected behavior with holes:**
- Files with unfilled holes will show `UnfilledHoleError` - expected for scaffolds
- All other errors (type errors, syntax errors, undefined references) should be fixed

## CLI Commands

### Installation

```bash
pip install -e .
```

### Parsing and Inspection

```bash
slop parse file.slop              # Parse and inspect
slop parse file.slop --holes      # Show holes in file
```

### Type Checking

```bash
slop check file.slop              # Type check a file
```

### Contract Verification

```bash
slop verify file.slop             # Verify contracts with Z3 (requires z3-solver)
slop verify file.slop --mode warn # Warn instead of error
slop verify file.slop --timeout 10 # Set Z3 timeout (seconds)
```

The verifier checks:
- `@pre` and `@post` contract consistency (path-sensitive)
- Range type bounds propagation through arithmetic
- Type invariants (`@invariant`)
- Record field axioms and union tag axioms
- Imported function postcondition propagation
- Six automatic loop patterns (filter, map/transform, count, fold, find-first, nested/join)
- `@property` auto-propagation as loop invariants
- `@callback-assume` for higher-order function reasoning
- `list-contains` verifier-only predicate (usable in annotations, no runtime representation)

**Verifier-only predicates:**
- `(list-contains lst elem)` — membership test for `@post`, `@property`, `@loop-invariant`, `@assume` annotations only (no C codegen)

See `$SLOP_HOME/spec/VERIFICATION.md` for the full verification guide.

### Hole Validation

```bash
# Validate a hole implementation against expected type
slop check-hole '(+ x 1)' -t Int -p '((x Int))'

# With context from a file
slop check-hole '(helper 42)' -t Int -c myfile.slop

# From stdin
echo '(ok value)' | slop check-hole -t '(Result T E)'
```

### Transpilation and Building

```bash
slop transpile file.slop -o output.c   # Transpile to C
slop build file.slop -o binary         # Full build (requires cc)
slop build file.slop --library static  # Build as static library
slop build file.slop --library shared  # Build as shared library
slop build file.slop --skip-check      # Skip type checking (bootstrapping)
```

**Build Modes:**
- **Debug**: `-g -DSLOP_DEBUG` - runtime contract checks enabled
- **Release**: `-O3 -DNDEBUG` - contract checks removed for speed
- **Safe**: `-O2 -DSLOP_SAFE` - contract checks with optimization

Most commands accept `--python` to use the Python toolchain instead of native binaries.

### Other Commands

```bash
slop derive schema.json -o types.slop  # Generate types from schema
slop fill file.slop -o filled.slop     # Fill holes with LLM
slop test file.slop                    # Run @example and @property tests
slop test file.slop --rebuild          # Rebuild before testing
slop format file.slop                  # Format SLOP source code
slop format file.slop --check          # Check formatting (exit 1 if not)
slop paths                             # Show resolved SLOP paths
slop paths -v                          # Verbose (show examples list)
```

### Documentation Generation

```bash
slop doc file.slop                     # Generate markdown docs to stdout
slop doc file.slop -o docs.md          # Write to file
slop doc file.slop -f json             # Output as JSON
```

Extracts `@intent`, `@spec`, `@pre`, `@post`, `@example` annotations into readable documentation.

### Language Reference (for AI)

```bash
slop ref                  # Full language reference
slop ref types            # Type system reference
slop ref --list           # List available topics
```

Use `slop ref` for quick lookups of syntax, built-ins, and patterns.

## Common Mistakes

These functions/patterns do NOT exist in SLOP - use the alternatives:

| Don't Use | Use Instead |
|-----------|-------------|
| `print-int n` | `(println n)` - print/println accept Int directly |
| `print-float n` | `(println n)` - print/println accept Float directly |
| `(println enum-value)` | Use `match` to print different strings |
| `arena` outside with-arena | Wrap code in `(with-arena size ...)` |
| `(block ...)` | `(do ...)` for sequencing |
| `(begin ...)` | `(do ...)` for sequencing |
| `(progn ...)` | `(do ...)` for sequencing |
| `strlen s` | `(string-len s)` |
| `malloc` | `(arena-alloc arena size)` |
| `list.length` | `(list-len list)` |
| `arr.length` | Arrays are fixed size - use declared size |
| `list-append` | `(list-push list elem)` |
| `list-add` | `(list-push list elem)` |
| `map-set` | `(map-put map key val)` |
| `hash-get` | `(map-get map key)` |
| `parse-int` | `(import strlib (parse-int))` |
| `json-parse` | Implement manually or FFI |
| `string-find` | Iterate with for-each |
| `read-line` | FFI to stdio.h |
| `sqrt`, `sin`, `cos` | FFI to math.h |
| Definitions outside module | All `(type)`, `(fn)`, `(const)` inside `(module ...)` |

### Module Structure

All definitions must be INSIDE the module form:

```lisp
;; CORRECT:
(module my-module
  (export public-fn)

  (type MyType (Int 0 ..))

  (fn public-fn (...)
    ...))  ; <-- closing paren wraps entire module

;; WRONG:
(module my-module
  (export public-fn))

(fn public-fn ...)  ; ERROR: outside module form
```

### Error Returns

Quote error variants:

```lisp
(error 'not-found)     ; CORRECT: quoted
(error not-found)      ; WRONG: undefined variable
```

## See Also

- references/types.md - Full type system
- references/patterns.md - Common patterns
- references/common-mistakes.md - What NOT to do
