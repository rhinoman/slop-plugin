---
name: slop
description: |
  Generate code in SLOP (Symbolic LLM-Optimized Programming), a language designed for
  minimal human involvement in coding. Use when: (1) User asks for SLOP code, (2) Need
  strong contracts and range types, (3) Creating code with typed holes for incremental
  generation, (4) Generating efficient C code. SLOP uses S-expression syntax and transpiles to C.
---

# SLOP Language Skill

SLOP is designed for hybrid generation where humans specify intent and machines generate code.

## Philosophy

```
Humans: Specify WHAT (intent, contracts, types, examples)
Machines: Handle HOW (implementation, verification, compilation)
Type system: Catch errors early (range bounds, exhaustive matching, contract violations)
```

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
          (mut state Type))  ; Read-write - pointer to initialized
  ...)
```

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
(Option T)             ; T or none
(Result T E)           ; Success or error
(Map K V)              ; Hash map
(Set T)                ; Hash set
(enum a b c)           ; Enumeration
(record (x T) (y U))   ; Struct
(union (a T) (b U))    ; Tagged union

;; Collection literals (explicit type preferred for clarity)
(list Int 1 2 3)                ; List with explicit element type
(list 1 2 3)                    ; Inferred from first element
(map String Int ("a" 1) ("b" 2)) ; Map with explicit key/value types
(map ("a" 1) ("b" 2))           ; Inferred from first pair

;; See references/types.md for complete type reference
```

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
(@pure)                    ; No side effects, deterministic
(@example (args) -> result) ; Executable test case - include multiple!
(@alloc arena)             ; Memory allocation strategy
```

Examples are especially important - they serve as:
- Documentation by example
- Test cases for automated testing
- Guidance for hole filling

### Advanced Annotations

```
(@property (forall (x T) expr))   ; Property assertion
(@generation-mode mode)           ; deterministic|template|llm
(@derived-from "path")            ; Source tracking
(@generated-by src :version v)    ; Generation metadata
(@requires category :prompt "..." (fn-sigs...))  ; Requirements
```

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

;; For allocating functions, pass arena as first param
(fn create-user ((arena Arena) (name String))
  (@alloc arena)
  ...)

;; Owning pointers
(let ((owned (OwnPtr Data) (create-data arena)))
  (transfer owned other-fn))  ; Ownership transferred

;; Slices (borrowed views)
(fn process ((data (Slice U8)))
  (for-each (byte data) ...))
```

### Holes (For LLM Generation)

```
(hole Type "prompt")
(hole Type "prompt"
  :complexity tier-2          ; tier-1 to tier-4
  :context (var1 var2 fn1)    ; Whitelist of available identifiers
  :required (var1 fn1)        ; Identifiers that must appear in output
  :examples ((in) -> out))
```

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
(for-each (x list) body)
(while cond body)
(break) (continue) (return expr)

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
```

Example:
```
(ffi "stdio.h"
  (SEEK_SET Int)                             ; Constant
  (SEEK_CUR Int)
  (EOF Int)
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
(@pre cond)        →    SLOP_PRE(cond, "...")
(@post cond)       →    SLOP_POST(cond, "...")
```

## Standard Library

```
;; Memory
(arena-new size) (arena-alloc arena size) (arena-free arena)
(with-arena size body)   ; Scoped arena, implicit 'arena' var
(sizeof Type) (addr expr) (deref ptr)

;; I/O (strings only)
(print str) (println str)

;; Strings
(int-to-string arena n)  ; Int -> String
(string-new arena str) (string-len s) (string-concat arena a b)
(string-eq a b) (string-slice s start end) (string-split arena s delim)

;; Lists
(list-new arena Type)           ; Create empty typed list
(list Type e1 e2...)            ; Literal with explicit type
(list-push list item) (list-get list idx) (list-len list)

;; Maps
(map-new arena KeyType ValType)
(map KeyType ValType (k1 v1)...)
(map-put m k v) (map-get m k) (map-has m k)

;; Result
(ok val) (error 'variant) (is-ok r) (unwrap r)

;; Option
(some val) (none) (is-some o) (is-none o)

;; Time
(now-ms) (sleep-ms ms)
```

## Generation Guidelines

1. Always include @intent and @spec
2. Use @pre/@post to specify contracts for all non-trivial functions
3. Include @example annotations - at least 2-3 per function
4. Mark pure functions with @pure for optimization and testing
5. Use range types to constrain values
6. Pass Arena as first param for allocating functions
7. Use (Result T E) for fallible operations
8. Mark hole complexity for optimal model routing
9. Quote error variants: `(error 'not-found)` not `(error not-found)`

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

### Avoid Overlapping Enum Variants
```lisp
;; BAD: Same variant in multiple enums causes type ambiguity
(type ApiError (enum bad-request not-found))
(type HttpStatus (enum ok bad-request not-found))

;; GOOD: Unique names
(type ApiError (enum api-bad-request api-not-found))
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

## Validation

After generating SLOP files, run the type checker:

```bash
uv run slop check path/to/file.slop
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
```

### Other Commands

```bash
slop derive schema.json -o types.slop  # Generate types from schema
slop fill file.slop -o filled.slop     # Fill holes with LLM
slop test file.slop                    # Run @example and @property tests
```

## See Also

- references/types.md - Full type system
- references/patterns.md - Common patterns
- references/common-mistakes.md - What NOT to do
