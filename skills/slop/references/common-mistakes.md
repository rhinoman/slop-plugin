# SLOP Common Mistakes

These functions/patterns do NOT exist in SLOP - use the alternatives:

| Don't Use | Use Instead |
|-----------|-------------|
| `print-int n` | `(println (int-to-string arena n))` |
| `print-float n` | `(println (float-to-string arena n))` |
| `(println enum-value)` | Use `match` to print different strings |
| `arena` outside with-arena | Wrap code in `(with-arena size ...)` |
| `(block ...)` | `(do ...)` for sequencing |
| `(begin ...)` | `(do ...)` for sequencing |
| `(progn ...)` | `(do ...)` for sequencing |
| `read-line` | FFI to stdio.h |
| `sqrt`, `sin`, `cos` | FFI to math.h |
| `strlen s` | `(string-len s)` |
| `malloc` | `(arena-alloc arena size)` |
| `arr.length` | Arrays are fixed size - use declared size |
| `list.length` | `(list-len list)` |
| `list-append` | `(list-push list elem)` |
| `list-add` | `(list-push list elem)` |
| `map-set` | `(map-put map key val)` |
| `hash-get` | `(map-get map key)` |
| `parse-int` | Implement manually or FFI |
| `json-parse` | Implement manually or FFI |
| `string-find` | Iterate with for-each |

## Quoting Errors

### Error Variants Must Be Quoted

```lisp
;; CORRECT
(error 'not-found)
(error 'invalid-input)

;; WRONG - undefined variable error
(error not-found)
(error invalid-input)
```

### Enum Values in Match

For simple enums (no data), use bare variant names in match:

```lisp
;; CORRECT for simple enums
(match status
  (Active (do-active))
  (Inactive (do-inactive)))

;; For tagged unions with data, use bindings
(match result
  ((ok val) (use val))
  ((error e) (handle e)))
```

## Arena Scoping

### Arena Must Be In Scope

```lisp
;; WRONG - arena not in scope
(let ((s (string-new arena "hello")))
  ...)

;; CORRECT - wrap in with-arena
(with-arena 4096
  (let ((s (string-new arena "hello")))
    ...))

;; OR pass arena as parameter
(fn my-func ((arena Arena) ...)
  (let ((s (string-new arena "hello")))
    ...))
```

## Type Mismatches

### Range Types Must Match

```lisp
;; If function expects (Int 0 ..):
(const HANDLER_ID Int 0)        ;; WRONG - Int is not (Int 0 ..)
(const HANDLER_ID (Int 0 ..) 0) ;; CORRECT

;; Named types must match exactly
(type PetId (Int 1 ..))
(fn get-pet ((id PetId)) ...)   ;; Expects PetId, not (Int 1 ..)
```

### Empty Collections Need Types

```lisp
;; WRONG - can't infer type of empty list
(let ((items (list)))
  ...)

;; CORRECT - provide explicit type
(let ((items (list-new arena Int)))
  ...)

;; Or use literal with type
(let ((items (list Int)))
  ...)
```

## Field Access

### Use Dot Notation, Not Arrow

```lisp
;; CORRECT - transpiler handles pointer detection
(. user name)
(. ptr field)

;; The transpiler generates:
;; user.name    (if user is a value)
;; ptr->field   (if ptr is a pointer)
```

## Control Flow

### Use `do` for Sequencing

```lisp
;; WRONG
(when condition
  (action1)
  (action2))  ; action2 is outside the when!

;; CORRECT
(when condition
  (do
    (action1)
    (action2)))
```

### Break/Continue Need `do` Wrapper

```lisp
;; WRONG
(for (i 0 10)
  (when (== i 5)
    (set! found i)
    (break)))  ; break is outside the when!

;; CORRECT
(for (i 0 10)
  (when (== i 5)
    (do
      (set! found i)
      (break))))
```

## Memory

### Cast Arena Allocations

```lisp
;; WRONG - arena-alloc returns (Ptr U8)
(let ((user (arena-alloc arena (sizeof User))))
  (set! user name "foo"))  ; Type error

;; CORRECT - cast to proper pointer type
(let ((user (cast (Ptr User) (arena-alloc arena (sizeof User)))))
  (set! user name "foo"))
```

## I/O

### Only Strings Can Be Printed

```lisp
;; WRONG
(println 42)
(println my-float)

;; CORRECT
(println (int-to-string arena 42))

;; For enums, use match
(match status
  (Active (println "active"))
  (Inactive (println "inactive")))
```
