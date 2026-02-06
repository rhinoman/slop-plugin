# SLOP Type System Reference

## Integer Types

### Unbounded

```
(Int)       ; int64_t, any value
(I8)        ; int8_t
(I16)       ; int16_t
(I32)       ; int32_t
(I64)       ; int64_t
(U8)        ; uint8_t
(U16)       ; uint16_t
(U32)       ; uint32_t
(U64)       ; uint64_t
```

### Range-Bounded

```
(Int min ..)           ; >= min
(Int .. max)           ; <= max
(Int min .. max)       ; min <= x <= max

; Examples
(type UserId (Int 1 ..))
(type Age (Int 0 .. 150))
(type Percentage (Int 0 .. 100))
(type Port (Int 1 .. 65535))
```

Range types compile to appropriate C types with runtime checks:
- `(Int 0 .. 255)` → `uint8_t`
- `(Int 0 .. 65535)` → `uint16_t`
- `(Int -128 .. 127)` → `int8_t`
- `(Int 0 ..)` → `int64_t` (unbounded)

## Float Types

```
(Float)                 ; double (64-bit)
(F32)                   ; float (32-bit)
(Float min .. max)      ; bounded double

(type Probability (Float 0.0 .. 1.0))
(type Latitude (Float -90.0 .. 90.0))
```

## Boolean Type

```
(Bool)                  ; uint8_t, true or false
```

## String Types

```
(String)                ; Record: { data: (Ptr U8), len: U64 }
(String min ..)         ; at least min chars
(String .. max)         ; at most max chars
(String min .. max)     ; length in range

(type NonEmpty (String 1 ..))
(type Username (String 3 .. 20))
(type UUID (String 36 .. 36))
```

Access string fields directly:
```
(. my-string data)      ; (Ptr U8) - raw bytes
(. my-string len)       ; U64 - length
```

## Bytes Type

```
(Bytes)                 ; Record: { data: (Ptr U8), len: U64, cap: U64 }
(Bytes .. max-len)      ; Bounded byte buffer
```

Access bytes fields directly:
```
(. my-bytes data)       ; (Ptr U8) - raw bytes
(. my-bytes len)        ; U64 - current length
(. my-bytes cap)        ; U64 - capacity
```

## Collection Types

### Lists (Dynamic)

```
(List T)                ; dynamic array
(List T n)              ; exactly n elements
(List T min ..)         ; at least min
(List T min .. max)     ; length in range

(type Tags (List String))
(type Coords (List Float 2))
```

### Arrays (Fixed, Stack)

```
(Array T n)             ; T[n], stack allocated

(type Matrix3x3 (Array Float 9))
(type Buffer256 (Array U8 256))
```

### Slices (Views)

```
(Slice T)               ; View into array/list (pointer + length)
```

### Maps and Sets

```
(Map K V)               ; hash map
(Set T)                 ; hash set

(type UserCache (Map UserId User))
(type Permissions (Set String))

;; Map literal
(map ("key1" value1) ("key2" value2))

;; Set literal
(set Type elem1 elem2 ...)

;; Examples
(let ((scores (map ("alice" 100) ("bob" 85))))
  (map-get scores "alice"))  ; → (some 100)

(let ((tags (set String "rust" "go" "slop")))
  (set-has tags "slop"))     ; → true
```

## Concurrency Types

```
(Chan T)                ; Typed channel for communication
(Thread T)              ; Thread handle returning T when joined

(type MessageChan (Ptr (Chan String)))
(type WorkerThread (Ptr (Thread Int)))
```

Channel and thread creation functions return pointers (explicit type argument required):
```
(chan Type arena)            ; → (Ptr (Chan T))
(chan-buffered Type arena n) ; → (Ptr (Chan T))
(spawn arena func)           ; → (Ptr (Thread T))
```

Channels enable safe communication between threads (closures capture outer variables):
```
(with-arena 4096
  (let ((ch (chan Int arena)))
    (spawn arena (fn ()
      (send ch 42)
      0))
    (match (recv ch)
      ((ok msg) (println msg))
      ((error e) (println "channel error")))))
```

## Algebraic Types

### Enums

```
(type Status (enum pending active completed))
; → enum { Status_pending, Status_active, Status_completed }
```

### Records (Structs)

```
(type User (record
  (id UserId)
  (name String)
  (email String)
  (age (Option Age))))

; → struct User { UserId id; slop_string name; ... }
```

### Tagged Unions

```
(type Result (union
  (ok T)
  (error E)))

(type Shape (union
  (circle Float)
  (rect Float Float)
  (point)))

; → struct with tag + union
```

## Pointer Types

```
(Ptr T)                 ; T*, borrowed reference
(ScopedPtr T)           ; T*, scoped (freed when scope ends)
(OptPtr T)              ; T*, nullable

(type UserRef (Ptr User))
(type ScopedUser (ScopedPtr User))
```

## Function Types

```
(Fn (A B) -> R)         ; function pointer

(type Predicate (Fn (Int) -> Bool))
(type Callback (Fn (Event) -> Unit))
```

### Lambda Functions

```
(fn ((param Type)...) body)      ; Anonymous function

;; Example: passing a lambda to spawn
(spawn arena (fn () (do-work) 0))

;; Example: as a callback
(map-values items (fn ((x Int)) (* x 2)))
```

## Generic Type Parameters

Use `@generic` to declare polymorphic functions:

```
(@generic (T))                   ; Single type parameter
(@generic (T U))                 ; Multiple type parameters

(fn first ((in items (List T)))
  (@generic (T))
  (@spec (((List T)) -> (Option T)))
  (@pure)
  (if (== (list-len items) 0)
    (none)
    (list-get items 0)))
```

Type variables appear in `@spec` param/return types. At call sites, the checker unifies argument types to bind type variables.

## Utility Types

### Option

```
(Option T)              ; T or none (sugar for union)

(let ((age (Option Age)))
  (match age
    ((some a) (use a))
    ((none) (default))))
```

### Result

```
(Result T E)            ; success or error (sugar for union)

(fn divide ((a Int) (b Int))
  (@spec ((Int Int) -> (Result Int DivError)))
  (if (== b 0)
    (error 'div-by-zero)
    (ok (/ a b))))
```

## Type Aliases

```
(alias UserId (Int 1 ..))
(alias Email (String 5 .. 255))
(alias Handler (Fn (Request) -> Response))
```

## FFI-Only Types

```
Char                    ; C char type for FFI interop
```

`Char` is distinct from `I8` and `U8`. Use it when interfacing with C functions
that expect `char*` parameters:

```
(ffi "string.h"
  (strlen ((s (Ptr Char))) U64)
  (strcpy ((dest (Ptr Char)) (src (Ptr Char))) (Ptr Char)))
```

## Collection Mutability

**Collection literals are immutable.** To mutate a collection, use `list-new`/`map-new`/`set-new` with a `mut` binding:

```
;; Immutable - cannot use list-push
(let ((items (list Int 1 2 3)))
  (list-get items 0))           ; OK: read-only access

;; Mutable - can use list-push
(let ((mut items (list-new arena Int)))
  (list-push items 1)           ; OK: items is mutable
  (list-push items 2)
  items)

;; Mutable set
(let ((mut seen (set-new arena Int)))
  (set-put seen 42)
  (set-has seen 42))            ; → true
```

## Collection Literal Type Inference

When explicit type is provided, it is used directly:
```
(list Int 1 2 3)              ; → (List Int), immutable
(map String Int ("a" 1))      ; → (Map String Int), immutable
```

When type is omitted, inference follows these rules:
1. **Binding with annotation**: `(let ((x (List Int) (list 1 2))) ...)` → infer from binding
2. **Passed to typed param**: function expects `(List Int)`, passed `(list 1 2)` → infer from param
3. **Return with @spec**: function returns `(List Int)`, `(list 1 2)` → infer from spec
4. **Non-empty, no context**: `(list 1 2 3)` → infer from first element
5. **Empty, no context**: `(list)` → ERROR: explicit type required
