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

;; Example
(let ((scores (map ("alice" 100) ("bob" 85))))
  (map-get scores "alice"))  ; → (some 100)
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
(OwnPtr T)              ; T*, owning (freed on scope exit)
(OptPtr T)              ; T*, nullable

(type UserRef (Ptr User))
(type OwnedUser (OwnPtr User))
```

## Function Types

```
(Fn (A B) -> R)         ; function pointer

(type Predicate (Fn (Int) -> Bool))
(type Callback (Fn (Event) -> Unit))
```

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

## Collection Literal Type Inference

When explicit type is provided, it is used directly:
```
(list Int 1 2 3)              ; → (List Int)
(map String Int ("a" 1))      ; → (Map String Int)
```

When type is omitted, inference follows these rules:
1. **Binding with annotation**: `(let ((x (List Int) (list 1 2))) ...)` → infer from binding
2. **Passed to typed param**: function expects `(List Int)`, passed `(list 1 2)` → infer from param
3. **Return with @spec**: function returns `(List Int)`, `(list 1 2)` → infer from spec
4. **Non-empty, no context**: `(list 1 2 3)` → infer from first element
5. **Empty, no context**: `(list)` → ERROR: explicit type required
