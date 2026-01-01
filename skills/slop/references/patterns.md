# SLOP Patterns Reference

## Contracts for Verification

Contracts are central to SLOP. They enable:
- Automated property-based testing
- Static verification (future SMT integration)
- Documentation that's always correct

### Complete Contract Example

```
(fn divide ((in a Int) (in b Int))
  (@intent "Safely divide two integers")
  (@spec ((Int Int) -> (Result Int DivError)))
  (@pre {b != 0})
  (@post {(is-error $result) or (b * (unwrap $result)) == a})
  (@pure)
  ;; Multiple examples - cover normal cases and edge cases
  (@example (10 2) -> (ok 5))
  (@example (0 5) -> (ok 0))
  (@example (7 3) -> (ok 2))      ; Integer division
  (@example (7 0) -> (error 'div-by-zero))
  ...)
```

### Pure Functions

Mark functions with `@pure` when they have no side effects:

```
(fn length ((in s String))
  (@intent "Get string length")
  (@spec ((String) -> (Int 0 ..)))
  (@pure)
  (@post {$result >= 0})
  (@example ("hello") -> 5)
  (@example ("") -> 0)
  (@example ("a") -> 1)
  ...)
```

### Postconditions

Use `$result` to refer to the return value:

```
(fn clamp ((in x Int) (in lo Int) (in hi Int))
  (@intent "Clamp value to range")
  (@spec ((Int Int Int) -> Int))
  (@pre {lo <= hi})
  (@post {$result >= lo and $result <= hi})
  (@pure)
  (@example (5 0 10) -> 5)
  (@example (-5 0 10) -> 0)
  (@example (15 0 10) -> 10)
  (cond
    ((< x lo) lo)
    ((> x hi) hi)
    (else x)))
```

## Parameter Modes

### Input Parameters (Default)

```
(fn process ((in x Int))     ; Read-only, pass by value
  (@intent "Process value")
  (@spec ((Int) -> Int))
  (@pure)
  (* x 2))
```

### Output Parameters

```
(fn parse-int ((in s String) (out result Int))
  (@intent "Parse string to int, store in result")
  (@spec ((String Int) -> Bool))
  (@pre {(string-len s) > 0})
  (let ((val (string-to-int s)))
    (if (is-some val)
      (do (set! result (unwrap val)) true)
      false)))
```

### Mutable Parameters

```
(fn increment ((mut counter Int))
  (@intent "Increment counter in place")
  (@spec ((Int) -> Unit))
  (@pre {counter < 2147483647})
  (set! counter (+ counter 1)))
```

## Arena Allocation

Standard pattern for request-scoped memory:

```
(fn handle-request ((req (Ptr Request)))
  (@intent "Process incoming request")
  (@spec (((Ptr Request)) -> (Ptr Response)))

  (with-arena 4096
    (let ((user (parse-user arena req))
          (result (process arena user)))
      (send-response result))))
; Arena freed automatically at end
```

For functions that allocate:

```
(fn create-user ((arena Arena) (name String) (email String))
  (@intent "Create a new user")
  (@spec ((Arena String String) -> (Ptr User)))
  (@pre {arena != nil})
  (@pre {(string-len name) > 0})
  (@alloc arena)

  (let ((user (cast (Ptr User) (arena-alloc arena (sizeof User)))))
    (set! user name name)
    (set! user email email)
    user))
```

## Record Construction

Two ways to construct records:

### Named Fields (Explicit)

```
(type User (record
  (id UserId)
  (name String)
  (email String)))

;; Named field construction
(record-new User (id 1) (name "Alice") (email "alice@example.com"))
```

### Positional (Concise)

```
;; Positional construction - order matches definition
(User 1 "Alice" "alice@example.com")
```

Positional is shorter but named is clearer for records with many fields.

## Error Handling

### Result Pattern

```
(fn read-file ((arena Arena) (path String))
  (@intent "Read file contents")
  (@spec ((Arena String) -> (Result (Ptr Bytes) IoError)))
  (@alloc arena)

  (let ((fd (open path)))
    (if (< fd 0)
      (error 'file-not-found)
      (let ((data (read-all arena fd)))
        (close fd)
        (ok data)))))
```

### Matching Results

```
(fn process ((arena Arena) (path String))
  (match (read-file arena path)
    ((ok data)
      (parse arena data))
    ((error 'file-not-found)
      (error 'missing-input))
    ((error e)
      (error e))))
```

### Early Return with ?

```
(fn process-all ((arena Arena) (paths (List String)))
  (@spec ((Arena (List String)) -> (Result (List Data) Error)))

  (let ((mut results (list-new arena Data)))
    (for-each (path paths)
      ;; ? returns early on error
      (let ((data (? (read-file arena path))))
        (list-push results data)))
    (ok results)))
```

## Loop Patterns

### Find Index Matching Predicate

```
(let ((mut result -1))
  (for (i 0 SIZE)
    (when PREDICATE
      (do
        (set! result i)
        (break))))
  result)
```

### Count Matching Elements

```
(let ((mut count 0))
  (for (i 0 SIZE)
    (when PREDICATE
      (set! count (+ count 1))))
  count)
```

### Sum Values

```
(let ((mut total 0))
  (for (i 0 SIZE)
    (set! total (+ total ACCESSOR)))
  total)
```

### Find Empty Slot

```
(let ((mut idx -1))
  (for (i 0 SIZE)
    (when (== (. (@ storage i) id) 0)
      (do
        (set! idx i)
        (break))))
  idx)
```

### Array Shift Delete

```
(for (i idx (- SIZE 1))
  (set! (@ arr i) (@ arr (+ i 1))))
```

## Match Patterns

### Simple Enums (No Bindings)

Simple enums have no data - use bare variant names:

```
(match status
  (Pending (println "waiting"))
  (Active (println "running"))
  (Done (println "finished")))
```

### Tagged Unions (With Bindings)

Result and Option carry data - bind with parens:

```
(match result
  ((ok val) (use val))
  ((error e) (handle e)))

(match option
  ((some x) (use x))
  ((none) (handle-none)))
```

### Error Returns

IMPORTANT: Quote the error variant!

```
(if (< fd 0)
  (error 'file-not-found)    ; CORRECT: quoted
  (ok data))

; WRONG: (error file-not-found) - unquoted is undefined variable
```

## Iterating Collections

### For-Each

```
(fn sum-ages ((users (Ptr (List (Ptr User)))))
  (@intent "Sum all user ages")
  (@spec (((Ptr (List (Ptr User)))) -> Int))
  (@pure)

  (let ((mut total 0))
    (for-each (user users)
      (when (. user age)
        (set! total (+ total (. (. user age) value)))))
    total))
```

### For Range

```
(fn fill-zeros ((arr (Ptr (Array Int 100))))
  (for (i 0 100)
    (set! (@ arr i) 0)))
```

### While

```
(fn find-first ((list (Ptr (List Int))) (pred (Fn (Int) -> Bool)))
  (@spec (((Ptr (List Int)) (Fn (Int) -> Bool)) -> (Option Int)))

  (let ((mut i 0)
        (len (. list len)))
    (while (< i len)
      (let ((val (@ (. list data) i)))
        (if (pred val)
          (return (some val))
          (set! i (+ i 1)))))
    (none)))
```

## Validation Pattern

```
(fn validate-user ((input (Ptr CreateUserInput)))
  (@intent "Validate user input")
  (@spec (((Ptr CreateUserInput)) -> (Result Unit ValidationError)))
  (@pre {input != nil})
  (@pure)

  (cond
    ((< (string-len (. input email)) 5)
      (error 'email-too-short))
    ((< (string-len (. input password)) 8)
      (error 'password-too-short))
    ((not (contains (. input email) "@"))
      (error 'invalid-email))
    (else
      (ok unit))))
```

## Builder Pattern

```
(type RequestBuilder (record
  (arena Arena)
  (method Method)
  (url String)
  (headers (Ptr (List Header)))
  (body (Option (Ptr Bytes)))))

(fn request-new ((arena Arena))
  (@spec ((Arena) -> (Ptr RequestBuilder)))
  (let ((b (cast (Ptr RequestBuilder) (arena-alloc arena (sizeof RequestBuilder)))))
    (set! b arena arena)
    (set! b method 'GET)
    (set! b headers (list-new arena Header))
    b))

(fn request-method ((b (Ptr RequestBuilder)) (m Method))
  (@spec (((Ptr RequestBuilder) Method) -> (Ptr RequestBuilder)))
  (set! b method m)
  b)

(fn request-url ((b (Ptr RequestBuilder)) (u String))
  (@spec (((Ptr RequestBuilder) String) -> (Ptr RequestBuilder)))
  (set! b url u)
  b)
```

## State Machine

```
(type ConnState (enum
  disconnected
  connecting
  connected
  closing))

(type Connection (record
  (state ConnState)
  (socket (Option Socket))
  (buffer (Ptr Bytes))))

(fn conn-transition ((conn (Ptr Connection)) (event Event))
  (@intent "Handle state transition")
  (@spec (((Ptr Connection) Event) -> (Result Unit ConnError)))
  (@pre {conn != nil})

  (match (tuple (. conn state) event)
    ;; disconnected + connect → connecting
    ((Disconnected Connect)
      (set! conn state 'Connecting)
      (ok unit))

    ;; connecting + connected → connected
    ((Connecting (Connected sock))
      (set! conn state 'Connected)
      (set! conn socket (some sock))
      (ok unit))

    ;; connected + close → closing
    ((Connected Close)
      (set! conn state 'Closing)
      (ok unit))

    ;; Invalid transition
    (else
      (error 'invalid-transition))))
```

## Resource Cleanup

```
(fn with-file ((path String) (mode FileMode) (body (Fn (File) -> T)))
  (@intent "Execute body with open file, ensure cleanup")
  (@spec ((String FileMode (Fn (File) -> T)) -> (Result T IoError)))

  (let ((file (? (file-open path mode))))
    (let ((result (body file)))
      (file-close file)
      (ok result))))

;; Usage
(with-file "data.txt" 'read (fn (f)
  (file-read-all arena f)))
```

## FFI Pattern

```
;; Declare external C function
(ffi "openssl/sha.h"
  (SHA256 ((data (Ptr U8)) (len U64) (out (Ptr U8))) (Ptr U8)))

;; Wrap with SLOP types
(fn hash-sha256 ((arena Arena) (data Bytes))
  (@intent "Compute SHA256 hash")
  (@spec ((Arena Bytes) -> Bytes))
  (@alloc arena)

  (let ((out (cast (Ptr U8) (arena-alloc arena 32))))
    (SHA256 (. data ptr) (. data len) out)
    (bytes-from-ptr out 32)))
```
