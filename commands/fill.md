Fill the holes in: $ARGUMENTS

## Instructions

Read the specified SLOP file and fill each `(hole ...)` expression with a valid implementation.

### Constraints for Each Hole

You MUST respect these constraints strictly:

1. **Type**: The implementation must return the type specified in the hole
2. **:context**: You may ONLY use identifiers listed in the `:context` attribute, plus any SLOP built-in functions from the language spec (arithmetic, list operations, map operations, string operations, control flow, etc.)
3. **:required**: Every identifier in `:required` MUST appear in your implementation
4. **Function contracts**: The implementation must satisfy:
   - `@pre` - Assume preconditions hold
   - `@post` - Ensure postconditions are satisfied (use `$result` semantics)
   - `@example` - Implementation must produce correct outputs for all examples

### What You Can Use

- Identifiers listed in `:context`
- SLOP built-ins: `+`, `-`, `*`, `/`, `<`, `>`, `<=`, `>=`, `==`, `!=`, `and`, `or`, `not`
- Control flow: `if`, `cond`, `match`, `let`, `do`, `when`, `unless`
- Loops: `for`, `for-each`, `while`, `break`, `continue`, `return`
- List ops: `list-new`, `list-push`, `list-get`, `list-len`, `list`
- Map ops: `map-new`, `map-put`, `map-get`, `map-has`, `map`
- String ops: `string-new`, `string-len`, `string-concat`, `string-eq`, `string-slice`
- Result ops: `ok`, `error`, `is-ok`, `unwrap`, `?`
- Option ops: `some`, `none`, `is-some`, `is-none`
- Memory: `arena-alloc`, `sizeof`, `addr`
- Field access: `.`, `set!`, `@`

### What You Cannot Do

- Reference variables, functions, or types not in `:context`
- Call user-defined functions not listed in `:context`
- Access global state not passed via `:context`
- Ignore `:required` identifiers

### Process

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
```lisp
(hole Int "add one to x" :context (x))
```

With implementation `(+ x 1)`, validate with:
```bash
slop check-hole '(+ x 1)' -t Int -c original.slop
```

### Output

Replace each `(hole ...)` with its implementation. Keep all annotations, types, and structure intact. Only write the file after all holes pass validation.
