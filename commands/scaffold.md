Generate a SLOP scaffold for: $ARGUMENTS

## Instructions

Create SLOP code where implementations are stubbed out using `(hole ...)` expressions. Follow these rules:

### Structure
1. Include the module declaration with appropriate exports
2. Define all necessary types (records, enums, unions) with full specifications
3. For each function, provide complete annotations but stub the body with a hole

### Required Annotations (do not stub these)
- `@intent` - Human-readable purpose
- `@spec` - Full type signature
- `@pre` - Preconditions (when applicable)
- `@post` - Postconditions using `$result` (when applicable)
- `@pure` - Mark pure functions
- `@example` - Include 2-3 examples per function

### Hole Syntax
```lisp
(hole ReturnType "description of what to implement"
  :complexity tier-N        ; tier-1 (trivial) to tier-4 (complex)
  :context (var1 var2 fn1)  ; whitelist of identifiers the implementation may use
  :required (fn1))          ; identifiers that MUST appear in the fill
```

### Guidelines
- Use named types consistently (define `(type Foo ...)` then use `Foo` in signatures)
- Avoid overlapping enum variant names across different types
- Quote enum variants in code: `'active` not `active`
- Module name must match the intended filename
- Set `:context` to the parameters and any helper functions available
- Set `:required` only for functions that MUST be called (e.g., state mutations)

### Validation

After writing the scaffold file, run:
```bash
slop check <filename>.slop
```

- `UnfilledHoleError` messages are **expected** and can be ignored (holes will be filled later)
- Fix any other errors (type errors, syntax errors, undefined references) before considering the scaffold complete

### Output
Generate the complete `.slop` file with all types, constants, and function scaffolds. Validate with `slop check` and fix any non-hole errors.
