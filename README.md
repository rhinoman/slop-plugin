# SLOP Plugin for Claude Code

A Claude Code plugin for SLOP (Symbolic LLM-Optimized Programming) - a language designed for hybrid human-machine code generation.

## What is SLOP?

SLOP is a language where:
- **Humans** specify intent via contracts, types, and examples
- **Machines** generate implementation and transpile to C
- **Type system** catches errors early with range bounds, exhaustive matching, and contract violations

Key features:
- S-expression syntax (Lisp-like)
- Mandatory contracts (`@intent`, `@spec`, `@pre`, `@post`)
- Range types: `(Int 0 .. 100)` catches bounds errors at compile time
- Typed holes for LLM generation with complexity tiers
- Transpiles to C for performance

## Installation

### Local Development

Test the plugin locally:

```bash
claude --plugin-dir /path/to/slop-plugin
```

### From Git Repository

Clone and use as a local plugin:

```bash
git clone https://github.com/yourusername/slop-plugin.git
claude --plugin-dir ./slop-plugin
```

## Usage

Once installed, the SLOP skill is automatically available. Ask Claude to:

- Generate SLOP code for your specifications
- Create scaffolds with typed holes for incremental generation
- Write code with strong contracts and range types
- Transpile SLOP to efficient C code

## Example

```lisp
(fn clamp ((in x Int) (in lo Int) (in hi Int))
  (@intent "Clamp value to range")
  (@spec ((Int Int Int) -> Int))
  (@pre (<= lo hi))
  (@post (and (>= $result lo) (<= $result hi)))
  (@pure)
  (@example (5 0 10) -> 5)
  (@example (-5 0 10) -> 0)
  (@example (15 0 10) -> 10)
  (cond
    ((< x lo) lo)
    ((> x hi) hi)
    (else x)))
```

## Plugin Contents

- `skills/slop/SKILL.md` - Main skill definition with language reference
- `skills/slop/references/types.md` - Complete type system documentation
- `skills/slop/references/patterns.md` - Common patterns and idioms

## License

MIT
