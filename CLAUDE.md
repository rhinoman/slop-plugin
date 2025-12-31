# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Claude Code plugin for SLOP (Symbolic LLM-Optimized Programming) - a language designed for hybrid human-machine code generation. The plugin provides a skill that teaches Claude how to generate SLOP code.

## Repository Structure

- `.claude-plugin/plugin.json` - Plugin manifest defining metadata, commands, and skill location
- `commands/` - Slash commands (`/slop:scaffold`, `/slop:fill`)
- `skills/slop/SKILL.md` - Main skill definition with language reference and generation guidelines
- `skills/slop/references/types.md` - Complete type system documentation
- `skills/slop/references/patterns.md` - Common patterns and idioms

## Testing the Plugin

Test locally with:
```bash
claude --plugin-dir /path/to/slop-plugin
```

## SLOP Language Key Concepts

SLOP uses S-expression syntax and transpiles to C. Key features:

- **Mandatory annotations**: Every function requires `@intent` and `@spec`
- **Contracts**: `@pre`, `@post`, `@pure`, `@example` for verification
- **Range types**: `(Int 0 .. 100)` catches bounds errors at compile time
- **Typed holes**: `(hole Type "prompt")` for incremental LLM generation
- **Parameter modes**: `in` (read-only), `out` (write-only), `mut` (read-write)
- **Arena allocation**: Pass `Arena` as first param for allocating functions

## SLOP Tooling

### Installation
```bash
pip install -e .
```

### Common Commands
```bash
slop parse file.slop              # Parse and inspect
slop parse file.slop --holes      # Show holes in file
slop check file.slop              # Type check
slop transpile file.slop -o out.c # Transpile to C
slop build file.slop -o binary    # Full build (requires cc)
```

### Hole Validation
```bash
slop check-hole '(+ x 1)' -t Int -p '((x Int))'
slop check-hole '(helper 42)' -t Int -c context.slop
echo '(ok val)' | slop check-hole -t '(Result T E)'
```

Files with unfilled holes will show `UnfilledHoleError` - this is expected for scaffolds. Fix all other errors.

## Slash Commands

- `/slop:scaffold <description>` - Generate SLOP functions/files with hole expressions as implementation stubs
- `/slop:fill <file>` - Fill holes in a SLOP file, respecting `:context`, `:required`, and contracts

## Scaffold Generation Guidelines

When modifying skill documentation for scaffold generation:

1. Use named types in signatures (not inline ranges)
2. Avoid overlapping enum variants across types
3. Quote enum variants in code: `'GET` not `GET`
4. Match constant types to their usage context
5. Module names must match filenames
