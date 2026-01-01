Initialize a new SLOP project in the current directory.

## Instructions

Create a `slop.toml` configuration file by prompting the user for project details.

### Process

1. Check if `slop.toml` already exists - if so, ask before overwriting

2. Prompt the user for:
   - **Project name** (default: current directory name)
   - **Version** (default: "0.1.0")
   - **Entry point** (default: "src/main.slop")
   - **Output path** (default: "build/<project-name>")
   - **Build type**: executable, static, or shared (default: executable)

3. Ask if they want to configure LLM providers for hole filling (optional)

4. Generate the `slop.toml` file

### Template

```toml
[project]
name = "<project-name>"
version = "<version>"
entry = "<entry-point>"

[build]
output = "<output-path>"
include = ["src"]
type = "<build-type>"
debug = false

[build.link]
libraries = []
library_paths = []
```

### Optional: LLM Provider Configuration

If the user wants hole filling support, add:

```toml
[providers.ollama]
type = "ollama"
base_url = "http://localhost:11434"

[tiers.tier-1]
provider = "ollama"
model = "phi3:mini"

[tiers.tier-2]
provider = "ollama"
model = "llama3:8b"
```

### Starter File Template

Create `src/main.slop` (or the configured entry point) with this exact SLOP syntax:

```slop
;; main.slop - Entry point
(module main
  (export main))

(fn main ()
  (@intent "Program entry point")
  (@spec (() -> Unit))
  (println "Hello, SLOP!"))
```

**CRITICAL**: Use exact SLOP syntax - `(fn name () ...)` not `(defun ...)` or `(define ...)`.

### After Creation

1. Create the entry point directory if it doesn't exist (e.g., `src/`)
2. Create the starter file using the template above
3. Show the user what was created and next steps:
   ```
   Created slop.toml
   Created src/main.slop

   Next steps:
     slop check src/main.slop    # Type check
     slop build                   # Build project
   ```
