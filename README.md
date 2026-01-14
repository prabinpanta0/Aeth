# Aeth

[![prabinpanta0](https://img.shields.io/badge/prabinpanta0-red)](https://github.com/prabinpanta0)
![NISH](https://img.shields.io/badge/Aeth-Nesh-green)

An elegant polymorphic shell that lacks the concept of elegance.

-   **Version:** 0.1.0
-   **License:** [MIT](LICENSE)

<div align="center">
  <img src="docs/assets/aeth.png" alt="Aeth Prompt" width="600"/>
</div>

A hybrid Linux shell written in Haskell, designed to bridge the legacy world of raw text with the modern world of structured data. Aeth is enjoyable to use, and safe by construction.

Aeth is a **polymorphic shell** that doesn't force you into one paradigm. Instead of choosing between traditional Unix shells (text streams) or modern shells like Nushell (structured data), Aeth provides both:

-   **Raw Mode:** `ls` → Returns text (traditional shell behavior)
-   **Structured Mode:** `@ls` → Returns structured data (tables)

This allows you to leverage decades of Unix tools while also enjoying the benefits of type-safe structured data manipulation.

---

## Features

### Core Shell Capabilities

-   **⚡ Fast Startup:** ~25ms launch time
-   **Process Management:** Fork/exec to launch external programs
-   **Path Resolution:** Finds executables through `$PATH`
-   **Piping:** Connect stdout → stdin for commands
-   **Built-in Commands:** `cd`, `exit`, `export`, `pwd`, `history`, `clear`
-   **Environment Variables:** Full access to all system variables (`$HOME`, `$USER`, `$PATH`, etc.)
-   **Tab Completion:** Command and path completion
-   **Signal Handling:** Proper Ctrl+C and Ctrl+D handling
-   **IDE Compatible:** Works correctly in VS Code and other IDE terminals
-   **AI/Copilot Compatible:** Can receive programmatic input
-   **Proper Quoting:** Supports `"double quotes"`, `'single quotes'`, and `escape\ characters`

### Structured Commands

-   **`@ls [path]`** - List directory contents as structured table (name, kind, size)
-   **`@pwd`** - Print working directory as structured text
-   **`filter`** - Filter structured tables with expressions

### Configurable Prompt

Choose from built-in prompt styles or customize colors:

-   **minimal:** `~/projects (main) > `
-   **powerline:** `~/projects  main  ✓`
-   **simple:** `user@host:~/projects$ `

---

## Installation

### Prerequisites

-   GHC 9.6+ and Cabal

### Build

```bash
# Clone the repository
git clone https://github.com/prabinpanta0/Aeth
cd Aeth

# Build
cabal build

# Run
cabal run Aeth

# Or install to ~/.cabal/bin
cabal install
```

---

## Usage

### Interactive Mode

```bash
./aeth
```

Try these commands:

```bash
# Raw mode (traditional shell)
ls -la
echo "hello world"
cat README.md | grep Aeth

# Structured mode
@ls
@pwd
@ls /usr/bin

# Filtered structured output
@ls | filter { .size > 1MB }
@ls | filter { .kind == dir }
@ls | filter { .name contains test }
```

### Non-Interactive Mode

Execute commands directly:

```bash
./aeth -c "ls -la"
./aeth -c "@ls"
./aeth -c "echo 'hello world'"
```

### Filtering Structured Data

The `filter` command allows rich queries on structured tables:

```bash
@ls | filter { .size > 1MB }        # Files larger than 1MB
@ls | filter { .size >= 1024 }      # Files >= 1024 bytes
@ls | filter { .kind == dir }       # Only directories
@ls | filter { .kind != file }      # Everything except files
@ls | filter { .name contains kde } # Names containing "kde"
```

**Supported operators:** `==`, `!=`, `>`, `>=`, `<`, `<=`, `contains`  
**Supported fields:** `.name`, `.kind`, `.size`

---

## Configuration

Aeth uses a simple TOML-like configuration file.

### Default Location

```
~/.config/Aeth/config.toml
```

### Example Configuration

```toml
# UI Mode: 'normal' or 'tui' (fullscreen)
ui_mode = "normal"

# Prompt Style: 'minimal', 'powerline', or 'simple'
prompt_style = "minimal"

# Prompt features
show_git_branch = true
show_exit_code = true
show_duration = false

# Colors: black, red, green, yellow, blue, magenta, cyan, white, gray
cwd_color = "cyan"
git_color = "magenta"
error_color = "red"
success_color = "green"
```

### Quick Setup

```bash
# Create config directory
mkdir -p ~/.config/Aeth

# Copy template
cp config/config.toml ~/.config/Aeth/config.toml

# Edit to customize
$EDITOR ~/.config/Aeth/config.toml
```

For detailed configuration options, see [docs/CONFIGURATION.md](docs/CONFIGURATION.md).

---

## Documentation

-   [Configuration Guide](docs/CONFIGURATION.md) - Customize prompts, colors, and settings
-   [Architecture Overview](docs/ARCHITECTURE.md) - Understanding the hybrid shell design

---

## Current Limitations

This is an early prototype. Known limitations:

-   Multi-stage raw pipelines are delegated to `/bin/sh -c`
-   Structured pipelines (`@a | @b`) not yet fully implemented
-   Limited I/O redirection support (`<`, `>`, `2>`)
-   Small set of built-in structured commands

---

## Roadmap

### Completed ✅

-   [x] Fast startup (~25ms)
-   [x] Tab completion (commands + paths)
-   [x] Proper quoting support
-   [x] Environment variable expansion
-   [x] Signal handling (Ctrl+C, Ctrl+D)
-   [x] IDE/AI compatibility
-   [x] Configurable prompts

### Next Milestones

1. I/O redirection (`<`, `>`, `>>`, `2>`)
2. Job control (background processes, `&`, `fg`, `bg`)
3. Command aliases
4. More structured commands (`@find`, `@ps`, `@df`)
5. Structured pipeline chaining (`@ls | @filter | @sort`)

---

## Contributing

Please read the [architecture docs](docs/ARCHITECTURE.md) to understand the basic design.

---

## License

MIT License - see [LICENSE](LICENSE) file for details.
