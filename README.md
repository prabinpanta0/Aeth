# Aeth

A hybrid Linux shell written in Haskell that bridges traditional text-based shells with modern structured data.

- **Version:** 0.4.0
- **License:** [MIT](LICENSE)

Aeth is a polymorphic shell that supports both traditional Unix commands and structured data operations:

- **Raw Mode:** `ls` returns text (traditional shell behavior)
- **Structured Mode:** `@ls` returns structured data (tables with columns)

---

## Features

### Core Shell

- Fast startup (~25ms)
- Process management via fork/exec
- Path resolution through `$PATH`
- Native I/O redirection (>, >>, <, 2>, 2>>, &>)
- Piping and command chaining (|, &&, ||, ;)
- Environment variable expansion
- Brace expansion ({a,b,c}, {1..10}, {a..z})
- Extended globbing (\*_/_.txt recursive patterns)
- Tab completion for commands and paths
- Command history with up/down arrow navigation
- Signal handling (Ctrl+C, Ctrl+D)
- Proper quoting support
- Custom source files and binary paths

### Built-in Commands

| Command            | Description                        |
| ------------------ | ---------------------------------- |
| `cd [path]`        | Change directory (supports `cd -`) |
| `pwd`              | Print working directory            |
| `exit`             | Exit shell                         |
| `export KEY=VALUE` | Set environment variable           |
| `unset KEY`        | Remove environment variable        |
| `history`          | Show command history               |
| `clear`            | Clear screen                       |
| `source FILE`      | Execute commands from file         |
| `type CMD`         | Show command type                  |
| `which CMD`        | Find executable path               |
| `echo [args]`      | Print arguments                    |
| `true` / `false`   | Return exit codes                  |
| `jobs`             | List background jobs               |
| `audit-verify`     | Verify audit log integrity         |
| `audit-export`     | Export audit log as JSON           |
| `audit-hash`       | Hash a command string              |

### Structured Commands

| Command                  | Description             |
| ------------------------ | ----------------------- |
| `@ls [path]`             | List directory as table |
| `@ls -a`                 | Include hidden files    |
| `@pwd`                   | Print working directory |
| `@ps`                    | Process list as table   |
| `@df`                    | Disk usage as table     |
| `@env`                   | Environment variables   |
| `@find [path] [pattern]` | Find files recursively  |

### Structured Transformations

| Command                      | Description                    |
| ---------------------------- | ------------------------------ |
| `filter { .field op value }` | Filter table rows              |
| `sort .field`                | Sort table by column           |
| `select .field1 .field2`     | Select specific columns        |
| `count`                      | Count rows                     |
| `unique [.field]`            | Deduplicate rows               |
| `head N` / `tail N`          | Take/skip N rows               |
| `json`                       | Output as clean JSON           |

**Filter operators:** `==`, `!=`, `>`, `>=`, `<`, `<=`, `contains`

Numeric fields (`%CPU`, `%MEM`, `size`) use numeric comparison automatically:

```bash
@ls | filter { .size > 1MB }
@ls | filter { .kind == dir }
@ps | filter { .%CPU > 1 }
@ls | filter { .permissions == rw-r--r-- }
@ls | filter { .name contains "dist" }
@find | filter { .path contains "dist" }

# Pipeline examples
@ls | filter { .kind == dir } | count
@ps | sort .%MEM | head 5
@ls | filter { .kind == file } | json
```

### Audit Log (Blockchain-Inspired)

Every command is logged with a hash chain for tamper detection:

```bash
audit-verify     # Verify audit log integrity
audit-export     # Export audit log as JSON
audit-hash cmd   # Hash a command string
```

The audit log is persisted to `~/.config/aeth/audit.log` with chained hashes.

### Configurable Prompt

Prompt styles: `minimal`, `powerline`, `simple`, or custom format.

Custom prompt placeholders: `{cwd}`, `{user}`, `{host}`, `{branch}`, `{exit}`, `{git}`

### Command Aliases

Define in config.toml:

```toml
alias.ll = "ls -la"
alias.la = "@ls -a"
```

---

## Installation

Requirements: GHC 9.6+ and Cabal

```bash
git clone https://github.com/prabinpanta0/aeth
cd Aeth
cabal build
cabal run aeth
```

---

## Usage

### Interactive Mode

```bash
./aeth
```

### Non-Interactive Mode

```bash
./aeth -c "ls -la"
./aeth -c "@ls"
```

---

## Configuration

Location: `~/.config/aeth/config.toml`

```bash
mkdir -p ~/.config/aeth
cp config/config.toml ~/.config/aeth/config.toml
```

See [docs/CONFIGURATION.md](docs/CONFIGURATION.md) for details.

---

## Documentation

- [Configuration Guide](docs/CONFIGURATION.md)
- [Architecture Overview](docs/ARCHITECTURE.md)

---

## Roadmap

### Completed

- Fast startup (~25ms)
- Tab completion
- Proper quoting support
- Environment variable expansion
- Signal handling
- Configurable prompts
- Command history with arrow navigation
- Visual file/directory distinction
- Structured commands (@ls, @ps, @df, @env, @find)
- Structured transformations (filter, sort, select, count, unique, head, tail, json)
- Command aliases
- Full I/O redirection (via /bin/sh delegation)
- Job control (fg, bg)
- Syntax highlighting
- Auto-suggestions (fish-like history suggestions, Right arrow to accept)
- Command chaining (&&, ||, ;)
- Native I/O redirection (>, >>, <, 2>, 2>>, &>)
- Brace expansion ({a,b,c}, {1..10}, {a..z})
- Extended globbing (`**/*.txt` recursive patterns)
- Source files/folders feature (source.N and path.N in config)
- Numeric comparison for filter (> < >= <= auto-detect numbers)
- JSON output for LLM consumption (`@ls | json`)
- Blockchain-inspired audit log with hash chaining
- `--help` and `--version` flags

### Planned

- Process substitution (<(cmd), >(cmd))
- Arithmetic expansion
- More structured commands

---

## License

MIT License - see [LICENSE](LICENSE) file.
