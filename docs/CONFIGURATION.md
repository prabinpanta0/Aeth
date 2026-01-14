# Configuration Guide

This guide explains how to customize Aeth to match your workflow and preferences.

## Quick Start

```bash
# Create config directory
mkdir -p ~/.config/Aeth

# Copy templates
cp config/config.toml ~/.config/Aeth/config.toml
cp config/rc ~/.config/Aeth/rc
```

## Configuration Files

Aeth looks for configuration at:

| File                         | Purpose                                   |
| ---------------------------- | ----------------------------------------- |
| `~/.config/Aeth/config.toml` | Main configuration                        |
| `~/.config/Aeth/rc`          | Startup commands (shell aliases, exports) |
| `~/.config/Aeth/history`     | Command history (auto-managed)            |

---

## config.toml

The main configuration uses a simple key=value format:

```toml
# Aeth Shell Configuration

ui_mode = "normal"        # 'normal' or 'tui' (fullscreen)
prompt_style = "minimal"  # 'minimal', 'powerline', or 'simple'

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

### Options Reference

| Option            | Values                           | Default   | Description             |
| ----------------- | -------------------------------- | --------- | ----------------------- |
| `ui_mode`         | `normal`, `tui`                  | `normal`  | Interface mode          |
| `prompt_style`    | `minimal`, `powerline`, `simple` | `minimal` | Prompt appearance       |
| `show_git_branch` | `true`, `false`                  | `true`    | Show git branch         |
| `show_exit_code`  | `true`, `false`                  | `true`    | Show error indicator    |
| `show_duration`   | `true`, `false`                  | `false`   | Show execution time     |
| `cwd_color`       | color name                       | `cyan`    | Directory color         |
| `git_color`       | color name                       | `magenta` | Git branch color        |
| `error_color`     | color name                       | `red`     | Error indicator color   |
| `success_color`   | color name                       | `green`   | Success indicator color |

### Prompt Styles

**minimal** - Clean and simple:

```
~/projects (main) >
```

**powerline** - Fancy with segments:

```
~/projects  main  ✓
```

**simple** - Classic bash-like:

```
user@host:~/projects$
```

---

## rc (Startup Commands)

The `rc` file runs shell commands at startup:

```sh
# Environment variables
export EDITOR=vim
export PATH="$HOME/bin:$PATH"

# Aliases (via shell passthrough)
# alias ll='ls -la'  # Not yet supported, use functions instead
```

Each line is executed as a shell command. Lines starting with `#` are comments.

---

## Structured Data Filtering

Aeth's structured commands (`@ls`, `@ps`, `@env`) support filtering:

```bash
@ls | filter { .size > 1MB }
@ls | filter { .kind == dir }
@ls | filter { .name contains config }
@env | filter { .name contains PATH }
```

### Filter Syntax

**Operators:** `==`, `!=`, `>`, `>=`, `<`, `<=`, `contains`

**Fields (from @ls):** `.name`, `.kind`, `.size`

**Size units:** `B`, `KB`, `MB`, `GB` (case-insensitive)

---

## Troubleshooting

### Config Not Loading

1. Check path: `~/.config/Aeth/config.toml`
2. Verify syntax (key = value format)
3. Run `aeth --debug` for verbose output

### Icons Not Showing

1. Install a [Nerd Font](https://www.nerdfonts.com/)
2. Configure your terminal to use it

### Terminal Issues

If your terminal is broken after exit:

-   This should be fixed in the current version (using haskeline)
-   Try running `reset` in your normal shell

---

## Legacy Configuration

The old `config.hs` system (Haskell-based configuration) is deprecated but still supported via `--legacy` flag:

```bash
aeth --legacy  # Use old hint-based config
```

**Why deprecated?** The `hint` library (GHC interpreter) caused 2-5 second startup delays. The new TOML system starts in ~25ms.

For legacy configuration documentation, see the [config.hs.legacy](../config/config.hs.legacy) template.

---

## Next Steps

-   See [ARCHITECTURE.md](ARCHITECTURE.md) for shell internals
-   Explore structured commands: `@ls`, `@ps`, `@env`
-   Customize your prompt style and colors
