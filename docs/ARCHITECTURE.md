# Architecture Overview

This document explains the internal design of Aeth and how the hybrid shell concept is implemented.

## Core Concept

Aeth is built on **polymorphic command execution**. Every command can operate in one of two modes:

1. **Raw Mode:** Traditional string-based I/O (like Bash)
2. **Structured Mode:** Type-safe data structures (like Nushell)

The user chooses the mode with a prefix:

-   `ls` → Raw mode (returns text)
-   `@ls` → Structured mode (returns `StructuredValue`)

---

## Module Structure

```
app/
  Main.hs                    -- Entry point, argument parsing

src/Aeth/
  Types.hs                   -- Core types (OutputMode, Segment, Pipeline, ShellState)
  Parse.hs                   -- Parser with quoting support
  ParseMegaparsec.hs         -- Megaparsec-based parser (alternative)
  Exec.hs                    -- Command execution (raw & structured)
  Structured.hs              -- Structured value types and commands

  # Fast Shell (Default)
  ShellFast.hs               -- Main shell using haskeline
  ConfigFast.hs              -- Simple TOML-like config parser

  # Legacy Shell (--legacy flag)
  Shell.hs                   -- Legacy REPL with hint-based config
  Config.hs                  -- Dynamic config loading via GHC interpreter
  LineEditor.hs              -- Custom line editor
  LineEditorVty.hs           -- Vty-based fullscreen UI

config/
  config.toml                -- Configuration template
  rc                         -- Startup commands template
  config.hs.legacy           -- Legacy Haskell config template
```

---

## Core Types

### OutputMode

```haskell
data OutputMode
  = RawString    -- Traditional text output
  | Structured   -- Structured data (tables, records)
  deriving (Eq, Show)
```

### Segment

A single command in a pipeline:

```haskell
data Segment = Segment
  { segMode :: OutputMode     -- Raw or Structured
  , segName :: T.Text         -- Command name (e.g., "ls", "@ls")
  , segArgs :: [T.Text]       -- Arguments
  }
```

### Pipeline

```haskell
newtype Pipeline = Pipeline { unPipeline :: [Segment] }
```

### ShellState

```haskell
data ShellState = ShellState
  { cwd            :: FilePath
  , envOverrides   :: Map.Map String String
  , lastExitCode   :: Int
  , lastDurationMs :: Maybe Int
  }
```

### StructuredValue

```haskell
data StructuredValue
  = SText T.Text
  | STable [T.Text] [[T.Text]]  -- Headers + rows
  deriving (Eq, Show)
```

---

## Execution Flow

### 1. User Input

```
User types: @ls /home | filter { .size > 1MB }
```

### 2. Parsing

The parser (`Parse.hs`) tokenizes with quote support and splits into segments:

```haskell
Pipeline [
  Segment Structured "@ls" ["/home"],
  Segment Structured "filter" ["{", ".size", ">", "1MB", "}"]
]
```

### 3. Execution

The executor (`Exec.hs`) processes each segment:

**Raw Mode:**

-   Resolves command in `$PATH`
-   Forks process with `System.Process`
-   Connects stdio pipes if part of pipeline

**Structured Mode:**

-   Looks up in structured registry: `@ls`, `@pwd`, `@ps`, `@env`, `filter`, `select`
-   Returns `StructuredValue`
-   Renders as table

### 4. Built-ins

```haskell
case cmd of
  "cd"     -> builtin_cd args
  "exit"   -> builtin_exit
  "export" -> builtin_export args
  _        -> executeExternal cmd args
```

---

## Configuration System

### Fast Config (Default)

`ConfigFast.hs` uses a simple line-based parser:

```haskell
parseConfigLines :: [T.Text] -> Map.Map T.Text T.Text
```

Parses `key = value` format without external dependencies. **Startup: ~25ms**

### Legacy Config

`Config.hs` uses the `hint` library to interpret Haskell code at runtime. This allows arbitrary Haskell in configuration but causes **2-5 second startup delays**.

Enabled via `aeth --legacy`.

---

## Shell Modes

### ShellFast (Default)

Uses `haskeline` for line editing:

```haskell
mainLoop :: PromptFunction -> InputT IO ()
mainLoop promptFn = loop
  where
    loop = do
      st <- liftIO $ readIORef globalStateRef
      prompt <- liftIO $ promptFn st
      minput <- getInputLine prompt
      case minput of
        Nothing -> return ()
        Just line -> do
          liftIO $ executeOneLine (T.pack line)
          loop
```

Features:

-   Readline-style editing (Emacs/Vi)
-   History search (Ctrl-R)
-   Tab completion for commands and files
-   Proper PTY support (AI/Copilot compatible)
-   Signal handling (Ctrl-C, Ctrl-Z)

### Legacy Shell

Uses custom `LineEditor` with raw terminal mode. Has issues with:

-   AI assistants can't execute commands
-   Terminal state not always restored on exit

---

## Structured Commands

### @ls

```haskell
lsStructured :: FilePath -> IO StructuredValue
lsStructured path = do
  entries <- Dir.listDirectory path
  rows <- mapM (fileInfo path) entries
  pure (STable ["name", "kind", "size"] rows)
```

### @ps

```haskell
psStructured :: IO StructuredValue
psStructured = do
  output <- readProcess "ps" ["aux"] ""
  let rows = map (take 11 . words) (tail $ lines output)
  pure (STable psHeaders rows)
```

### filter

```haskell
filterStructured :: T.Text -> StructuredValue -> Either T.Text StructuredValue
filterStructured expr (STable headers rows) = do
  (field, op, literal) <- parsePredicate expr
  idx <- findFieldIndex field headers
  pure (STable headers (filter (evalPredicate op literal idx) rows))
```

Operators: `==`, `!=`, `>`, `>=`, `<`, `<=`, `contains`

---

## Performance

| Component            | Time                          |
| -------------------- | ----------------------------- |
| Fast shell startup   | ~25ms                         |
| Legacy shell startup | 2-5s                          |
| Prompt generation    | ~10-50ms (with git detection) |
| Structured commands  | ~1-10ms                       |

---

## Design Principles

1. **Safety Through Types** - Haskell prevents many runtime errors
2. **Explicit Mode Selection** - `@` prefix makes structured mode clear
3. **Interoperability** - Raw mode works with all Unix tools
4. **Functional State** - `StateT` keeps state clean without globals
5. **Fast by Default** - Simple config, quick startup

---

## Future Plans

-   Full quoting/escaping parser (Megaparsec)
-   Type-aware structured piping (`@ls | @filter`)
-   More structured commands (`@git-status`, `@docker`)
-   Rich table rendering (sorting, pagination)
-   Plugin system for custom commands

---

## References

### Inspiration

-   **Bash/Zsh** - Raw mode semantics
-   **Nushell** - Structured data concept
-   **Fish** - User experience
-   **Xonsh** - Hybrid shell idea

### Haskell Libraries

-   `haskeline` - Line editing
-   `vty` - Terminal UI
-   `process` - Process management
-   `mtl` - Monad transformers
