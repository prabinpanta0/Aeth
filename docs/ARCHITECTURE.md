# Architecture Overview

This document explains the internal design of Aeth and how the hybrid shell concept is implemented.

Aeth is built on a simple idea: **polymorphic command execution**. Every command can operate in one of two modes:

1. **Raw Mode:** Traditional string-based I/O (like Bash)
2. **Structured Mode:** Type-safe data structures (like Nushell, but in Haskell)

The user chooses the mode with a prefix:

-   `ls` → Raw mode (returns text)
-   `@ls` → Structured mode (returns `StructuredValue`)

---

## Module Structure

```
app/
  Main.hs                    -- Entry point, arg parsing
src/Aeth/
  Types.hs                   -- Core types (OutputMode, Segment, Pipeline, ShellState)
  Parse.hs                   -- Simple whitespace-based parser
  ParseMegaparsec.hs         -- Future: full parser with quoting/escaping
  Shell.hs                   -- REPL loop, state management
  Exec.hs                    -- Command execution (raw & structured)
  Structured.hs              -- Structured value types and commands
  Config.hs                  -- Config loading (dynamic)
  LineEditor.hs              -- Haskeline wrapper (normal UI)
  LineEditorVty.hs           -- Vty-based fullscreen UI
config/
  config.hs                  -- User configuration template
```

---

## Core Types

### OutputMode

```haskell
data OutputMode
  = RawString    -- Traditional text output
  | Structured   -- Structured data (tables, records, etc.)
  deriving (Eq, Show)
```

### Segment

A single command in a pipeline:

```haskell
data Segment = Segment
  { segMode :: OutputMode     -- Raw or Structured
  , segName :: T.Text          -- Command name (e.g., "ls", "@ls")
  , segArgs :: [T.Text]        -- Arguments
  }
```

### Pipeline

```haskell
newtype Pipeline = Pipeline { unPipeline :: [Segment] }
```

A pipeline is a list of segments connected by `|`.

### ShellState

```haskell
data ShellState = ShellState
  { cwd            :: FilePath                    -- Current working directory
  , envOverrides   :: Map.Map String String       -- Environment variable overrides
  , lastExitCode   :: Int                         -- Exit code of last command
  , lastDurationMs :: Maybe Int                   -- Duration of last command (ms)
  }
```

Managed via `StateT ShellState IO` throughout the shell.

### StructuredValue

```haskell
data StructuredValue
  = SText T.Text                      -- Simple text
  | STable [T.Text] [[T.Text]]        -- Table: headers + rows
  deriving (Eq, Show)
```

This is the foundation for structured data. Future versions will add:

-   `SRecord [(T.Text, StructuredValue)]` - Key-value records
-   `SList [StructuredValue]` - Lists
-   `SInt`, `SFloat`, `SBool` - Primitives

---

## Execution Flow

### 1. User Input

```
User types: @ls /home | filter { .size > 1MB }
```

### 2. Parsing

The parser (`Parse.hs`) splits the input into segments:

```haskell
Pipeline [
  Segment Structured "@ls" ["/home"],
  Segment Structured "filter" ["{", ".size", ">", "1MB", "}"]
]
```

The parser detects `@` prefix and sets `segMode = Structured`.

### 3. Execution

The executor (`Exec.hs`) processes each segment:

```haskell
runPipeline :: Pipeline -> StateT ShellState IO ()
```

For each segment:

**Raw Mode:**

-   Resolves command in `$PATH`
-   Forks process with `System.Process`
-   Connects stdio pipes if part of pipeline
-   Delegates multi-segment raw pipelines to `/bin/sh -c` (for now)

**Structured Mode:**

-   Looks up command in structured registry:
    ```haskell
    "@ls"     -> lsStructured
    "@pwd"    -> pwdStructured
    "filter"  -> filterStructured
    ```
-   Executes function, returns `StructuredValue`
-   Renders with `renderStructured` (pretty-printing)

### 4. Built-ins

Special commands bypass external execution:

```haskell
case cmd of
  "cd"   -> builtin_cd args
  "exit" -> builtin_exit
  _      -> executeExternal cmd args
```

---

## Structured Command Implementation

### Example: @ls

Located in `Structured.hs`:

```haskell
lsStructured :: FilePath -> IO StructuredValue
lsStructured path = do
  entries <- Dir.listDirectory path
  rows <- mapM (fileInfo path) entries
  pure (STable ["name", "kind", "size"] rows)

fileInfo :: FilePath -> String -> IO [T.Text]
fileInfo base name = do
  let fp = base </> name
  stat <- Posix.getFileStatus fp
  let kind = if Posix.isDirectory stat then "dir" else "file"
  let size = T.pack (show (Posix.fileSize stat))
  pure [T.pack name, T.pack kind, size]
```

Returns a table with columns: `name`, `kind`, `size`.

### Example: filter

```haskell
filterStructured :: T.Text -> StructuredValue -> Either T.Text StructuredValue
filterStructured expr (STable headers rows) = do
  (field, op, literal) <- parsePredicate expr
  idx <- findFieldIndex field headers
  let keepRow row = evalPredicate field op literal (row !! idx)
  pure (STable headers (filter keepRow rows))
```

Supports predicates like:

-   `.size > 1MB`
-   `.kind == dir`
-   `.name contains test`

**Operators:** `==`, `!=`, `>`, `>=`, `<`, `<=`, `contains`

---

## Configuration System

### Dynamic Loading

At startup, the shell loads `~/.config/Aeth/config.hs` using the `hint` library (Haskell interpreter):

```haskell
loadConfig :: IO (ShellConfig, Maybe String)
loadConfig = do
  home <- Dir.getHomeDirectory
  let cfgPath = home </> ".config/Aeth/config.hs"
  exists <- Dir.doesFileExist cfgPath
  if not exists
    then pure (defaultConfig, Nothing)
    else do
      result <- Hint.runInterpreter $ do
        Hint.loadModules [cfgPath]
        Hint.setTopLevelModules ["Config"]
        useTui <- Hint.interpret "useTui" (Hint.as :: Bool)
        promptFn <- Hint.interpret "myPromptSegments" ...
        extensions <- Hint.interpret "myExtensions" ...
        pure (ShellConfig useTui promptFn extensions)
      case result of
        Left err  -> pure (defaultConfig, Just (show err))
        Right cfg -> pure (cfg, Nothing)
```

This allows users to write arbitrary Haskell in their config while maintaining type safety.

---

## REPL Architecture

### Normal Mode (Haskeline)

```haskell
loopNormal :: ShellConfig -> StateT ShellState IO ()
loopNormal cfg = withLineEditor $ \ed -> go ed
  where
    go ed = do
      st <- get
      promptStr <- liftIO (prompt cfg st)
      history <- liftIO readHistory
      mLine <- liftIO (getLineEdited ed [] promptStr history)
      case mLine of
        Nothing   -> pure ()  -- Ctrl-D
        Just line -> do
          liftIO (appendHistory line)
          runOne (T.pack line)
          go ed
```

Provides:

-   Line editing (Emacs/Vi keybindings)
-   History search (Ctrl-R)
-   Tab completion (future)

### TUI Mode (Vty)

```haskell
loopTui :: ShellConfig -> StateT ShellState IO ()
```

Fullscreen interface with:

-   Scrollback buffer (last 1000 lines)
-   Prompt at bottom
-   Real-time output rendering

Experimental; may have rough edges.

---

## State Management

The shell uses `StateT ShellState IO` to maintain state across commands:

```haskell
-- Change directory
modify' (\st -> st { cwd = newPath })

-- Update exit code
modify' (\st -> st { lastExitCode = code })

-- Track command duration
start <- liftIO getCurrentTime
runPipeline pipeline
end <- liftIO getCurrentTime
let ms = round (diffUTCTime end start * 1000)
modify' (\st -> st { lastDurationMs = Just ms })
```

This provides a clean, functional interface without mutable global state.

---

## Prompt System

Prompts are generated dynamically for each input cycle:

```haskell
prompt :: ShellConfig -> ShellState -> IO String
prompt cfg st = do
  let cwdPretty = prettyCwd (cwd st)      -- ~/proj instead of /home/user/proj
  gitBranch <- detectGitBranch (cwd st)   -- Git integration
  segments <- promptFn cfg cwdPretty gitBranch (lastExitCode st) (lastDurationMs st)
  pure (renderSegments segments)
```

Rendering uses ANSI escape codes:

```haskell
renderSegments :: [(String, String, String)] -> String
renderSegments = concatMap renderSeg
  where
    renderSeg (fg, bg, content) =
      ansiColor fg ++ ansiBg bg ++ content ++ ansiReset
```

---

## Future Architecture Plans

### 1. Full Parser (Megaparsec)

Replace the simple whitespace splitter with a real parser:

```haskell
-- Quoted strings
command "arg with spaces"

-- Redirects
command > output.txt 2>&1

-- Escapes
echo "hello\"world"
```

Implementation stub exists in `ParseMegaparsec.hs`.

### 2. Structured Piping

Currently, structured commands can't pipe to each other:

```bash
@ls | @filter { .size > 1MB }  # Not yet implemented
```

Will require:

-   Type-aware pipeline combinator
-   `StructuredValue` propagation between segments
-   Automatic Raw ↔ Structured conversion

### 3. Expanded Structured Commands

Add more built-ins:

-   `@ps` - Process table
-   `@env` - Environment variables as table
-   `@history` - Command history
-   `@git-status` - Git repo state
-   Custom user commands via config

### 4. Display Engine

Rich rendering for tables:

-   Column sorting (click column header)
-   Pagination (less-style)
-   Themes (colors, borders)
-   Export (CSV, JSON)

### 5. Type System

Full Haskell type inference for structured values:

```haskell
data StructuredValue
  = SInt Int
  | SText Text
  | SList [StructuredValue]
  | SRecord (Map Text StructuredValue)
  | STable [Text] [[StructuredValue]]  -- Strongly-typed rows
```

Enable type checking in pipelines:

```bash
@ls | filter { .size : Int > 1000 } | sort .size
```

---

## Design Principles

### 1. Safety Through Types

Haskell's type system prevents common shell scripting errors:

-   No uninitialized variables
-   No string-to-command injection (unless explicitly using raw mode)
-   Exhaustive pattern matching on command results

### 2. Explicit Mode Selection

The `@` prefix makes it obvious when structured mode is active. No guessing about data types.

### 3. Interoperability

Raw mode ensures Aeth can work with any existing Unix tool. Users aren't locked into a new ecosystem.

### 4. Functional State

`StateT` monad keeps state management clean without global variables or mutation.

### 5. Dynamic Configuration

Haskell-based config allows unlimited customization while maintaining type safety (checked at load time).

---

## Performance Considerations

### Prompt Generation

Dynamic detection (git, language icons, etc.) adds ~10-50ms latency. For faster prompts, disable expensive checks in config.

### Structured Commands

Currently synchronous. For long-running operations, consider:

-   Async execution with progress indicators
-   Lazy evaluation of large tables
-   Streaming results

### Parser

The current parser is O(n) in input length. Megaparsec will maintain similar performance with better features.

---

## Testing Strategy

Current testing is manual. Future plans:

### Unit Tests

-   Parser edge cases
-   Structured value operations
-   State transformations

### Integration Tests

-   Full command pipelines
-   Config loading
-   Built-in commands

### Property Tests (QuickCheck)

-   Parser → execute → render roundtrips
-   State invariants

---

## Contributing

When adding features, consider:

1. **Keep raw mode simple** - Don't break existing Unix assumptions
2. **Make structured mode explicit** - Clear syntax, clear types
3. **Maintain config compatibility** - Version migrations if needed
4. **Document type signatures** - Config API should be clear
5. **Test edge cases** - Empty pipelines, missing commands, etc.

On another note, just make it work then think about other things.

---

## References

### Inspiration

-   **Bash/Zsh:** Raw mode semantics
-   **Nushell:** Structured data concept
-   **Fish:** User experience polish
-   **Xonsh:** Python-based shell (similar hybrid idea)

### Haskell Libraries

-   `haskeline` - Line editing
-   `vty` - Terminal UI
-   `hint` - Dynamic code loading
-   `process` - Process management
-   `parsec/megaparsec` - Parsing
-   `mtl` - Monad transformers (StateT)

### Further Reading

-   [Unix Shell Programming](https://pubs.opengroup.org/onlinepubs/9699919799/)
-   [Nushell Book](https://www.nushell.sh/book/)
-   [Haskeline Docs](https://hackage.haskell.org/package/haskeline)
