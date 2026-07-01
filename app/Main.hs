module Main (main) where

-- Use the fast shell (haskeline-based, no hint dependency)
import qualified Aeth.ShellFast as Shell
import qualified System.Environment as Env

version :: String
version = "aeth 0.3.0.0"

helpText :: String
helpText = unlines
  [ version,
    "",
    "A structured shell with built-in data processing commands.",
    "",
    "Usage:",
    "  aeth              Start interactive shell",
    "  aeth -c \"cmd\"     Execute a single command",
    "  aeth --help       Show this help message",
    "  aeth --version    Show version",
    "",
    "Structured commands (@ prefix for structured output):",
    "  @ls [path]        List files as a table",
    "  @ps               List processes as a table",
    "  @df               Disk space usage",
    "  @env              Environment variables",
    "  @find [path]      Find files",
    "",
    "Pipeline transforms (work with structured output):",
    "  filter { .field op value }   Filter rows",
    "  sort .field                  Sort by column",
    "  select .field1 .field2       Select columns",
    "  count                        Count rows",
    "  unique [.field]              Deduplicate",
    "  head N / tail N              Pagination",
    "  json                         Output as JSON",
    "",
    "Filter operators: ==, !=, >, <, >=, <=, contains",
    "Filter examples:",
    "  @ls | filter { .size > 1MB }",
    "  @ps | filter { .%CPU > 1.0 } | head 5",
    "  @ls | filter { .kind == dir } | count",
    "  @ls | json",
    "",
    "Audit commands:",
    "  audit-verify     Verify audit log integrity",
    "  audit-export     Export audit log as JSON",
    "  audit-hash cmd   Hash a command string"
  ]

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    ["-c", cmd] -> Shell.runCommandLine cmd
    ["--legacy"] -> do
      putStrLn "Note: --legacy mode uses the old shell with hint (slow startup)"
      runLegacyShell
    ["-h"] -> putStr helpText
    ["--help"] -> putStr helpText
    ["-v"] -> putStrLn version
    ["--version"] -> putStrLn version
    [] -> Shell.run
    _ -> do
      putStr helpText
      putStrLn "Error: unknown arguments. Use 'aeth --help' for usage."

-- | Run the legacy shell (with hint, slow startup)
runLegacyShell :: IO ()
runLegacyShell = do
  putStrLn "Legacy shell not available in this build."
  putStrLn "Use the default fast shell instead."
