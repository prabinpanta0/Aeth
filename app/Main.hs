module Main (main) where

-- Use the fast shell (haskeline-based, no hint dependency)
import qualified Aeth.ShellFast as Shell
import qualified System.Environment as Env

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    ["-c", cmd] -> Shell.runCommandLine cmd
    ["--legacy"] -> do
      -- For backward compatibility: run the old shell
      putStrLn "Note: --legacy mode uses the old shell with hint (slow startup)"
      -- Import and run the old shell module
      runLegacyShell
    _ -> Shell.run

-- | Run the legacy shell (with hint, slow startup)
runLegacyShell :: IO ()
runLegacyShell = do
  -- Dynamic import of the old shell
  putStrLn "Legacy shell not available in this build."
  putStrLn "Use the default fast shell instead."
