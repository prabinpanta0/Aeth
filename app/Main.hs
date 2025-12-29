module Main (main) where

import qualified Aeth.Shell as Shell
import qualified System.Environment as Env

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    ["-c", cmd] -> Shell.runCommandLine cmd
    _ -> Shell.run
