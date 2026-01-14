module Aeth.Types
  ( OutputMode (..),
    Segment (..),
    Pipeline (..),
    ShellState (..),
    emptyShellState,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data OutputMode
  = RawString
  | Structured
  deriving (Eq, Show)

data Segment = Segment
  { segMode :: OutputMode,
    segName :: T.Text,
    segArgs :: [T.Text]
  }
  deriving (Eq, Show)

newtype Pipeline = Pipeline {unPipeline :: [Segment]}
  deriving (Eq, Show)

data ShellState = ShellState
  { cwd :: FilePath,
    baseEnv :: Map.Map String String,
    envOverrides :: Map.Map String String,
    lastExitCode :: Int,
    lastDurationMs :: Maybe Int,
    history :: [String]
  }
  deriving (Eq, Show)

emptyShellState :: FilePath -> Map.Map String String -> ShellState
emptyShellState initialCwd base =
  ShellState
    { cwd = initialCwd,
      baseEnv = base,
      envOverrides = Map.singleton "PWD" initialCwd,
      lastExitCode = 0,
      lastDurationMs = Nothing,
      history = []
    }
