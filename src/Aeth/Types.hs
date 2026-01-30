module Aeth.Types
  ( OutputMode (..),
    Segment (..),
    Pipeline (..),
    CommandList (..),
    ListOperator (..),
    ShellState (..),
    BackgroundJob (..),
    JobStatus (..),
    Redirection (..),
    RedirectType (..),
    emptyShellState,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Posix.Types (ProcessID)

data OutputMode
  = RawString
  | Structured
  deriving (Eq, Show)

-- | Redirection type
data RedirectType
  = RedirectIn -- <
  | RedirectOut -- >
  | RedirectAppend -- >>
  | RedirectErr -- 2>
  | RedirectErrAppend -- 2>>
  | RedirectErrToOut -- 2>&1
  | RedirectOutAndErr -- &>
  deriving (Eq, Show)

-- | A single redirection
data Redirection = Redirection
  { redirType :: RedirectType,
    redirTarget :: T.Text
  }
  deriving (Eq, Show)

data Segment = Segment
  { segMode :: OutputMode,
    segName :: T.Text,
    segArgs :: [T.Text],
    segRedirects :: [Redirection], -- Redirections for this segment
    segBackground :: Bool -- Run in background (&)
  }
  deriving (Eq, Show)

newtype Pipeline = Pipeline {unPipeline :: [Segment]}
  deriving (Eq, Show)

-- | Operator between commands in a command list
data ListOperator
  = OpAnd -- &&  (run next only if previous succeeded)
  | OpOr
  | -- | |  (run next only if previous failed)
    OpSeq -- ;   (run next unconditionally)
  deriving (Eq, Show)

-- | A command list is a sequence of pipelines connected by &&, ||, or ;
data CommandList = CommandList
  { -- | Each pipeline paired with the operator that follows it (Nothing for last)
    clPipelines :: [(Pipeline, Maybe ListOperator)]
  }
  deriving (Eq, Show)

-- | Job status for background processes
data JobStatus = Running | Stopped | Done
  deriving (Eq, Show)

-- | Background job information
data BackgroundJob = BackgroundJob
  { jobId :: Int,
    jobPid :: ProcessID,
    jobCommand :: T.Text,
    jobStatus :: JobStatus
  }
  deriving (Eq, Show)

data ShellState = ShellState
  { cwd :: FilePath,
    baseEnv :: Map.Map String String,
    envOverrides :: Map.Map String String,
    deletedEnv :: Set.Set String, -- Tombstone set for unset variables
    lastExitCode :: Int,
    lastDurationMs :: Maybe Int,
    history :: [String],
    backgroundJobs :: [BackgroundJob],
    nextJobId :: Int
  }
  deriving (Eq, Show)

emptyShellState :: FilePath -> Map.Map String String -> ShellState
emptyShellState initialCwd base =
  ShellState
    { cwd = initialCwd,
      baseEnv = base,
      envOverrides = Map.singleton "PWD" initialCwd,
      deletedEnv = Set.empty,
      lastExitCode = 0,
      lastDurationMs = Nothing,
      history = [],
      backgroundJobs = [],
      nextJobId = 1
    }
