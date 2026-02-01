{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Aeth.Exec
  ( runPipeline,
    runPipelineCapture,
    runCommandList,
    runCommandListCapture,
  )
where

import Aeth.Parse (parsePipeline)
import Aeth.Structured
import Aeth.Types
import Control.Exception (IOException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, get, put)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.IO (IOMode (..), hClose, openFile, stderr)
import System.Posix.Process (getProcessStatus)
import System.Posix.Signals (sigCONT, signalProcess)
import qualified System.Process as Proc

-- | Shared list of shell builtin commands
shellBuiltins :: [T.Text]
shellBuiltins =
  [ "cd",
    "exit",
    "export",
    "unset",
    "pwd",
    "history",
    "clear",
    "source",
    ".",
    "type",
    "which",
    "echo",
    "true",
    "false",
    "jobs",
    "fg",
    "bg"
  ]

exitCodeToInt :: Exit.ExitCode -> Int
exitCodeToInt ec =
  case ec of
    Exit.ExitSuccess -> 0
    Exit.ExitFailure n -> n

setLastExit :: (MonadIO m) => Exit.ExitCode -> StateT ShellState m ()
setLastExit ec = do
  st <- get
  put st {lastExitCode = exitCodeToInt ec}

-- | Type alias for alias map
type AliasMap = Map.Map T.Text T.Text

-- | Expand aliases in a segment
-- Note: We only apply aliases to raw commands, not structured (@) commands.
-- For structured commands, look up the alias with the "@" prefix.
expandAlias :: AliasMap -> Segment -> Segment
expandAlias aliasMap seg =
  case segMode seg of
    Structured ->
      -- For structured commands, look up alias with "@" prefix (e.g., "@ls" -> "@ls -a")
      case Map.lookup ("@" <> segName seg) aliasMap of
        Nothing -> seg -- No structured alias defined, use as-is
        Just expanded ->
          let tokens = T.words expanded
           in case tokens of
                [] -> seg
                (cmd : args) ->
                  let isStructured = T.isPrefixOf "@" cmd
                      mode = if isStructured then Structured else RawString
                      name = if isStructured then T.drop 1 cmd else cmd
                   in seg {segMode = mode, segName = name, segArgs = args ++ segArgs seg}
    RawString ->
      -- For raw commands, look up alias by name
      case Map.lookup (segName seg) aliasMap of
        Nothing -> seg
        Just expanded ->
          let tokens = T.words expanded
           in case tokens of
                [] -> seg
                (cmd : args) ->
                  let isStructured = T.isPrefixOf "@" cmd
                      mode = if isStructured then Structured else RawString
                      name = if isStructured then T.drop 1 cmd else cmd
                   in seg {segMode = mode, segName = name, segArgs = args ++ segArgs seg}

runPipeline :: (MonadIO m) => AliasMap -> Pipeline -> StateT ShellState m ()
runPipeline aliasMap (Pipeline segments) =
  let expandedSegments = map (expandAlias aliasMap) segments
   in case expandedSegments of
        [] -> pure ()
        [seg] -> runSingle seg
        _ -> runMulti expandedSegments

-- | Like 'runPipeline' but captures stdout/stderr into a returned Text.
-- Used by the interactive Vty UI so output stays visible.
runPipelineCapture :: Pipeline -> StateT ShellState IO T.Text
runPipelineCapture (Pipeline segments) =
  case segments of
    [] -> pure ""
    [seg] -> runSingleCapture seg
    _ -> runMultiCapture segments

-- | Run a command list, handling &&, ||, ; operators
runCommandList :: (MonadIO m) => AliasMap -> CommandList -> StateT ShellState m ()
runCommandList aliasMap (CommandList parts) = go parts
  where
    go [] = pure ()
    go ((pipeline, mOp) : rest) = do
      runPipeline aliasMap pipeline
      st <- get
      let exitCode = lastExitCode st
      case mOp of
        Nothing -> pure () -- Last command, done
        Just OpSeq -> go rest -- ; always continues
        Just OpAnd ->
          if exitCode == 0
            then go rest -- && continues only on success
            else pure ()
        Just OpOr ->
          if exitCode /= 0
            then go rest
            -- \|| continues only on failure
            else pure ()

-- | Like 'runCommandList' but captures output
runCommandListCapture :: CommandList -> StateT ShellState IO T.Text
runCommandListCapture (CommandList parts) = go parts []
  where
    go [] acc = pure (T.intercalate "\n" (filter (not . T.null) acc))
    go ((pipeline, mOp) : rest) acc = do
      out <- runPipelineCapture pipeline
      st <- get
      let exitCode = lastExitCode st
      case mOp of
        Nothing -> pure (T.intercalate "\n" (filter (not . T.null) (acc ++ [out])))
        Just OpSeq -> go rest (acc ++ [out])
        Just OpAnd ->
          if exitCode == 0
            then go rest (acc ++ [out])
            else pure (T.intercalate "\n" (filter (not . T.null) (acc ++ [out])))
        Just OpOr ->
          if exitCode /= 0
            then go rest (acc ++ [out])
            else pure (T.intercalate "\n" (filter (not . T.null) (acc ++ [out])))

runSingle :: (MonadIO m) => Segment -> StateT ShellState m ()
runSingle seg =
  case segMode seg of
    Structured -> runStructured seg
    RawString -> runRawSingle seg

runSingleCapture :: Segment -> StateT ShellState IO T.Text
runSingleCapture seg =
  case segMode seg of
    Structured -> do
      (v, ec) <- structuredValueFor seg
      setLastExit ec
      pure (renderStructured v)
    RawString -> runRawSingleCapture seg

-- | Check if a segment should be treated as structured, either explicitly (@cmd)
-- or implicitly (e.g. filter, select).
isStructuredSegment :: Segment -> Bool
isStructuredSegment s =
  segMode s == Structured || segName s `elem` ["filter", "select", "sort"]

runMulti :: (MonadIO m) => [Segment] -> StateT ShellState m ()
runMulti segs = do
  -- Minimal rule for now:
  -- - Structured pipelines can feed into raw pipelines by rendering structured output to stdin.
  -- - Fully-structured pipelines are not implemented yet.
  -- - Raw pipelines are executed via /bin/sh -c for correctness (quoting, redirects).
  let anyStructured = any isStructuredSegment segs
  if not anyStructured
    then do
      let cmdline = renderRawSegments segs
      runViaSh cmdline
    else case segs of
      (s0 : _)
        | segMode s0 == Structured || segName s0 `elem` ["filter", "select", "sort"] -> do
            let (structuredHead, rawTail) = splitStructuredHead segs
            (v, ec) <- runStructuredChain structuredHead
            setLastExit ec
            let stdinText = renderStructured v
            let cmdline = renderRawSegments rawTail
            if T.null (T.strip cmdline)
              then liftIO (TIO.putStrLn stdinText)
              else runViaShWithStdin stdinText cmdline
      _ ->
        liftIO $
          TIO.putStrLn
            "Structured pipelines require the first stage to be structured (start with @cmd, e.g. @ls)"

renderRawSegments :: [Segment] -> T.Text
renderRawSegments segs =
  T.intercalate
    " | "
    [ T.unwords (segName s : segArgs s)
      | s <- segs
    ]

runMultiCapture :: [Segment] -> StateT ShellState IO T.Text
runMultiCapture segs = do
  let anyStructured = any isStructuredSegment segs
  if not anyStructured
    then do
      let cmdline = renderRawSegments segs
      runViaShCapture cmdline
    else do
      case segs of
        (s0 : _)
          | segMode s0 == Structured || segName s0 `elem` ["filter", "select", "sort"] -> do
              let (structuredHead, rawTail) = splitStructuredHead segs
              (v, ec) <- runStructuredChain structuredHead
              setLastExit ec
              let stdinText = renderStructured v
              let cmdline = renderRawSegments rawTail
              if T.null (T.strip cmdline)
                then pure stdinText
                else runViaShWithStdinCapture stdinText cmdline
        _ ->
          pure "Structured pipelines require the first stage to be structured (start with @cmd, e.g. @ls)"

splitStructuredHead :: [Segment] -> ([Segment], [Segment])
splitStructuredHead = go True []
  where
    go _ acc [] = (reverse acc, [])
    go inStructured acc (s : rest)
      | inStructured && isStructuredTransformLike s = go True (asStructuredTransform s : acc) rest
      | otherwise = (reverse acc, s : rest)

isStructuredTransformLike :: Segment -> Bool
isStructuredTransformLike s =
  segMode s == Structured || segName s `elem` ["filter", "select", "sort"]

asStructuredTransform :: Segment -> Segment
asStructuredTransform s =
  if segMode s == Structured
    then s
    else s {segMode = Structured}

runStructuredChain :: (MonadIO m) => [Segment] -> StateT ShellState m (StructuredValue, Exit.ExitCode)
runStructuredChain segs =
  case segs of
    [] -> pure (SText "", Exit.ExitFailure 2)
    (s0 : rest) -> do
      (v0, ec0) <- structuredValueFor s0
      foldl step (pure (v0, ec0)) rest
  where
    step accM s = do
      (vPrev, _ecPrev) <- accM
      structuredApply s vPrev

structuredApply :: (MonadIO m) => Segment -> StructuredValue -> StateT ShellState m (StructuredValue, Exit.ExitCode)
structuredApply seg input =
  case segName seg of
    "filter" -> do
      let expr = T.unwords (segArgs seg)
      case filterStructured expr input of
        Left e -> pure (SText ("aeth: " <> e), Exit.ExitFailure 2)
        Right out -> pure (out, Exit.ExitSuccess)
    "sort" -> do
      let col = T.unwords (segArgs seg)
      case sortStructured col input of
        Left e -> pure (SText ("aeth: " <> e), Exit.ExitFailure 2)
        Right out -> pure (out, Exit.ExitSuccess)
    "select" -> do
      case selectStructured (segArgs seg) input of
        Left e -> pure (SText ("aeth: " <> e), Exit.ExitFailure 2)
        Right out -> pure (out, Exit.ExitSuccess)
    _ -> do
      -- Fallback: run as external command that consumes rendered input and captures stdout.
      st <- get
      let envMap = effectiveEnvMap st
      let name' = expandTextEnv envMap (segName seg)
      let args' = map (expandTextEnv envMap) (segArgs seg)
      liftIO $ externalToStructuredStdin (cwd st) envMap name' args' (renderStructured input)

externalToStructuredStdin :: FilePath -> Map.Map String String -> T.Text -> [T.Text] -> T.Text -> IO (StructuredValue, Exit.ExitCode)
externalToStructuredStdin workingDir envMap name args stdinText = do
  expandedArgs <- expandGlobs envMap workingDir args
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc (T.unpack name) (map T.unpack expandedArgs))
          { Proc.cwd = Just workingDir,
            Proc.env = Just envMerged
          }
  result <- try (Proc.readCreateProcessWithExitCode cp (T.unpack stdinText)) :: IO (Either IOException (Exit.ExitCode, String, String))
  case result of
    Left e -> pure (SText ("aeth: " <> renderExecException name e), Exit.ExitFailure 127)
    Right (ec, out, err) ->
      case ec of
        Exit.ExitSuccess -> pure (SText (T.pack out), ec)
        _ ->
          pure (SText (T.pack out <> (if null err then "" else "\n" <> T.pack err) <> "\n" <> "exit: " <> T.pack (show ec)), ec)

-- | Helper for cd: resolve target and change directory
doCd :: (MonadIO m) => ShellState -> Map.Map String String -> Maybe String -> StateT ShellState m ()
doCd st envMap target = do
  result <- liftIO (try (resolveCdTarget envMap target >>= Dir.setCurrentDirectory) :: IO (Either IOException ()))
  case result of
    Left e -> do
      liftIO (hPutShellError ("cd: " <> T.pack (show e)))
      put st {lastExitCode = 1}
    Right () -> do
      newDir <- liftIO Dir.getCurrentDirectory
      let oldDir = cwd st
      put
        st
          { cwd = newDir,
            envOverrides =
              Map.insert
                "PWD"
                newDir
                (Map.insert "OLDPWD" oldDir (envOverrides st)),
            lastExitCode = 0
          }

-- | Helper for cd in capture context: resolve target and change directory, return output
doCdCapture :: ShellState -> Map.Map String String -> Maybe String -> StateT ShellState IO T.Text
doCdCapture st envMap target = do
  result <- liftIO (try (resolveCdTarget envMap target >>= Dir.setCurrentDirectory) :: IO (Either IOException ()))
  case result of
    Left e -> do
      put st {lastExitCode = 1}
      pure ("cd: " <> T.pack (show e))
    Right () -> do
      newDir <- liftIO Dir.getCurrentDirectory
      let oldDir = cwd st
      put
        st
          { cwd = newDir,
            envOverrides =
              Map.insert
                "PWD"
                newDir
                (Map.insert "OLDPWD" oldDir (envOverrides st)),
            lastExitCode = 0
          }
      pure ""

runRawSingle :: (MonadIO m) => Segment -> StateT ShellState m ()
runRawSingle seg = do
  let name = segName seg
  let args = segArgs seg
  let redirs = segRedirects seg
  let isBg = segBackground seg
  -- Check for shell operators first; if present, delegate to /bin/sh so that
  -- commands like "cd app && ls" work correctly.
  if shouldUseShSegment seg
    then do
      let cmdline = renderRawSegment seg
      runViaSh cmdline
    else
      if name == ""
        then pure ()
        else case name of
          "exit" -> liftIO (Exit.exitWith Exit.ExitSuccess)
          "export" -> runExport args
          "unset" -> runUnset args
          "pwd" -> runPwd
          "history" -> runHistory
          "clear" -> liftIO $ do
            putStr "\ESC[H\ESC[2J\ESC[3J" -- ANSI clear screen + scrollback
          "source" -> runSource args
          "." -> runSource args -- POSIX alias for source
          "type" -> runType args
          "which" -> runWhich args
          "echo" -> runEchoWithRedirects args redirs
          "true" -> setLastExit Exit.ExitSuccess
          "false" -> setLastExit (Exit.ExitFailure 1)
          "jobs" -> runJobs
          "fg" -> runFg args
          "bg" -> runBg args
          "cd" -> do
            st <- get
            let envMap = effectiveEnvMap st
            -- Handle "cd -" specially: error if OLDPWD not set
            case args of
              ("-" : _) ->
                case Map.lookup "OLDPWD" envMap of
                  Nothing -> do
                    liftIO (hPutShellError "cd: OLDPWD not set")
                    put st {lastExitCode = 1}
                  Just oldpwd -> doCd st envMap (Just oldpwd)
              [] -> doCd st envMap Nothing
              (p : _) -> doCd st envMap (Just (T.unpack (expandTextEnv envMap p)))
          _ -> runExternalWithRedirects name args redirs isBg

runRawSingleCapture :: Segment -> StateT ShellState IO T.Text
runRawSingleCapture seg = do
  let name = segName seg
  let args = segArgs seg
  -- Check for shell operators first; if present, delegate to /bin/sh so that
  -- commands like "cd app && ls" work correctly.
  if shouldUseShSegment seg
    then runViaShCapture (renderRawSegment seg)
    else
      if name == ""
        then pure ""
        else case name of
          "exit" -> liftIO (Exit.exitWith Exit.ExitSuccess)
          "export" -> do
            runExport args
            pure ""
          "cd" -> do
            st <- get
            let envMap = effectiveEnvMap st
            -- Handle "cd -" specially: error if OLDPWD not set
            case args of
              ("-" : _) ->
                case Map.lookup "OLDPWD" envMap of
                  Nothing -> do
                    put st {lastExitCode = 1}
                    pure "cd: OLDPWD not set"
                  Just oldpwd -> doCdCapture st envMap (Just oldpwd)
              [] -> doCdCapture st envMap Nothing
              (p : _) -> doCdCapture st envMap (Just (T.unpack (expandTextEnv envMap p)))
          _ -> runExternalCapture name args

renderRawSegment :: Segment -> T.Text
renderRawSegment s =
  let base = T.unwords (segName s : segArgs s)
      redirs = T.concat (map renderRedirection (segRedirects s))
      bg = if segBackground s then " &" else ""
   in base <> redirs <> bg

-- | Render a redirection for shell delegation
renderRedirection :: Redirection -> T.Text
renderRedirection r =
  case redirType r of
    RedirectIn -> " < " <> redirTarget r
    RedirectOut -> " > " <> redirTarget r
    RedirectAppend -> " >> " <> redirTarget r
    RedirectErr -> " 2> " <> redirTarget r
    RedirectErrAppend -> " 2>> " <> redirTarget r
    RedirectErrToOut -> " 2>&1"
    RedirectOutAndErr -> " &> " <> redirTarget r

shouldUseShSegment :: Segment -> Bool
shouldUseShSegment s =
  -- Use /bin/sh only for complex shell syntax that we can't handle natively
  -- Note: Simple redirections (>, <, >>, 2>, etc.) are now handled natively
  -- Background jobs (&) are handled natively
  let cmd = renderRawSegment s
   in -- Only delegate to sh for very complex cases:
      -- - Command substitution: $(cmd) or `cmd`
      -- - Process substitution: <(cmd) or >(cmd) - TODO: implement natively
      -- - Pipelines within commands (rare edge case)
      T.isInfixOf "$(" cmd
        || T.isInfixOf "`" cmd
        || T.isInfixOf "<(" cmd
        || T.isInfixOf ">(" cmd
        || T.isInfixOf "|&" cmd

-- | Check if redirections can be handled natively
_canHandleRedirectsNatively :: [Redirection] -> Bool
_canHandleRedirectsNatively redirs =
  all isSimpleRedirect redirs
  where
    isSimpleRedirect r = case redirType r of
      RedirectIn -> True
      RedirectOut -> True
      RedirectAppend -> True
      RedirectErr -> True
      RedirectErrAppend -> True
      RedirectErrToOut -> True
      RedirectOutAndErr -> True

-- | Run an external command with native I/O redirection support
runExternalWithRedirects :: (MonadIO m) => T.Text -> [T.Text] -> [Redirection] -> Bool -> StateT ShellState m ()
runExternalWithRedirects name args redirs isBg = do
  st <- get
  let envMap = effectiveEnvMap st
  let name' = expandTextEnv envMap name
  let args' = map (expandTextEnv envMap) args
  expandedArgs <- liftIO (expandGlobs envMap (cwd st) args')
  let envMerged = Map.toList envMap

  -- Build CreateProcess with redirections and run, returning result for state update
  result <- liftIO $ do
    -- Prepare stdin/stdout/stderr handles based on redirections
    (stdinSpec, stdoutSpec, stderrSpec) <- buildStdSpecs (cwd st) envMap redirs

    let cp =
          (Proc.proc (T.unpack name') (map T.unpack expandedArgs))
            { Proc.cwd = Just (cwd st),
              Proc.env = Just envMerged,
              Proc.std_in = stdinSpec,
              Proc.std_out = stdoutSpec,
              Proc.std_err = stderrSpec
            }

    if isBg
      then do
        -- Start process in background
        (_, _, _, ph) <- Proc.createProcess cp
        pid <- Proc.getPid ph
        case pid of
          Just p -> do
            TIO.putStrLn $ "[" <> T.pack (show (nextJobId st)) <> "] " <> T.pack (show p)
            -- Return job info for state update
            return (Left (fromIntegral p, T.unwords (name : args)))
          Nothing -> return (Right Exit.ExitSuccess) -- Background with no PID, treat as success
      else do
        -- Run synchronously
        execResult <- try (Proc.withCreateProcess cp (\_ _ _ ph -> Proc.waitForProcess ph)) :: IO (Either IOException Exit.ExitCode)
        case execResult of
          Left e -> do
            hPutShellError (renderExecException name e)
            return (Right (Exit.ExitFailure 127))
          Right ec -> do
            when (ec /= Exit.ExitSuccess) $ hPutShellError ("exit: " <> T.pack (show ec))
            return (Right ec)

  -- Update state based on result
  case result of
    Left (pid, cmd) -> do
      -- Background job: add to job list and increment job ID
      let newJob =
            BackgroundJob
              { jobId = nextJobId st,
                jobPid = pid,
                jobCommand = cmd,
                jobStatus = Running
              }
      put
        st
          { backgroundJobs = backgroundJobs st ++ [newJob],
            nextJobId = nextJobId st + 1
          }
      setLastExit Exit.ExitSuccess -- Background jobs return success immediately
    Right ec ->
      setLastExit ec

-- | Build std_in, std_out, std_err specs from redirections
buildStdSpecs ::
  FilePath ->
  Map.Map String String ->
  [Redirection] ->
  IO (Proc.StdStream, Proc.StdStream, Proc.StdStream)
buildStdSpecs workDir envMap redirs = do
  let stdinRedir = List.find (\r -> redirType r == RedirectIn) redirs
      stdoutRedir = List.find (\r -> redirType r `elem` [RedirectOut, RedirectAppend, RedirectOutAndErr]) redirs
      stderrRedir = List.find (\r -> redirType r `elem` [RedirectErr, RedirectErrAppend, RedirectErrToOut, RedirectOutAndErr]) redirs

  stdinSpec <- case stdinRedir of
    Nothing -> return Proc.Inherit
    Just r -> do
      let path = expandPath workDir envMap (redirTarget r)
      h <- openFile (T.unpack path) ReadMode
      return (Proc.UseHandle h)

  stdoutSpec <- case stdoutRedir of
    Nothing -> return Proc.Inherit
    Just r -> case redirType r of
      RedirectOut -> do
        let path = expandPath workDir envMap (redirTarget r)
        h <- openFile (T.unpack path) WriteMode
        return (Proc.UseHandle h)
      RedirectAppend -> do
        let path = expandPath workDir envMap (redirTarget r)
        h <- openFile (T.unpack path) AppendMode
        return (Proc.UseHandle h)
      RedirectOutAndErr -> do
        let path = expandPath workDir envMap (redirTarget r)
        h <- openFile (T.unpack path) WriteMode
        return (Proc.UseHandle h)
      _ -> return Proc.Inherit

  -- For 2>&1 (RedirectErrToOut), stderr should use the same destination as stdout
  -- For RedirectOutAndErr, use the already-opened stdout handle instead of opening twice
  stderrSpec <- case stderrRedir of
    Nothing -> return Proc.Inherit
    Just r -> case redirType r of
      RedirectErr -> do
        let path = expandPath workDir envMap (redirTarget r)
        h <- openFile (T.unpack path) WriteMode
        return (Proc.UseHandle h)
      RedirectErrAppend -> do
        let path = expandPath workDir envMap (redirTarget r)
        h <- openFile (T.unpack path) AppendMode
        return (Proc.UseHandle h)
      RedirectErrToOut ->
        -- 2>&1: stderr goes where stdout goes
        return stdoutSpec
      RedirectOutAndErr ->
        -- &> or >&: share stdout's spec (already opened handle)
        return stdoutSpec
      _ -> return Proc.Inherit

  return (stdinSpec, stdoutSpec, stderrSpec)

-- | Expand path with ~ and environment variables
-- Note: ~username expansion is simplified - it uses $HOME + rest rather than
-- looking up the actual home directory of the user. For full POSIX behavior,
-- System.Posix.User.getUserEntryForName could be used.
expandPath :: FilePath -> Map.Map String String -> T.Text -> T.Text
expandPath workDir envMap path =
  let expanded = expandTextEnv envMap path
      str = T.unpack expanded
   in case str of
        ('~' : '/' : rest) -> case Map.lookup "HOME" envMap of
          Just home -> T.pack (home ++ "/" ++ rest)
          Nothing -> expanded
        ('~' : rest) -> case Map.lookup "HOME" envMap of
          -- Simplified: treats ~foo as $HOME/foo, not as foo's home directory
          Just home -> T.pack (home ++ "/" ++ rest)
          Nothing -> expanded
        ('/' : _) -> expanded -- Absolute path
        _ -> T.pack (workDir FP.</> str) -- Relative path

_runExternal :: (MonadIO m) => T.Text -> [T.Text] -> StateT ShellState m ()
_runExternal name args = runExternalWithRedirects name args [] False

runExternalCapture :: T.Text -> [T.Text] -> StateT ShellState IO T.Text
runExternalCapture name args = do
  st <- get
  let envMap = effectiveEnvMap st
  let name' = expandTextEnv envMap name
  let args' = map (expandTextEnv envMap) args
  expandedArgs <- liftIO (expandGlobs envMap (cwd st) args')
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc (T.unpack name') (map T.unpack expandedArgs))
          { Proc.cwd = Just (cwd st),
            Proc.env = Just envMerged
          }
  result <- liftIO (try (Proc.readCreateProcessWithExitCode cp "") :: IO (Either IOException (Exit.ExitCode, String, String)))
  case result of
    Left e -> do
      put st {lastExitCode = 127}
      pure ("aeth: " <> renderExecException name e)
    Right (ec, out, err) -> do
      setLastExit ec
      let outT = T.pack out
          errT = T.pack err
          extra =
            if ec == Exit.ExitSuccess
              then ""
              else "\n" <> "aeth: exit: " <> T.pack (show ec)
          merged =
            T.stripEnd (outT <> (if T.null errT then "" else if T.null outT then errT else "\n" <> errT) <> extra)
      pure merged

runViaSh :: (MonadIO m) => T.Text -> StateT ShellState m ()
runViaSh cmdline = do
  st <- get
  let envMap = effectiveEnvMap st
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc "/bin/sh" ["-c", T.unpack cmdline])
          { Proc.cwd = Just (cwd st),
            Proc.env = Just envMerged
          }
  ec <- liftIO (Proc.withCreateProcess cp (\_ _ _ ph -> Proc.waitForProcess ph))
  setLastExit ec

runViaShCapture :: T.Text -> StateT ShellState IO T.Text
runViaShCapture cmdline = do
  st <- get
  let envMap = effectiveEnvMap st
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc "/bin/sh" ["-c", T.unpack cmdline])
          { Proc.cwd = Just (cwd st),
            Proc.env = Just envMerged
          }
  result <- liftIO (try (Proc.readCreateProcessWithExitCode cp "") :: IO (Either IOException (Exit.ExitCode, String, String)))
  case result of
    Left e -> do
      put st {lastExitCode = 127}
      pure ("aeth: " <> T.pack (show e))
    Right (ec, out, err) -> do
      setLastExit ec
      let outT = T.pack out
          errT = T.pack err
          extra =
            if ec == Exit.ExitSuccess
              then ""
              else "\n" <> "aeth: exit: " <> T.pack (show ec)
      pure (T.stripEnd (outT <> (if T.null errT then "" else if T.null outT then errT else "\n" <> errT) <> extra))

runViaShWithStdin :: (MonadIO m) => T.Text -> T.Text -> StateT ShellState m ()
runViaShWithStdin stdinText cmdline = do
  st <- get
  let envMap = effectiveEnvMap st
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc "/bin/sh" ["-c", T.unpack cmdline])
          { Proc.cwd = Just (cwd st),
            Proc.env = Just envMerged,
            Proc.std_in = Proc.CreatePipe
          }
  ec <- liftIO $ Proc.withCreateProcess cp $ \mIn _ _ ph -> do
    case mIn of
      Nothing -> pure ()
      Just hIn -> do
        TIO.hPutStr hIn stdinText
        hClose hIn
    Proc.waitForProcess ph
  setLastExit ec

runViaShWithStdinCapture :: T.Text -> T.Text -> StateT ShellState IO T.Text
runViaShWithStdinCapture stdinText cmdline = do
  st <- get
  let envMap = effectiveEnvMap st
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc "/bin/sh" ["-c", T.unpack cmdline])
          { Proc.cwd = Just (cwd st),
            Proc.env = Just envMerged
          }
  result <- liftIO (try (Proc.readCreateProcessWithExitCode cp (T.unpack stdinText)) :: IO (Either IOException (Exit.ExitCode, String, String)))
  case result of
    Left e -> do
      put st {lastExitCode = 127}
      pure ("aeth: " <> T.pack (show e))
    Right (ec, outS, errS) -> do
      setLastExit ec
      let outT = T.pack outS
          errT = T.pack errS
          extra =
            if ec == Exit.ExitSuccess
              then ""
              else "\n" <> "aeth: exit: " <> T.pack (show ec)
      pure (T.stripEnd (outT <> (if T.null errT then "" else if T.null outT then errT else "\n" <> errT) <> extra))

runStructured :: (MonadIO m) => Segment -> StateT ShellState m ()
runStructured seg = do
  (v, ec) <- structuredValueFor seg
  setLastExit ec
  liftIO (TIO.putStrLn (renderStructured v))

structuredValueFor :: (MonadIO m) => Segment -> StateT ShellState m (StructuredValue, Exit.ExitCode)
structuredValueFor seg = do
  st <- get
  let envMap = effectiveEnvMap st
  let args' = map (expandTextEnv envMap) (segArgs seg)
  let name = segName seg
  liftIO $
    case name of
      "ls" -> do
        let (opts, pathArgs) = parseLsArgs args'
        let targetDir = case pathArgs of
              (p : _) -> T.unpack p
              _ -> cwd st
        v <- lsStructured opts targetDir
        pure (v, Exit.ExitSuccess)
      "pwd" -> do
        v <- pwdStructured (cwd st)
        pure (v, Exit.ExitSuccess)
      "ps" -> do
        v <- psStructured
        pure (v, Exit.ExitSuccess)
      "df" -> do
        v <- dfStructured
        pure (v, Exit.ExitSuccess)
      "env" -> do
        v <- envStructured
        pure (v, Exit.ExitSuccess)
      "find" -> do
        let (basePath, patterns) = case args' of
              [] -> (cwd st, [])
              (p : rest) -> (T.unpack p, rest)
        v <- findStructured basePath patterns
        pure (v, Exit.ExitSuccess)
      _ -> do
        -- For now, treat unknown @commands as "capture external stdout".
        -- This enables: @cmd args | grep ...
        externalToStructured (cwd st) envMap (expandTextEnv envMap name) args'

externalToStructured :: FilePath -> Map.Map String String -> T.Text -> [T.Text] -> IO (StructuredValue, Exit.ExitCode)
externalToStructured workingDir envMap name args = do
  expandedArgs <- expandGlobs envMap workingDir args
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc (T.unpack name) (map T.unpack expandedArgs))
          { Proc.cwd = Just workingDir,
            Proc.env = Just envMerged
          }
  -- readCreateProcessWithExitCode captures stdout/stderr (good enough for now).
  result <- try (Proc.readCreateProcessWithExitCode cp "") :: IO (Either IOException (Exit.ExitCode, String, String))
  case result of
    Left e -> pure (SText ("aeth: " <> renderExecException name e), Exit.ExitFailure 127)
    Right (ec, out, err) ->
      case ec of
        Exit.ExitSuccess -> pure (SText (T.pack out), ec)
        _ ->
          pure (SText (T.pack out <> (if null err then "" else "\n" <> T.pack err) <> "\n" <> "exit: " <> T.pack (show ec)), ec)

hPutShellError :: T.Text -> IO ()
hPutShellError msg = TIO.hPutStrLn stderr ("aeth: " <> msg)

renderExecException :: T.Text -> IOException -> T.Text
renderExecException cmd e =
  let msg = T.pack (show e)
   in if isCommandNotFound msg
        then "command not found: " <> cmd
        else "exec: " <> msg

isCommandNotFound :: T.Text -> Bool
isCommandNotFound msg =
  -- Heuristic: different platforms/RTS versions format this differently.
  T.isInfixOf "does not exist" msg || T.isInfixOf "No such file or directory" msg

expandGlobs :: Map.Map String String -> FilePath -> [T.Text] -> IO [T.Text]
expandGlobs envMap workingDir args = do
  -- First, expand braces (e.g., {a,b,c} -> a b c, {1..5} -> 1 2 3 4 5)
  let braceExpanded = concatMap expandBraces args
  -- Then, expand shorthands and env vars
  let args1 = map (expandArgShorthands envMap) braceExpanded
  -- Finally, expand globs
  fmap concat (mapM (expandOne workingDir) args1)

-- | Expand brace patterns like {a,b,c} and {1..10}
-- Returns a list of expanded strings
expandBraces :: T.Text -> [T.Text]
expandBraces t =
  let s = T.unpack t
   in case parseBraceExpr s of
        Nothing -> [t] -- No brace pattern found
        Just results -> map T.pack results

-- | Parse and expand a brace expression
-- Returns Nothing if no valid brace pattern, Just results otherwise
parseBraceExpr :: String -> Maybe [String]
parseBraceExpr s =
  case findBracePattern s of
    Nothing -> Nothing
    Just (prefix, content, suffix) ->
      let expanded = expandBraceContent content
       in Just [prefix ++ e ++ suffix | e <- expanded]

-- | Find the first complete brace pattern {..}
-- Returns (prefix, content, suffix) if found
findBracePattern :: String -> Maybe (String, String, String)
findBracePattern s = go "" s (0 :: Int)
  where
    go :: String -> String -> Int -> Maybe (String, String, String)
    go _ [] _ = Nothing
    go prefix ('{' : rest) 0 =
      case findClosingBrace rest (1 :: Int) "" of
        Just (content, suffix) -> Just (reverse prefix, content, suffix)
        Nothing -> go ('{' : prefix) rest 0
    go prefix (c : rest) depth = go (c : prefix) rest depth

    findClosingBrace :: String -> Int -> String -> Maybe (String, String)
    findClosingBrace [] _ _ = Nothing
    findClosingBrace ('}' : rest) 1 acc = Just (reverse acc, rest)
    findClosingBrace ('{' : rest) n acc = findClosingBrace rest (n + 1) ('{' : acc)
    findClosingBrace ('}' : rest) n acc = findClosingBrace rest (n - 1) ('}' : acc)
    findClosingBrace (c : rest) n acc = findClosingBrace rest n (c : acc)

-- | Expand the content of a brace pattern
expandBraceContent :: String -> [String]
expandBraceContent content =
  case parseRange content of
    Just xs -> xs -- Numeric or char range like 1..10 or a..z
    Nothing -> splitBraceItems content -- Comma-separated items

-- | Parse a range expression like 1..10 or a..z
parseRange :: String -> Maybe [String]
parseRange s =
  case break (== '.') s of
    (start, '.' : '.' : end) ->
      case (reads start :: [(Int, String)], reads end :: [(Int, String)]) of
        ([(n1, "")], [(n2, "")]) ->
          if n1 <= n2
            then Just (map show [n1 .. n2])
            else Just (map show [n1, n1 - 1 .. n2])
        _ ->
          -- Try character range
          case (start, end) of
            ([c1], [c2])
              | c1 <= c2 -> Just (map (: []) [c1 .. c2])
              | otherwise -> Just (map (: []) [c1, pred c1 .. c2])
            _ -> Nothing
    _ -> Nothing

-- | Split brace content by commas, respecting nested braces
splitBraceItems :: String -> [String]
splitBraceItems = go "" (0 :: Int)
  where
    go :: String -> Int -> String -> [String]
    go acc _ [] = [reverse acc]
    go acc 0 (',' : rest) = reverse acc : go "" 0 rest
    go acc n ('{' : rest) = go ('{' : acc) (n + 1) rest
    go acc n ('}' : rest) = go ('}' : acc) (max 0 (n - 1)) rest
    go acc n (c : rest) = go (c : acc) n rest

expandArgShorthands :: Map.Map String String -> T.Text -> T.Text
expandArgShorthands envMap t =
  let p1 = expandTildeText envMap t
   in expandTextEnv envMap p1

expandOne :: FilePath -> T.Text -> IO [T.Text]
expandOne workingDir arg
  | not (hasGlobChars arg) && not (T.isInfixOf "**" arg) = pure [arg]
  | T.isInfixOf "**" arg = expandRecursiveGlob workingDir arg
  | otherwise = do
      let raw = T.unpack arg
      let (dirPart, pattern) = splitDirPattern raw
      if any isGlobChar dirPart
        then pure [arg]
        else do
          let listDirPath =
                if null dirPart || dirPart == "."
                  then workingDir
                  else
                    if FP.isAbsolute dirPart
                      then dirPart
                      else workingDir FP.</> dirPart
          exists <- Dir.doesDirectoryExist listDirPath
          if not exists
            then pure [arg]
            else do
              entries <- Dir.listDirectory listDirPath
              let isDot = case pattern of
                    ('.' : _) -> True
                    _ -> False
              let candidates =
                    filter
                      (\n -> (isDot || not (List.isPrefixOf "." n)) && matchGlob pattern n)
                      entries
              let sorted = List.sort candidates
              if null sorted
                then pure [arg]
                else
                  pure
                    (map (T.pack . (if null dirPart || dirPart == "." then id else (dirPart FP.</>))) sorted)

-- | Expand recursive glob patterns like **/*.txt
-- Supports:
--   **/*.txt    - all .txt files in any subdirectory
--   src/**/*.hs - all .hs files under src/
--   **          - all files recursively
expandRecursiveGlob :: FilePath -> T.Text -> IO [T.Text]
expandRecursiveGlob workingDir pattern = do
  let patStr = T.unpack pattern
      -- Split pattern into prefix (before **) and suffix (after **)
      (prefix, rest) = splitOnDoubleStar patStr
      -- Get the base directory to search
      baseDir =
        if null prefix || prefix == "."
          then workingDir
          else
            if FP.isAbsolute prefix
              then prefix
              else workingDir FP.</> prefix
      -- The pattern to match after **
      suffixPattern = case rest of
        '/' : xs -> xs
        xs -> xs
  exists <- Dir.doesDirectoryExist baseDir
  if not exists
    then pure [pattern] -- Return original if base doesn't exist
    else do
      -- Get all files recursively
      allFiles <- getRecursiveContents baseDir
      -- Filter by suffix pattern
      let matches = filter (matchRecursivePattern suffixPattern) allFiles
          -- Convert back to relative paths
          relPaths = map (makeRelativeTo baseDir) matches
          -- Add prefix back
          results =
            map
              ( \p ->
                  if null prefix || prefix == "."
                    then p
                    else prefix FP.</> p
              )
              relPaths
          sorted = List.sort results
      if null sorted
        then pure [pattern] -- No matches, return original pattern
        else pure (map T.pack sorted)

-- | Split a pattern on the first occurrence of **
splitOnDoubleStar :: String -> (String, String)
splitOnDoubleStar s = go "" s
  where
    go acc ('*' : '*' : rest) = (trimTrailingSlash (reverse acc), rest)
    go acc (c : rest) = go (c : acc) rest
    go acc [] = (reverse acc, "")

    trimTrailingSlash p = case reverse p of
      '/' : xs -> reverse xs
      _ -> p

-- | Get all files and directories recursively
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents dir = do
  entries <- Dir.listDirectory dir
  -- Filter out hidden files/directories (those starting with .)
  let visibleEntries = filter (not . List.isPrefixOf ".") entries
  paths <- forM visibleEntries $ \entry -> do
    let fullPath = dir FP.</> entry
    isDir <- Dir.doesDirectoryExist fullPath
    if isDir
      then do
        subPaths <- getRecursiveContents fullPath
        -- Include both the directory and its contents
        pure (fullPath : subPaths)
      else pure [fullPath]
  pure (concat paths)
  where
    forM = flip mapM

-- | Make a path relative to a base directory
makeRelativeTo :: FilePath -> FilePath -> FilePath
makeRelativeTo base path =
  let baseLen = length base
      path' =
        if List.isPrefixOf base path
          then drop (baseLen + 1) path -- +1 for the /
          else path
   in if null path' then "." else path'

-- | Match a path against a recursive pattern (the part after **)
-- The pattern can contain additional globs
matchRecursivePattern :: String -> FilePath -> Bool
matchRecursivePattern "" _ = True
-- \** matches everything
matchRecursivePattern pat path =
  let filename = FP.takeFileName path
   in -- For patterns like *.txt, match against filename
      -- For patterns like foo/*.txt, match the whole relative path
      if '/' `elem` pat
        then matchGlobPath pat path
        else matchGlob pat filename

-- | Match a glob pattern that may contain path separators
matchGlobPath :: String -> FilePath -> Bool
matchGlobPath pat path = go (splitPath pat) (splitPath path)
  where
    splitPath = FP.splitDirectories

    go [] [] = True
    go [] _ = False
    go ("**" : ps) s = any (go ps) (tails s)
    go (p : ps) (s : ss) = matchGlob p s && go ps ss
    go _ [] = False

    tails [] = [[]]
    tails xs@(_ : xs') = xs : tails xs'

splitDirPattern :: FilePath -> (FilePath, String)
splitDirPattern p =
  let dirPart = FP.takeDirectory p
      basePart = FP.takeFileName p
   in if dirPart == "." then ("", basePart) else (dirPart, basePart)

hasGlobChars :: T.Text -> Bool
hasGlobChars = T.any isGlobChar

isGlobChar :: Char -> Bool
isGlobChar c = c == '*' || c == '?' || c == '['

matchGlob :: String -> String -> Bool
matchGlob pat str = go pat str
  where
    go [] [] = True
    go [] _ = False
    go ('*' : ps) s = any (go ps) (suffixes s)
    go ('?' : ps) (_ : ss) = go ps ss
    go ('?' : _) [] = False
    go ('[' : ps) s0 =
      case s0 of
        [] -> False
        (c : cs) ->
          case parseCharClass ps of
            Nothing -> go ('[' : ps) s0
            Just (neg, allowed, rest) ->
              let ok = (c `elem` allowed) /= neg
               in ok && go rest cs
    go (p : ps) (c : cs) = p == c && go ps cs
    go _ [] = False

    suffixes :: String -> [String]
    suffixes s =
      s : case s of
        [] -> []
        (_ : xs) -> suffixes xs

parseCharClass :: String -> Maybe (Bool, [Char], String)
parseCharClass s =
  let (neg, s1) =
        case s of
          ('!' : xs) -> (True, xs)
          _ -> (False, s)
      (body, rest0) = break (== ']') s1
   in case rest0 of
        [] -> Nothing
        (_ : rest) -> Just (neg, expandRanges body, rest)

expandRanges :: String -> [Char]
expandRanges = go
  where
    go [] = []
    go (a : '-' : b : xs) =
      if a <= b
        then [a .. b] ++ go xs
        else [b .. a] ++ go xs
    go (x : xs) = x : go xs

resolveCdTarget :: Map.Map String String -> Maybe FilePath -> IO FilePath
resolveCdTarget envMap mTarget =
  case mTarget of
    Just p -> do
      let p1 = T.unpack (expandTildeText envMap (T.pack p))
      let p2 = T.unpack (expandTextEnv envMap (T.pack p1))
      Dir.makeAbsolute p2
    Nothing -> do
      Dir.makeAbsolute (Map.findWithDefault "." "HOME" envMap)

-- | Merge base env with overrides (overrides win), excluding deleted vars.
effectiveEnvMap :: ShellState -> Map.Map String String
effectiveEnvMap st =
  let merged = Map.union (envOverrides st) (baseEnv st)
   in foldr Map.delete merged (Set.toList (deletedEnv st))

-- | Tilde expansion for arguments (only leading ~ forms).
expandTildeText :: Map.Map String String -> T.Text -> T.Text
expandTildeText envMap t =
  case T.unpack t of
    "~" -> T.pack (Map.findWithDefault "~" "HOME" envMap)
    '~' : '/' : rest ->
      case Map.lookup "HOME" envMap of
        Nothing -> t
        Just h -> T.pack (h FP.</> rest)
    _ -> t

-- | Expand $VAR and ${VAR} anywhere in the text.
--
-- This is intentionally minimal (no quoting/escape rules yet).
expandTextEnv :: Map.Map String String -> T.Text -> T.Text
expandTextEnv envMap = go
  where
    go s =
      case T.breakOn "$" s of
        (before, rest)
          | T.null rest -> before
          | otherwise ->
              let afterDollar = T.drop 1 rest
               in before <> expandVar afterDollar

    expandVar s
      | T.null s = "$"
      | T.isPrefixOf "{" s =
          case T.breakOn "}" (T.drop 1 s) of
            (var, tail0)
              | T.null tail0 -> "${" <> var
              | otherwise ->
                  let value = Map.findWithDefault ("${" <> T.unpack var <> "}") (T.unpack var) envMap
                      rest = T.drop 1 tail0
                   in T.pack value <> go rest
      | otherwise =
          let (var, rest) = T.span isVarChar s
           in if T.null var
                then "$" <> go s
                else
                  let value = Map.findWithDefault ("$" <> T.unpack var) (T.unpack var) envMap
                   in T.pack value <> go rest

    isVarChar c =
      (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c == '_'

runExport :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runExport args = do
  st <- get
  let envMap = effectiveEnvMap st
  case args of
    [] -> liftIO $ mapM_ putKV (Map.toList envMap)
    _ -> do
      newOverrides <- foldl (step envMap) (pure (envOverrides st)) args
      put st {envOverrides = newOverrides, lastExitCode = 0}
  where
    putKV (k, v) = TIO.hPutStrLn stderr (T.pack (k <> "=" <> v))

    step :: (MonadIO m) => Map.Map String String -> m (Map.Map String String) -> T.Text -> m (Map.Map String String)
    step envMap accM a = do
      acc <- accM
      let t = expandTextEnv envMap a
      case T.breakOn "=" t of
        (k, vEq)
          | T.null vEq ->
              -- export KEY: keep existing value if present, else set empty
              let key = T.unpack (T.strip k)
                  val = Map.findWithDefault "" key envMap
               in pure (Map.insert key val acc)
          | otherwise ->
              let key = T.unpack (T.strip k)
                  val = T.unpack (T.drop 1 vEq)
               in pure (Map.insert key val acc)

-- | unset - remove environment variables (adds to tombstone set so inherited vars are also unset)
runUnset :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runUnset args = do
  st <- get
  let keys = map T.unpack args
      newOverrides = foldr Map.delete (envOverrides st) keys
      newDeleted = foldr Set.insert (deletedEnv st) keys
  put st {envOverrides = newOverrides, deletedEnv = newDeleted, lastExitCode = 0}

-- | pwd - print working directory
runPwd :: (MonadIO m) => StateT ShellState m ()
runPwd = do
  st <- get
  liftIO $ putStrLn (cwd st)
  setLastExit Exit.ExitSuccess

-- | history - show command history
runHistory :: (MonadIO m) => StateT ShellState m ()
runHistory = do
  st <- get
  let hist = history st
  liftIO $ mapM_ (\(i, cmd) -> putStrLn $ show i ++ "  " ++ cmd) (zip ([1 ..] :: [Int]) hist)
  setLastExit Exit.ExitSuccess

-- | source - execute commands from a file
-- Executes each line in the current shell context so exports/cd persist.
-- Note: Aliases are not expanded in sourced files.
runSource :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runSource args = do
  case args of
    [] -> do
      liftIO $ hPutShellError "source: filename argument required"
      setLastExit (Exit.ExitFailure 1)
    (path : _) -> do
      st <- get
      let envMap = effectiveEnvMap st
      let expandedPath = T.unpack (expandTextEnv envMap path)
      exists <- liftIO $ Dir.doesFileExist expandedPath
      if not exists
        then do
          liftIO $ hPutShellError ("source: " <> T.pack expandedPath <> ": No such file")
          setLastExit (Exit.ExitFailure 1)
        else do
          contentResult <- liftIO $ try (TIO.readFile expandedPath)
          case contentResult of
            Left (e :: IOException) -> do
              liftIO $ hPutShellError ("source: " <> T.pack (show e))
              setLastExit (Exit.ExitFailure 1)
            Right content -> do
              let lns = filter (not . T.null) (map stripComments (T.lines content))
              -- Execute each line in the current shell context
              mapM_ executeSourceLine lns
  where
    stripComments t = T.strip (T.takeWhile (/= '#') t)

    executeSourceLine lineText = do
      case parsePipeline lineText of
        Left err -> do
          when (err /= "empty") $ do
            liftIO $ hPutShellError ("source: parse error: " <> T.pack err)
            setLastExit (Exit.ExitFailure 2)
        Right pipeline -> runPipeline Map.empty pipeline

-- | type - describe a command
runType :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runType args = do
  st <- get
  let envMap = effectiveEnvMap st
  -- Use the shared shellBuiltins constant
  results <- mapM (checkCmd envMap) args
  -- Set exit status based on whether all commands were found
  if and results
    then setLastExit Exit.ExitSuccess
    else setLastExit (Exit.ExitFailure 1)
  where
    checkCmd envMap cmd = do
      if cmd `elem` shellBuiltins
        then do
          liftIO $ TIO.putStrLn (cmd <> " is a shell builtin")
          pure True
        else do
          mPath <- liftIO $ findExecutable envMap (T.unpack cmd)
          case mPath of
            Just p -> do
              liftIO $ TIO.putStrLn (cmd <> " is " <> T.pack p)
              pure True
            Nothing -> do
              liftIO $ TIO.putStrLn (cmd <> " not found")
              pure False

-- | which - find executable
runWhich :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runWhich args = do
  st <- get
  let envMap = effectiveEnvMap st
  allFound <- mapM (checkCmd envMap) args
  setLastExit $ if and allFound then Exit.ExitSuccess else Exit.ExitFailure 1
  where
    checkCmd envMap cmd = do
      mPath <- liftIO $ findExecutable envMap (T.unpack cmd)
      case mPath of
        Just p -> do
          liftIO $ putStrLn p
          pure True
        Nothing -> pure False

-- | echo - print arguments
_runEcho :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
_runEcho args = runEchoWithRedirects args []

-- | echo with native I/O redirection support
runEchoWithRedirects :: (MonadIO m) => [T.Text] -> [Redirection] -> StateT ShellState m ()
runEchoWithRedirects args redirs = do
  st <- get
  let envMap = effectiveEnvMap st
  -- Apply brace expansion and globs to echo arguments too
  expanded <- liftIO $ expandGlobs envMap (cwd st) args
  let output = T.unwords expanded

  result <- liftIO $ do
    -- Find output redirection
    let stdoutRedir = List.find (\r -> redirType r `elem` [RedirectOut, RedirectAppend]) redirs
    case stdoutRedir of
      Nothing -> do
        TIO.putStrLn output
        return (Right ())
      Just r -> do
        let path = expandPath (cwd st) envMap (redirTarget r)
        writeResult <- try $ case redirType r of
          RedirectOut -> TIO.writeFile (T.unpack path) (output <> "\n")
          RedirectAppend -> TIO.appendFile (T.unpack path) (output <> "\n")
          _ -> TIO.putStrLn output
        case writeResult of
          Left (e :: IOException) -> do
            hPutShellError ("echo: " <> T.pack (show e))
            return (Left e)
          Right () -> return (Right ())

  case result of
    Left _ -> setLastExit (Exit.ExitFailure 1)
    Right () -> setLastExit Exit.ExitSuccess

-- | jobs - list background jobs
runJobs :: (MonadIO m) => StateT ShellState m ()
runJobs = do
  st <- get
  let jobs = backgroundJobs st
  if null jobs
    then setLastExit Exit.ExitSuccess
    else do
      liftIO $ mapM_ printJob jobs
      setLastExit Exit.ExitSuccess
  where
    printJob job =
      TIO.putStrLn $
        "["
          <> T.pack (show (jobId job))
          <> "] "
          <> T.pack (show (jobStatus job))
          <> " "
          <> jobCommand job

-- | fg - bring a job to foreground
runFg :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runFg args = do
  st <- get
  let jobs = backgroundJobs st
  case args of
    [] ->
      -- Bring most recent job to foreground
      case jobs of
        [] -> do
          liftIO $ hPutShellError "fg: no current job"
          setLastExit (Exit.ExitFailure 1)
        (job : _) -> bringToForeground job
    (arg : _) ->
      -- Parse job ID (e.g., %1 or just 1)
      let jobIdStr = T.unpack $ T.dropWhile (== '%') arg
       in case reads jobIdStr :: [(Int, String)] of
            [(jid, "")] ->
              case List.find (\j -> jobId j == jid) jobs of
                Nothing -> do
                  liftIO $ hPutShellError ("fg: %" <> T.pack (show jid) <> ": no such job")
                  setLastExit (Exit.ExitFailure 1)
                Just job -> bringToForeground job
            _ -> do
              liftIO $ hPutShellError ("fg: " <> arg <> ": invalid job spec")
              setLastExit (Exit.ExitFailure 1)
  where
    bringToForeground job = do
      liftIO $ TIO.putStrLn $ jobCommand job
      -- Send SIGCONT to the process to resume it
      _ <- liftIO $ signalProcess sigCONT (jobPid job)
      -- Wait for the process to complete
      _ <- liftIO $ getProcessStatus True False (jobPid job)
      -- Remove job from background jobs list
      st <- get
      put st {backgroundJobs = filter (\j -> jobId j /= jobId job) (backgroundJobs st)}
      setLastExit Exit.ExitSuccess

-- | bg - continue a stopped job in background
runBg :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runBg args = do
  st <- get
  let jobs = backgroundJobs st
  case args of
    [] ->
      -- Resume most recent stopped job
      case List.find (\j -> jobStatus j == Stopped) jobs of
        Nothing -> do
          liftIO $ hPutShellError "bg: no current job"
          setLastExit (Exit.ExitFailure 1)
        Just job -> continueInBackground job
    (arg : _) ->
      let jobIdStr = T.unpack $ T.dropWhile (== '%') arg
       in case reads jobIdStr :: [(Int, String)] of
            [(jid, "")] ->
              case List.find (\j -> jobId j == jid) jobs of
                Nothing -> do
                  liftIO $ hPutShellError ("bg: %" <> T.pack (show jid) <> ": no such job")
                  setLastExit (Exit.ExitFailure 1)
                Just job -> continueInBackground job
            _ -> do
              liftIO $ hPutShellError ("bg: " <> arg <> ": invalid job spec")
              setLastExit (Exit.ExitFailure 1)
  where
    continueInBackground job = do
      liftIO $ TIO.putStrLn $ "[" <> T.pack (show (jobId job)) <> "] " <> jobCommand job <> " &"
      -- Send SIGCONT to resume the process
      _ <- liftIO $ signalProcess sigCONT (jobPid job)
      -- Update job status to Running
      st <- get
      let updatedJobs = map (\j -> if jobId j == jobId job then j {jobStatus = Running} else j) (backgroundJobs st)
      put st {backgroundJobs = updatedJobs}
      setLastExit Exit.ExitSuccess

-- | Find executable in PATH, or test directly if name contains '/'
findExecutable :: Map.Map String String -> String -> IO (Maybe FilePath)
findExecutable envMap name
  -- If name contains '/', test it directly instead of searching PATH
  | elem '/' name = do
      exists <- Dir.doesFileExist name
      isExe <- if exists then Dir.executable <$> Dir.getPermissions name else pure False
      pure (if exists && isExe then Just name else Nothing)
  | otherwise = do
      let pathEnv = Map.findWithDefault "" "PATH" envMap
          pathDirs = FP.splitSearchPath pathEnv
      findFirst pathDirs
  where
    findFirst [] = pure Nothing
    findFirst (d : ds) = do
      let fullPath = d FP.</> name
      exists <- Dir.doesFileExist fullPath
      isExe <- if exists then Dir.executable <$> Dir.getPermissions fullPath else pure False
      if exists && isExe
        then pure (Just fullPath)
        else findFirst ds
