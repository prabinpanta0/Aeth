{-# LANGUAGE OverloadedStrings #-}

-- | Configuration system for Aeth shell.
--
-- This is a rewrite that removes the `hint` dependency for fast startup.
-- Configuration is done entirely via simple line-based parsing for robustness.
module Aeth.ConfigFast
  ( ShellConfig (..),
    UiMode (..),
    PromptStyle (..),
    defaultConfig,
    loadConfig,
    configDir,
    configTomlPath,
    rcFilePath,
    historyFilePath,
    readHistory,
    appendHistory,
    mkPromptFunction,
  )
where

import Aeth.Types (ShellState (..))
import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Char (toLower)
import Data.List (stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Console.ANSI as ANSI
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import qualified System.Process as Proc

-- | Shell configuration (no more hint/GHC interpreter!)
data ShellConfig = ShellConfig
  { uiMode :: UiMode,
    promptStyle :: PromptStyle,
    showGitBranch :: Bool,
    showExitCode :: Bool,
    showDuration :: Bool,
    promptColors :: PromptColors,
    aliases :: Map.Map T.Text T.Text,
    customPrompt :: Maybe T.Text, -- Custom prompt format string
    safeMode :: Bool, -- Restrict dangerous commands
    historySize :: Int, -- Max history entries
    syntaxHighlighting :: Bool, -- Enable syntax highlighting
    autoSuggestions :: Bool, -- Enable fish-like auto-suggestions
    sourceFiles :: [T.Text], -- Files to source at startup
    binaryPaths :: [T.Text] -- Additional paths to add to PATH
  }
  deriving (Eq, Show)

data UiMode = NormalUi | TuiUi
  deriving (Eq, Show)

-- | Prompt style presets
data PromptStyle
  = MinimalPrompt -- cwd >
  | PowerlinePrompt -- Fancy with colors
  | SimplePrompt -- user@host:cwd$
  | CustomPrompt -- User-defined format
  deriving (Eq, Show)

data PromptColors = PromptColors
  { cwdColor :: String,
    gitColor :: String,
    errorColor :: String,
    successColor :: String
  }
  deriving (Eq, Show)

defaultPromptColors :: PromptColors
defaultPromptColors =
  PromptColors
    { cwdColor = "cyan",
      gitColor = "magenta",
      errorColor = "red",
      successColor = "green"
    }

-- | Default aliases for better user experience
defaultAliases :: Map.Map T.Text T.Text
defaultAliases =
  Map.fromList
    [ ("ls", "ls --color=auto"),
      ("grep", "grep --color=auto"),
      ("fgrep", "fgrep --color=auto"),
      ("egrep", "egrep --color=auto"),
      ("diff", "diff --color=auto"),
      ("ll", "ls -la --color=auto"),
      ("la", "@ls -a")
    ]

defaultConfig :: ShellConfig
defaultConfig =
  ShellConfig
    { uiMode = NormalUi,
      promptStyle = MinimalPrompt,
      showGitBranch = True,
      showExitCode = True,
      showDuration = False,
      promptColors = defaultPromptColors,
      aliases = defaultAliases,
      customPrompt = Nothing,
      safeMode = False,
      historySize = 10000,
      syntaxHighlighting = True,
      autoSuggestions = False,
      sourceFiles = [],
      binaryPaths = []
    }

-- | Load configuration - simple and fast!
loadConfig :: IO (ShellConfig, Maybe String)
loadConfig = do
  path <- configTomlPath
  exists <- Dir.doesFileExist path
  if not exists
    then do
      -- Check for old config location and migrate if found
      migrateResult <- migrateOldConfig path
      case migrateResult of
        Just cfg -> pure (cfg, Just "Migrated config from ~/.config/Aeth to ~/.config/aeth")
        Nothing -> do
          -- Create default config file if it doesn't exist
          createDefaultConfig path
          pure (defaultConfig, Nothing)
    else do
      -- Parse config from file
      content <- TIO.readFile path
      let cfg = parseConfigLines (T.lines content)
      pure (cfg, Nothing)

-- | Migrate config from old ~/.config/Aeth to new ~/.config/aeth
migrateOldConfig :: FilePath -> IO (Maybe ShellConfig)
migrateOldConfig newPath = do
  mHome <- Env.lookupEnv "HOME"
  case mHome of
    Nothing -> pure Nothing
    Just home -> do
      let oldPath = home FP.</> ".config" FP.</> "Aeth" FP.</> "config.toml"
      oldExists <- Dir.doesFileExist oldPath
      if not oldExists
        then pure Nothing
        else do
          -- Copy old config to new location
          dir <- configDir
          Dir.createDirectoryIfMissing True dir
          content <- TIO.readFile oldPath
          TIO.writeFile newPath content
          -- Also copy history file if it exists
          let oldHistPath = home FP.</> ".config" FP.</> "Aeth" FP.</> "history"
          oldHistExists <- Dir.doesFileExist oldHistPath
          when oldHistExists $ do
            histPath <- historyFilePath
            TIO.readFile oldHistPath >>= TIO.writeFile histPath
          -- Parse the migrated config
          pure (Just (parseConfigLines (T.lines content)))

-- | Parse config from lines (simple key=value format)
parseConfigLines :: [T.Text] -> ShellConfig
parseConfigLines ls = foldr applyLine defaultConfig ls
  where
    applyLine line cfg =
      let stripped = T.strip line
       in if T.null stripped || T.isPrefixOf "#" stripped
            then cfg
            -- Handle alias definitions: alias.name = "value"
            else
              if T.isPrefixOf "alias." stripped
                then case T.breakOn "=" stripped of
                  (key, val)
                    | T.null val -> cfg
                    | otherwise ->
                        let aliasName = T.drop 6 (T.strip key) -- Remove "alias."
                            aliasVal = unquote (T.strip (T.drop 1 val))
                         in cfg {aliases = Map.insert aliasName aliasVal (aliases cfg)}
                -- Handle source.N = "/path/to/file"
                else
                  if T.isPrefixOf "source." stripped
                    then case T.breakOn "=" stripped of
                      (_, val)
                        | T.null val -> cfg
                        | otherwise ->
                            let sourceVal = unquote (T.strip (T.drop 1 val))
                             in cfg {sourceFiles = sourceVal : sourceFiles cfg}
                    -- Handle path.N = "/custom/bin"
                    else
                      if T.isPrefixOf "path." stripped
                        then case T.breakOn "=" stripped of
                          (_, val)
                            | T.null val -> cfg
                            | otherwise ->
                                let pathVal = unquote (T.strip (T.drop 1 val))
                                 in cfg {binaryPaths = pathVal : binaryPaths cfg}
                        else case T.breakOn "=" stripped of
                          (key, val)
                            | T.null val -> cfg
                            | otherwise ->
                                let k = T.strip key
                                    v = T.strip (T.drop 1 val) -- drop the '='
                                    vUnquoted = unquote v
                                 in applySetting k vUnquoted cfg

    unquote t =
      let s = T.unpack t
       in case s of
            ('"' : rest) | not (null rest) && last rest == '"' -> T.pack (init rest)
            ('\'' : rest) | not (null rest) && last rest == '\'' -> T.pack (init rest)
            _ -> t

    applySetting key val cfg =
      case T.toLower key of
        "ui_mode" -> cfg {uiMode = parseUiMode val}
        "prompt_style" -> cfg {promptStyle = parsePromptStyle val}
        "show_git_branch" -> cfg {showGitBranch = parseBool val}
        "show_exit_code" -> cfg {showExitCode = parseBool val}
        "show_duration" -> cfg {showDuration = parseBool val}
        "cwd_color" -> cfg {promptColors = (promptColors cfg) {cwdColor = T.unpack val}}
        "git_color" -> cfg {promptColors = (promptColors cfg) {gitColor = T.unpack val}}
        "error_color" -> cfg {promptColors = (promptColors cfg) {errorColor = T.unpack val}}
        "success_color" -> cfg {promptColors = (promptColors cfg) {successColor = T.unpack val}}
        "prompt" -> cfg {customPrompt = Just val, promptStyle = CustomPrompt}
        "safe_mode" -> cfg {safeMode = parseBool val}
        "history_size" -> cfg {historySize = parseIntDefault 10000 val}
        "syntax_highlighting" -> cfg {syntaxHighlighting = parseBool val}
        "auto_suggestions" -> cfg {autoSuggestions = parseBool val}
        -- Ignore unknown/legacy keys like use_dynamic_prompt
        _ -> cfg

    parseUiMode t =
      case T.toLower t of
        "tui" -> TuiUi
        _ -> NormalUi

    parsePromptStyle t =
      case T.toLower t of
        "powerline" -> PowerlinePrompt
        "simple" -> SimplePrompt
        "custom" -> CustomPrompt
        _ -> MinimalPrompt

    parseBool t =
      case T.toLower t of
        "true" -> True
        "yes" -> True
        "1" -> True
        _ -> False

    -- \| Parse an integer with a default; clamps to non-negative so negative inputs are treated as zero.
    parseIntDefault def t =
      case reads (T.unpack t) :: [(Int, String)] of
        [(n, "")] -> max 0 n
        _ -> def

-- | Create a default config file
createDefaultConfig :: FilePath -> IO ()
createDefaultConfig path = do
  dir <- configDir
  Dir.createDirectoryIfMissing True dir
  TIO.writeFile path defaultConfigContent

defaultConfigContent :: T.Text
defaultConfigContent =
  T.unlines
    [ "# aeth Shell Configuration",
      "# See docs/CONFIGURATION.md for full documentation",
      "",
      "# UI Mode: 'normal' or 'tui' (fullscreen)",
      "ui_mode = \"normal\"",
      "",
      "# Prompt Style: 'minimal', 'powerline', 'simple', or 'custom'",
      "prompt_style = \"minimal\"",
      "",
      "# Custom prompt format (only used when prompt_style = 'custom')",
      "# Available placeholders: {cwd}, {user}, {host}, {branch}, {exit}, {git}",
      "# prompt = \"{user}@{host}:{cwd} {git}$ \"",
      "",
      "# Prompt features",
      "show_git_branch = true",
      "show_exit_code = true",
      "show_duration = false",
      "",
      "# Colors: black, red, green, yellow, blue, magenta, cyan, white, gray",
      "cwd_color = \"cyan\"",
      "git_color = \"magenta\"",
      "error_color = \"red\"",
      "success_color = \"green\"",
      "",
      "# Features",
      "syntax_highlighting = true",
      "auto_suggestions = false",
      "",
      "# Security",
      "safe_mode = false",
      "",
      "# History",
      "history_size = 10000",
      "",
      "# Aliases - add your own aliases here",
      "# alias.ls = \"ls --color=auto\"",
      "# alias.ll = \"ls -la --color=auto\"",
      "# alias.la = \"@ls -a\"",
      "# alias.grep = \"grep --color=auto\"",
      "",
      "# Source files at startup (like source ~/.bashrc)",
      "# source.1 = \"~/.bashrc\"",
      "# source.2 = \"~/.bash_aliases\"",
      "",
      "# Additional paths to add to PATH",
      "# path.1 = \"~/.local/bin\"",
      "# path.2 = \"/opt/custom/bin\""
    ]

-- | Build the prompt function from config
mkPromptFunction :: ShellConfig -> ShellState -> IO String
mkPromptFunction cfg st = do
  prettyCwd <- shortenHomePath (cwd st)
  branch <-
    if showGitBranch cfg
      then gitBranchIn (cwd st)
      else pure Nothing

  let colors = promptColors cfg
      exitOk = lastExitCode st == 0

  case promptStyle cfg of
    MinimalPrompt -> do
      let cwdPart = colorize (cwdColor colors) prettyCwd
          gitPart = maybe "" (\b -> " " ++ colorize (gitColor colors) ("(" ++ b ++ ")")) branch
          exitIndicator
            | not (showExitCode cfg) = ""
            | exitOk = colorize (successColor colors) ">"
            | otherwise = colorize (errorColor colors) ("!" ++ show (lastExitCode st) ++ ">")
      pure $ cwdPart ++ gitPart ++ " " ++ (if null exitIndicator then "> " else exitIndicator ++ " ")
    PowerlinePrompt -> do
      let cwdSeg = colorize (cwdColor colors) (" " ++ prettyCwd ++ " ")
          gitSeg = maybe "" (\b -> colorize (gitColor colors) ("  " ++ b ++ " ")) branch
          statusSeg
            | not (showExitCode cfg) = ""
            | exitOk = colorize (successColor colors) " ok "
            | otherwise = colorize (errorColor colors) (" !" ++ show (lastExitCode st) ++ " ")
          durationSeg = case (showDuration cfg, lastDurationMs st) of
            (True, Just ms) | ms > 100 -> colorize "gray" (" " ++ show ms ++ "ms ")
            _ -> ""
      pure $ cwdSeg ++ gitSeg ++ statusSeg ++ durationSeg ++ " "
    SimplePrompt -> do
      user <- Env.lookupEnv "USER" >>= pure . fromMaybe "user"
      host <- getHostname
      let prompt = user ++ "@" ++ host ++ ":" ++ prettyCwd ++ "$ "
      pure prompt
    CustomPrompt -> do
      -- Custom prompt with format string substitution
      user <- Env.lookupEnv "USER" >>= pure . fromMaybe "user"
      host <- getHostname
      let branchStr = fromMaybe "" branch
          exitStr = if exitOk then "0" else show (lastExitCode st)
          template = T.unpack (fromMaybe "{cwd} > " (customPrompt cfg))
          -- Substitute placeholders
          result =
            substitute
              template
              [ ("{cwd}", prettyCwd),
                ("{user}", user),
                ("{host}", host),
                ("{branch}", branchStr),
                ("{exit}", exitStr),
                ("{git}", if null branchStr then "" else "(" ++ branchStr ++ ")")
              ]
      pure result
  where
    substitute :: String -> [(String, String)] -> String
    substitute s [] = s
    substitute s ((pat, repl) : rest) = substitute (replaceAll pat repl s) rest

    replaceAll :: String -> String -> String -> String
    replaceAll pat repl s =
      case stripPrefix pat s of
        Just rest -> repl ++ replaceAll pat repl rest
        Nothing -> case s of
          [] -> []
          (c : cs) -> c : replaceAll pat repl cs

-- | Get hostname
getHostname :: IO String
getHostname = do
  mHost <- Env.lookupEnv "HOSTNAME"
  case mHost of
    Just h -> pure h
    Nothing -> do
      r <- try (Proc.readCreateProcess (Proc.proc "hostname" []) "") :: IO (Either IOException String)
      case r of
        Left _ -> pure "localhost"
        Right out -> pure $ takeWhile (/= '\n') out

-- | Apply ANSI color to text
colorize :: String -> String -> String
colorize colorName txt =
  case parseColor colorName of
    Nothing -> txt
    Just sgrs -> ANSI.setSGRCode sgrs ++ txt ++ ANSI.setSGRCode [ANSI.Reset]

parseColor :: String -> Maybe [ANSI.SGR]
parseColor s =
  case map toLower s of
    "black" -> Just [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
    "red" -> Just [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
    "green" -> Just [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
    "yellow" -> Just [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
    "blue" -> Just [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
    "magenta" -> Just [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Magenta]
    "cyan" -> Just [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan]
    "white" -> Just [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
    "gray" -> Just [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    "grey" -> Just [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    _ -> Nothing

-- | Paths
configDir :: IO FilePath
configDir = do
  mXdg <- Env.lookupEnv "XDG_CONFIG_HOME"
  case mXdg of
    Just xdg | not (null xdg) -> pure (xdg FP.</> "aeth")
    _ -> do
      mHome <- Env.lookupEnv "HOME"
      let home = fromMaybe "." mHome
      pure (home FP.</> ".config" FP.</> "aeth")

configTomlPath :: IO FilePath
configTomlPath = (FP.</> "config.toml") <$> configDir

rcFilePath :: IO FilePath
rcFilePath = (FP.</> "rc") <$> configDir

historyFilePath :: IO FilePath
historyFilePath = (FP.</> "history") <$> configDir

-- | History management
readHistory :: IO [String]
readHistory = do
  path <- historyFilePath
  exists <- Dir.doesFileExist path
  if not exists
    then pure []
    else do
      content <- TIO.readFile path
      pure $ map T.unpack $ T.lines content

appendHistory :: String -> IO ()
appendHistory line = do
  when (not (null line) && not (all (== ' ') line)) $ do
    path <- historyFilePath
    dir <- configDir
    Dir.createDirectoryIfMissing True dir
    TIO.appendFile path (T.pack line <> "\n")

-- | Utilities
shortenHomePath :: FilePath -> IO FilePath
shortenHomePath p = do
  mh <- Env.lookupEnv "HOME"
  case mh of
    Nothing -> pure p
    Just h ->
      case stripPrefix (ensureTrailingSlash h) p of
        Just rest -> pure ("~/" <> rest)
        Nothing -> if p == h then pure "~" else pure p
  where
    ensureTrailingSlash s
      | null s = s
      | last s == '/' = s
      | otherwise = s <> "/"

gitBranchIn :: FilePath -> IO (Maybe String)
gitBranchIn dir = do
  isDir <- Dir.doesDirectoryExist dir
  if not isDir
    then pure Nothing
    else do
      r <-
        try
          ( Proc.readCreateProcessWithExitCode
              ((Proc.proc "git" ["-C", dir, "rev-parse", "--abbrev-ref", "HEAD"]) {Proc.std_err = Proc.NoStream})
              ""
          ) ::
          IO (Either IOException (Exit.ExitCode, String, String))
      case r of
        Left _ -> pure Nothing
        Right (ec, out, _err) ->
          if ec /= Exit.ExitSuccess
            then pure Nothing
            else do
              let b = trim (takeWhile (/= '\n') out)
              if null b || b == "HEAD" then pure Nothing else pure (Just b)
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
