{-# LANGUAGE OverloadedStrings #-}

module Aeth.Parse
  ( parsePipeline,
    renderParseError,
  )
where

-- \| Improved parser with proper quoting support:
-- - Supports double quotes: "hello world" -> single argument
-- - Supports single quotes: 'hello world' -> single argument (no var expansion)
-- - Supports escape: hello\ world -> single argument
-- - Splits by '|' for pipes
-- - '@cmd' means Structured, otherwise RawString

import Aeth.Types
import Data.Char (isSpace)
import qualified Data.Text as T

parsePipeline :: T.Text -> Either String Pipeline
parsePipeline input0 =
  let input = T.dropWhile isSpace input0
   in if T.null input
        then Left "empty"
        else Right (Pipeline (map parseSegment (splitPipes input)))

-- | Split by pipes, but not inside quotes
splitPipes :: T.Text -> [T.Text]
splitPipes t
  -- If the user is using shell boolean/pipe operators, don't attempt to
  -- interpret pipes at all. We'll delegate to /bin/sh -c.
  | T.isInfixOf "||" t = [T.strip t]
  | T.isInfixOf "|&" t = [T.strip t]
  | otherwise = splitByPipe t

-- | Split text by '|' while respecting quotes
splitByPipe :: T.Text -> [T.Text]
splitByPipe input = go (T.unpack input) "" [] Nothing
  where
    go :: String -> String -> [T.Text] -> Maybe Char -> [T.Text]
    go [] acc segments _ = segments ++ [T.strip (T.pack (reverse acc))]
    go ('\\' : c : rest) acc segments inQuote =
      go rest (c : '\\' : acc) segments inQuote
    go ('"' : rest) acc segments Nothing =
      go rest ('"' : acc) segments (Just '"')
    go ('"' : rest) acc segments (Just '"') =
      go rest ('"' : acc) segments Nothing
    go ('\'' : rest) acc segments Nothing =
      go rest ('\'' : acc) segments (Just '\'')
    go ('\'' : rest) acc segments (Just '\'') =
      go rest ('\'' : acc) segments Nothing
    go ('|' : rest) acc segments Nothing =
      go rest "" (segments ++ [T.strip (T.pack (reverse acc))]) Nothing
    go (c : rest) acc segments inQuote =
      go rest (c : acc) segments inQuote

-- | Parse a single segment (command + args)
parseSegment :: T.Text -> Segment
parseSegment segText =
  let toks = tokenize (T.unpack segText)
   in case toks of
        [] -> Segment RawString "" []
        (t : rest) ->
          case T.uncons (T.pack t) of
            Just ('@', name) -> Segment Structured name (map T.pack rest)
            _ -> Segment RawString (T.pack t) (map T.pack rest)

-- | Tokenize a string with proper quoting support
tokenize :: String -> [String]
tokenize = go "" [] Nothing
  where
    go :: String -> [String] -> Maybe Char -> String -> [String]
    go acc toks _ [] =
      if null acc then toks else toks ++ [reverse acc]
    -- Escape handling
    go acc toks inQuote ('\\' : c : rest) =
      let escaped = case c of
            'n' -> '\n'
            't' -> '\t'
            'r' -> '\r'
            _ -> c
       in go (escaped : acc) toks inQuote rest
    -- Double quote handling
    go acc toks Nothing ('"' : rest) =
      go acc toks (Just '"') rest
    go acc toks (Just '"') ('"' : rest) =
      go acc toks Nothing rest
    -- Single quote handling (no escape processing inside)
    go acc toks Nothing ('\'' : rest) =
      go acc toks (Just '\'') rest
    go acc toks (Just '\'') ('\'' : rest) =
      go acc toks Nothing rest
    -- Whitespace outside quotes - token boundary
    go acc toks Nothing (c : rest)
      | isSpace c =
          if null acc
            then go "" toks Nothing rest
            else go "" (toks ++ [reverse acc]) Nothing rest
    -- Regular character
    go acc toks inQuote (c : rest) =
      go (c : acc) toks inQuote rest

renderParseError :: String -> T.Text
renderParseError = T.pack
